#' Download from Clarity API, process, and save to CSV files
#'
#' This function downloads air quality data from the Clarity API for the previous month,
#' processes it into organized datasets, and saves both sensor and reference station data
#' to CSV files. It includes duplicate detection to prevent re-processing of data.
#'
#' @details
#' **Run:**
#' 1. **Validation**: Checks required parameters and environment variables
#' 2. **Log check**: Verifies if data for the target month has already been processed
#' 3. **Time calculation**: Determines the previous month's date range for data extraction
#' 4. **Retrieve monitor info**: Retrieves sensor information from CAMNMonitorTracking.xlsx
#' 5. **API requests**: Downloads data from Clarity API for each organization ID
#' 6. **Data processing**: Splits and formats data by sensor and reference stations
#' 7. **File operations**: Creates folder structure and saves CSV files
#' 8. **Logging**: Records completion status to prevent duplicate processing
#'
#' **Data processing details:**
#' - Retrieves unique organization IDs from monitor tracking data
#' - Downloads data for each aggregation period (daily and hourly by default)
#' - Formats timestamps using \code{format_timestamp()}
#' - Separates sensor data from reference station data
#' - Splits data by individual sensor/datasource IDs
#' - Removes duplicated reference stations by name
#'
#' **File structure:**
#' \preformatted{
#' root_folder/
#' ├── CSV/
#' │   ├── Clarity/
#' │   │   └── Clarity.YYYY-MM-DD.YYYY-MM-DD/
#' │   │       ├── [sensor files with Daily/Hourly data]
#' │   └── Clarity-Reference/
#' │       └── Clarity.YYYY-MM-DD.YYYY-MM-DD/
#' │           ├── [reference station files with Daily/Hourly data]
#' └── CSV/Exports/ClarityLog.csv (processing log)
#' }
#'
#' @param current_date Date object to determine the previous month for data extraction.
#'   The function calculates the previous month's boundaries from this date.
#' @param root_folder Character string specifying the root folder path for all file operations.
#'   Defaults to the UPLOAD_ROOT_FOLDER environment variable. Must be a valid directory path.
#' @param records_folder Character string specifying the folder path where CAMNMonitorTracking.xlsx is located.
#'   Defaults to the RECORDS_ROOT_FOLDER environment variable. Must be a valid directory path
#' @param clarity_api_key Character string containing the Clarity API key for authentication.
#'   Defaults to the CLARITYAPI environment variable. Required for API access.
#' @param aggregation_periods Character vector specifying time aggregation periods for data download.
#'   Defaults to c("day", "hour"). Must contain one or both of: "day" (24-hour averages) and "hour" (hourly data).
#'   Cannot be empty, contain duplicates, or have more than 2 periods. Each period generates separate API requests and output files.
#'
#' @return NULL.
#'
#' @section Error handling:
#' The function will stop execution with error messages if:
#' - \code{root_folder} is NULL, empty, or the environment variable is not set
#' - \code{clarity_api_key} is NULL, empty, or the environment variable is not set
#' - \code{aggregation_periods} is NULL, empty, contains invalid values, duplicates, or more than 2 periods
#' - Log file cannot be read (file doesn't exist or permission issues)
#' - Monitor tracking file is not accessible
#' - API requests fail or return invalid data
#' - Directory creation fails due to permissions
#'
#' @examples
#' \dontrun{
#' # Basic usage with current system date
#' save_clarity_to_csv(Sys.Date())
#'
#' # Process data for a specific month
#' save_clarity_to_csv(as.Date("2025-07-15"))
#'
#' # Custom configuration for different environment - daily data only
#' save_clarity_to_csv(
#'   current_date = Sys.Date(),
#'   root_folder = "/path/to/data/storage",
#'   clarity_api_key = "your_api_key_here",
#'   aggregation_periods = c("day")  # Only daily data
#' )
#'
#' # Download only hourly data
#' save_clarity_to_csv(
#'   current_date = Sys.Date(),
#'   aggregation_periods = c("hour")  # Only hourly data
#' )
#'
#' # Download both daily and hourly (default behavior)
#' save_clarity_to_csv(
#'   current_date = Sys.Date(),
#'   aggregation_periods = c("day", "hour")  # Both periods
#' )
#' }
#'
#' @seealso
#' \code{\link{previous_month_bounds}} for date range calculations,
#' \code{\link{read_monitor_info_from_monitor_tracking}} for sensor metadata,
#' \code{\link{clarity_get_organization_data}} for API data retrieval
#'
#' @export
#' @concept role:download
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
#' @concept addCheckSetupFolder:true
save_clarity_to_csv <- function(current_date,
                                root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER"),
                                records_folder = Sys.getenv("RECORDS_ROOT_FOLDER"),
                                clarity_api_key = Sys.getenv("CLARITYAPI"),
                                aggregation_periods = c("day", "hour")
                        ) {
  # Validate required parameters
  if (is.null(root_folder) || root_folder == "") {
    stop("root_folder parameter is required. Set UPLOAD_ROOT_FOLDER environment variable or provide explicit path.")
  }
  if (is.null(clarity_api_key) || clarity_api_key == "") {
    stop("clarity_api_key parameter is required. Set CLARITYAPI environment variable or provide explicit key.")
  }

  # Check folder structure and Excel file
  if (!check_folder_and_file_structure(root_folder, debug = TRUE)) {
    stop("Required folder structure not found. Please run setup_folder_and_file_structure() first.")
  }
  if (!check_excel_file(records_folder, testing = FALSE)) {
    stop("Required Excel file (CAMNMonitorTracking.xlsx) not found or has incorrect structure. Please run setup_excel_file() first.")
  }

  # Validate aggregation periods
  valid_periods <- c("day", "hour")
  if (is.null(aggregation_periods) || length(aggregation_periods) == 0) {
    stop("aggregation_periods parameter is required and cannot be empty.")
  }
  if (!all(aggregation_periods %in% valid_periods)) {
    invalid_periods <- aggregation_periods[!aggregation_periods %in% valid_periods]
    stop(paste("Invalid aggregation periods:", paste(invalid_periods, collapse = ", "),
               ". Valid options are:", paste(valid_periods, collapse = ", ")))
  }
  if (length(aggregation_periods) > 2) {
    stop("aggregation_periods can contain at most 2 periods: 'day' and/or 'hour'.")
  }
  if (length(unique(aggregation_periods)) != length(aggregation_periods)) {
    stop("aggregation_periods cannot contain duplicate values.")
  }

  # Get timestamp from start of month
  calc_time <- previous_month_bounds(current_date)
  calc_time_day_only <- previous_month_bounds(current_date, date_only = TRUE)

  start_of_last_month <- calc_time_day_only$start
  start_of_current_month <- calc_time_day_only$end

  start_time_ISO <- calc_time$start
  end_time_ISO <- calc_time$end

  # Check if Log has already been collected
  log_file_full_path <- file.path(root_folder, "CSV", "Exports", "ClarityLog.csv")
  logfile <- read.csv(log_file_full_path) %>% dplyr::as_tibble()

  if (logfile %>% dplyr::filter(OriginDate == start_of_last_month) %>% nrow > 0) {
    print("Data has already been collected for this month. Function terminating.")
    return()
  }

  # Get DeviceID from CAMNMonitorTracking.xlsx file - synced to Box
  sitesInfo <- read_monitor_info_from_monitor_tracking("Clarity")

  # Extract information
  deviceId <- sitesInfo[['DeviceID']]
  sensor_owners <- sitesInfo[['Owner']]
  sensor_shortcode <- sitesInfo[['ShortCode']]
  orgID <- sitesInfo[["OrgID"]]

  # Distinct OrgID
  uniqueOrgID <- unique(orgID)

  # Create folder name
  newFolderName <- paste("Clarity",
                         as.character(start_of_last_month),
                         as.character(start_of_current_month),
                         sep = ".")

  # Create folder paths
  folderPath <- file.path("CSV", "Clarity", newFolderName)
  referenceFolderPath <- file.path("CSV", "Clarity-Reference", newFolderName)

  # Create folders
  create_new_folder(folderPath, root_path = root_folder)
  create_new_folder(referenceFolderPath, root_path = root_folder)

  # Process data for each aggregation period
  for (period in aggregation_periods) {
    period_name <- ifelse(period == "day", "Daily",
                         ifelse(period == "hour", "Hourly", tools::toTitleCase(period)))

    # Get measurements by OrgID
    clarity_data <- purrr::map(
      .x = uniqueOrgID,
      .f = function(x, y) clarity_get_organization_data(x, clarity_api_key, period, start_time_ISO, end_time_ISO)
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::rowwise() %>%
      dplyr::mutate(startOfPeriod = format_timestamp(startOfPeriod)) %>%
      dplyr::mutate(endOfPeriod = format_timestamp(endOfPeriod)) %>%
      dplyr::ungroup()

    # Get reference sites
    clarity_reference_data <- split_clarity_reference_data_by_datasource(clarity_data) %>%
      purrr::list_flatten()
    clarity_reference_data <- clarity_reference_data[!duplicated(names(clarity_reference_data))]

    # Split CSV file to distinct datasource IDs
    clarity_data <- split_clarity_data_by_datasource(clarity_data, deviceId) %>%
      purrr::list_flatten()

    # Save sensor data to file
    purrr::pwalk(
      .l = list(deviceId, clarity_data, sensor_owners, sensor_shortcode),
      .f = save_clarity_aq_to_csv,
      average = period_name,
      foldername = file.path(root_folder, folderPath),
      current_date = current_date
    )

    # Save reference data to file
    purrr::walk(
      .x = clarity_reference_data,
      .f = save_clarity_aq_reference_to_csv,
      average = period_name,
      foldername = file.path(root_folder, referenceFolderPath),
      current_date = current_date
    )
  }

  # Write to log
  write.table(
    data.frame(
      OriginDate = c(start_of_last_month),
      Complete = c('COMPLETED')
    ),
    file = log_file_full_path,
    sep = ",",
    col.names = FALSE,
    row.names = FALSE,
    append = TRUE
  )
}

# Development notes: Finish testing: 30 Sep 2025 x 2
# Updates:
# - Parameterized hardcoded variables
# - Add more error handling
# - Add more documentation
# - Eliminated code redundancy by a for-loop for both hourly and daily data
# - Update naming for consistency
# - Update comments
# - Test out goodpractice::gp()
# - Explore ways of cleaning up dependencies namespace
# - Test pkgdown::build_site()

# Update 12/10/2025:
# - addCheckSetupFolder as a flag in roxygen @concept
# - Removed log_file_path, csv_base_path, clarity_folder_name, clarity_reference_folder_name from parameter lists
# - Added folder structure and Excel file validation checks after initial null checks
# - Replaced removed parameter references with hardcoded default values
# - Update typo: MainPersonel.csv to MainPersonnel.csv in setupFolderStructure.R
# - Update QualtricsUpdateLog.csv in setupFolderStructure.R to correct column names.
# - Replace all BOX_UPLOAD_ROOT_FOLDER with UPLOAD_ROOT_FOLDER
# - Replace all BOX_RECORDS_ROOT_FOLDER with RECORDS_ROOT_FOLDER
# - Add records_folder parameter to specify location of CAMNMonitorTracking.xlsx, update documentation

# Update 20/10/2025:
# - Add tibble package
# - Add dplyr package
# - Add stringr package
# - Add glue package
# - Remove calls to lubridate::months in Download_HelperClarity.R
# - Update Helper_File.R starting row to 2 instead of 10/3
# - Update Helper_File.R read_monitor_info_from_monitor_tracking with new column names
# - Add "Owner" field to MonitorStatus
# - Add mailR package
# - Add base64enc package
# - Add mime package
