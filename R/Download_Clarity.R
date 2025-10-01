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
#' **Required dependencies:**
#' - tidyverse, ggplot2, dplyr: Data manipulation and visualization
#' - httr2: HTTP requests to Clarity API
#' - glue: String interpolation
#' - stats, readr, readxl: Data I/O operations
#' - tools: Utility functions (toTitleCase)
#'
#' @param current_date Date object to determine the previous month for data extraction.
#'   The function calculates the previous month's boundaries from this date.
#' @param root_folder Character string specifying the root folder path for all file operations.
#'   Defaults to the BOX_UPLOAD_ROOT_FOLDER environment variable. Must be a valid directory path.
#' @param clarity_api_key Character string containing the Clarity API key for authentication.
#'   Defaults to the CLARITYAPI environment variable. Required for API access.
#' @param log_file_path Character string specifying the relative path to the processing log file.
#'   Defaults to "CSV/Exports/ClarityLog.csv". Used to track completed processing runs.
#' @param csv_base_path Character string specifying the base path for CSV file storage.
#'   Defaults to "CSV". All sensor and reference data folders are created under this path.
#' @param clarity_folder_name Character string specifying the folder name for sensor data.
#'   Defaults to "Clarity". Creates organized storage for main sensor measurements.
#' @param clarity_reference_folder_name Character string specifying the folder name for reference station data.
#'   Defaults to "Clarity-Reference". Separates reference station data from sensor data.
#' @param sensor_type_filter Character string used to filter monitor information by sensor type.
#'   Defaults to "Clarity". Must match entries in the monitor tracking spreadsheet.
#' @param aggregation_periods Character vector specifying time aggregation periods for data download.
#'   Defaults to c("day", "hour"). Accepts "day" (24-hour averages) and "hour" (hourly data).
#'   Each period generates separate API requests and output files.
#'
#' @return NULL.
#'
#' @section Error handling:
#' The function will stop execution with error messages if:
#' - \code{root_folder} is NULL, empty, or the environment variable is not set
#' - \code{clarity_api_key} is NULL, empty, or the environment variable is not set
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
#' # Custom configuration for different environment
#' save_clarity_to_csv(
#'   current_date = Sys.Date(),
#'   root_folder = "/path/to/data/storage",
#'   clarity_api_key = "your_api_key_here",
#'   aggregation_periods = c("day")  # Only daily data
#' )
#'
#' # Custom folder structure
#' save_clarity_to_csv(
#'   current_date = as.Date("2025-06-01"),
#'   csv_base_path = "data",
#'   clarity_folder_name = "sensors",
#'   clarity_reference_folder_name = "reference_stations"
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
#' @concept removedDependencies:false
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:false
#' @concept addRoxygenComments:true
save_clarity_to_csv <- function(current_date,
                                root_folder = Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"),
                                clarity_api_key = Sys.getenv("CLARITYAPI"),
                                log_file_path = "CSV/Exports/ClarityLog.csv",
                                csv_base_path = "CSV",
                                clarity_folder_name = "Clarity",
                                clarity_reference_folder_name = "Clarity-Reference",
                                sensor_type_filter = "Clarity",
                                aggregation_periods = c("day", "hour")
                        ) {
  # Validate required parameters
  if (is.null(root_folder) || root_folder == "") {
    stop("root_folder parameter is required. Set BOX_UPLOAD_ROOT_FOLDER environment variable or provide explicit path.")
  }
  if (is.null(clarity_api_key) || clarity_api_key == "") {
    stop("clarity_api_key parameter is required. Set CLARITYAPI environment variable or provide explicit key.")
  }

  # Downloading and installation
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, ggplot2, dplyr, httr2, glue, stats, readr, readxl, tools)

  # Get timestamp from start of month
  calc_time <- previous_month_bounds(current_date)
  calc_time_day_only <- previous_month_bounds(current_date, date_only = TRUE)

  start_of_last_month <- calc_time_day_only$start
  start_of_current_month <- calc_time_day_only$end

  start_time_ISO <- calc_time$start
  end_time_ISO <- calc_time$end

  # Check if Log has already been collected
  log_file_full_path <- file.path(root_folder, log_file_path)
  logfile <- read.csv(log_file_full_path) %>% as_tibble()

  if (logfile %>% dplyr::filter(OriginDate == start_of_last_month) %>% nrow > 0) {
    print("Data has already been collected for this month. Function terminating.")
    return()
  }

  # Get DeviceID from CAMNMonitorTracking.xlsx file - synced to Box
  sitesInfo <- read_monitor_info_from_monitor_tracking(sensor_type_filter)

  # Extract information
  deviceId <- sitesInfo[['DeviceID']]
  sensor_owners <- sitesInfo[['Owner']]
  sensor_shortcode <- sitesInfo[['ShortCode']]
  orgID <- sitesInfo[["OrgID"]]

  # Distinct OrgID
  uniqueOrgID <- unique(orgID)

  # Create folder name
  newFolderName <- paste(sensor_type_filter,
                         as.character(start_of_last_month),
                         as.character(start_of_current_month),
                         sep = ".")

  # Create folder paths
  folderPath <- file.path(csv_base_path, clarity_folder_name, newFolderName)
  referenceFolderPath <- file.path(csv_base_path, clarity_reference_folder_name, newFolderName)

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

# Example usage (commented out for package distribution):
# myDate <- Sys.Date()
# myDate <- as.Date("2025-07-02")
# tmp <- save_clarity_to_csv(myDate)

# Development notes: Finish testing: 30 Sep 2025
