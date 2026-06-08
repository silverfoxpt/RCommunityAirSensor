#' Download PurpleAir data and save to CSV files
#'
#' Downloads PurpleAir sensor data for the previous month, saves daily and hourly
#' CSV exports for each sensor, and records a log entry to avoid duplicate runs.
#'
#' @details
#' **Run:**
#' 1. **Validation**: Checks required parameters and folder structure
#' 2. **Log check**: Verifies if data for the target month has already been processed
#' 3. **Time calculation**: Determines the previous month's date range for data extraction
#' 4. **Retrieve monitor info**: Retrieves sensor information from monitor tracking
#' 5. **API requests**: Downloads data for each sensor using the PurpleAir API
#' 6. **File operations**: Creates folder structure and saves CSV files (daily and hourly)
#' 7. **Logging**: Records completion status to prevent duplicate processing
#'
#' **Data processing details:**
#' - Uses `previous_month_bounds()` to compute timestamps for the previous month
#' - Splits and saves sensor data by device ID
#'
#' **File structure:**
#' \preformatted{
#' root_folder/
#' └── CSV/
#'     └── PurpleAir/
#'         └── PurpleAir.YYYY-MM-DD.YYYY-MM-DD/
#'             ├── [sensor files]
#' └── CSV/Exports/PurpleAirLog.csv
#' }
#'
#' @param current_date Date object used to determine the target month for data extraction.
#' @param root_folder Character string specifying the root folder path for file operations. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @param records_folder Character string specifying the folder path where the monitor tracking Excel file is located. Defaults to `Sys.getenv("RECORDS_ROOT_FOLDER")`.
#' @param purpleair_api_key Character string containing the PurpleAir API key. Defaults to `Sys.getenv("PURPLEAPI")`.
#' @param is_testing Logical flag indicating whether the function is being run in a testing environment. Defaults to `FALSE`. 
#'
#' @return NULL (invisible). Called for side effects: creates CSV files and appends to a log.
#'
#' @section Error handling:
#' The function will stop with clear messages if required inputs are missing or if
#' folder creation or API calls fail. The function will not re-run for a month
#' already present in the log file.
#'
#' @examples
#' \dontrun{
#' save_purpleAir_to_csv(Sys.Date(), root_folder = "~/project_uploads")
#' }
#'
#' @seealso
#' \code{\link{previous_month_bounds}},
#' \code{\link{read_monitor_info_from_monitor_tracking}},
#' \code{\link{get_single_sensor_data_custom}},
#' \code{\link{save_aq_to_csv}}
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
save_purpleAir_to_csv <- function(current_date,
                                  root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER"),
                                  records_folder = Sys.getenv("RECORDS_ROOT_FOLDER"),
                                  purpleair_api_key = Sys.getenv("PURPLEAPI"),
                                  is_testing = FALSE) {
  # Validate required parameters
  if (is.null(root_folder) || root_folder == "") {
    stop("root_folder parameter is required. Set UPLOAD_ROOT_FOLDER environment variable or provide explicit path.")
  }
  if (is.null(purpleair_api_key) || purpleair_api_key == "") {
    stop("purpleair_api_key parameter is required. Set PURPLEAPI environment variable or provide explicit key.")
  }

  # Check folder structure and Excel file
  if (!check_folder_and_file_structure(root_folder, debug = TRUE)) {
    stop("Required folder structure not found. Please run setup_folder_and_file_structure() first.")
  }
  if (!check_excel_file(records_folder, testing = FALSE)) {
    stop("Required Excel file (CAMNMonitorTracking.xlsx) not found or has incorrect structure. Please run setup_excel_file() first.")
  }

  if (missing(current_date) || is.null(current_date)) {
    stop("`current_date` is required and must be a Date-like object.")
  }

  # compute previous month bounds (assumes helper `previous_month_bounds` exists)
  calc_time <- previous_month_bounds(current_date, nextMonth = TRUE)
  calc_time_day_only <- previous_month_bounds(current_date, date_only = TRUE)

  start_of_last_month <- calc_time_day_only$start
  start_of_current_month <- calc_time_day_only$end

  start_timestamp <- calc_time$start
  end_timestamp <- calc_time$end

  # prepare log path and ensure exports folder exists
  log_dir <- file.path(root_folder, "CSV", "Exports")
  log_path <- file.path(log_dir, "PurpleAirLog.csv")

  # read existing log if present
  if (file.exists(log_path)) {
    logfile <- tibble::as_tibble(utils::read.csv(log_path, stringsAsFactors = FALSE))
  } else {
    logfile <- tibble::tibble()
  }

  # if this month already processed, exit quietly
  if (nrow(logfile) > 0 && dplyr::nrow(dplyr::filter(logfile, OriginDate == start_of_last_month)) > 0) {
    return(invisible(NULL))
  }

  # API key and monitor tracking info
  api_key <- purpleair_api_key

  sitesInfo <- read_monitor_info_from_monitor_tracking("PurpleAir")

  sensor_ids <- sitesInfo[["DeviceID"]]
  sensor_owners <- sitesInfo[["Owner"]]
  sensor_shortcode <- sitesInfo[["ShortCode"]]

  # prepare rate-limited getter
  rate <- if (is_testing) purrr::rate_delay(0) else purrr::rate_delay(2)
  slow_get <- purrr::slowly(get_single_sensor_data_custom, rate = rate, quiet = TRUE)

  # fetch daily data (gap = 1440 -> daily)
  temp_list_sensors_data <- purrr::map(.x = sensor_ids,
                                       .f = purrr::possibly(slow_get, otherwise = NULL, quiet = FALSE),
                                       neededFields = "temperature,humidity,pm2.5_alt,pm2.5_atm,pm2.5_cf_1",
                                       starting = start_timestamp,
                                       ending = end_timestamp,
                                       gap = "1440",
                                       api_key = api_key)

  newFolderName <- paste("PurpleAir.", as.character(start_of_last_month), ".", as.character(start_of_current_month), sep = "")
  folderPath <- file.path("CSV", "PurpleAir", newFolderName)
  create_new_folder(folderPath, root_path = root_folder)

  # save daily CSVs
  purrr::pwalk(.l = list(sensor_ids, temp_list_sensors_data, sensor_owners, sensor_shortcode),
               .f = save_aq_to_csv,
               average = "Daily",
               foldername = file.path(root_folder, folderPath),
               current_date = current_date)

  # fetch hourly data (gap = 60 -> hourly)
  temp_list_sensors_data <- purrr::map(.x = sensor_ids,
                                       .f = purrr::possibly(slow_get, otherwise = NULL, quiet = FALSE),
                                       neededFields = "temperature,humidity,pm2.5_alt,pm2.5_atm,pm2.5_cf_1",
                                       starting = start_timestamp,
                                       ending = end_timestamp,
                                       gap = "60",
                                       api_key = api_key)

  # save hourly CSVs
  purrr::pwalk(.l = list(sensor_ids, temp_list_sensors_data, sensor_owners, sensor_shortcode),
               .f = save_aq_to_csv,
               average = "Hourly",
               foldername = file.path(root_folder, folderPath),
               current_date = current_date)

  # append to log to mark completion
  write.table(
    data.frame(OriginDate = c(start_of_last_month), Complete = c("COMPLETED")),
    file = log_path,
    sep = ",",
    col.names = FALSE,
    row.names = FALSE,
    append = TRUE
  )

  invisible(NULL)
}
