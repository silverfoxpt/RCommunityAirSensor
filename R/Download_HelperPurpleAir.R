#' Download PurpleAir history for one sensor
#'
#' Downloads history data for a single PurpleAir sensor and returns a tidy tibble.
#'
#' @details
#' Builds the PurpleAir history request for the supplied sensor and time window,
#' replaces missing values with `NA`, formats timestamps, and adds a sensor index column.
#'
#' @param sensor_id Character or numeric sensor identifier.
#' @param neededFields Character string of comma-separated PurpleAir field names.
#' @param starting Integer start timestamp used by the PurpleAir API.
#' @param ending Integer end timestamp used by the PurpleAir API.
#' @param gap Character or numeric averaging interval passed to the PurpleAir API.
#' @param api_key Character string containing the PurpleAir API key.
#' @param api_base_url Character string containing the PurpleAir API base URL.
#'
#' @return A tibble of sensor history data.
#'
#' @section Error handling:
#' Stops with a clear message when the API returns no history rows for the requested sensor.
#'
#' @examples
#' \dontrun{
#' get_single_sensor_data_custom(
#'   sensor_id = 12345,
#'   neededFields = "temperature,humidity,pm2.5_atm",
#'   starting = 1717200000,
#'   ending = 1717286400,
#'   gap = "60",
#'   api_key = Sys.getenv("PURPLEAPI")
#' )
#' }
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
get_single_sensor_data_custom <- function(sensor_id,
                                          neededFields,
                                          starting,
                                          ending,
                                          gap,
                                          api_key,
                                          api_base_url = "https://api.purpleair.com/v1") {
  message("Trying to retrieve history data for sensor ID: ", sensor_id)
  history_url <- glue::glue("{api_base_url}/sensors/{sensor_id}/history")

  req <- httr2::request(history_url)
  req <- httr2::req_headers(req, "X-API-Key" = api_key)
  req <- httr2::req_url_query(
    req,
    fields = neededFields,
    start_timestamp = starting,
    end_timestamp = ending,
    average = gap
  )
  req <- httr2::req_method(req, "GET")

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)

  if (is.null(resp_body$data) || length(resp_body$data) == 0) {
    stop(paste("Error - history data retrieve empty: Sensor ID ", as.character(sensor_id), sep = ""))
  }

  sensor_fields <- as.vector(resp_body$fields)

  sensor_data <- purrr::map(
    .x = resp_body$data,
    .f = function(x) {
      for (i in seq_along(x)) {
        if (is.null(x[[i]])) {
          x[[i]] <- NA
        }
      }

      names(x) <- sensor_fields
      x
    }
  )

  sensor_data <- dplyr::bind_rows(sensor_data)
  sensor_data <- tibble::as_tibble(sensor_data)
  sensor_data <- setNames(sensor_data, sensor_fields)
  sensor_data <- sensor_data[order(sensor_data$time_stamp, decreasing = TRUE), , drop = FALSE]
  sensor_data$time_stamp <- format_timestamp(sensor_data$time_stamp)
  sensor_data$sensor_index <- sensor_id
  message("Successfully retrieved history data for sensor ID: ", sensor_id)
  message("Waiting for 2 seconds to respect API rate limits...")

  sensor_data
}

#' Download PurpleAir status for multiple sensors
#'
#' Downloads sensor status data for a set of PurpleAir sensors and returns a tidy tibble.
#'
#' @details
#' Builds the PurpleAir sensor list request, replaces missing values with `NA`, and
#' returns the requested fields for the supplied sensor IDs.
#'
#' @param sensor_ids Character or numeric sensor identifiers.
#' @param neededFields Character string of comma-separated PurpleAir field names.
#' @param api_key Character string containing the PurpleAir API key.
#' @param api_base_url Character string containing the PurpleAir API base URL.
#'
#' @return A tibble of sensor status data.
#'
#' @section Error handling:
#' Stops with a clear message when the API returns no status rows for the requested sensors.
#'
#' @examples
#' \dontrun{
#' get_multi_sensors_status(
#'   sensor_ids = c(12345, 67890),
#'   neededFields = "temperature,humidity",
#'   api_key = Sys.getenv("PURPLEAPI")
#' )
#' }
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
get_multi_sensors_status <- function(sensor_ids,
                                     neededFields,
                                     api_key,
                                     api_base_url = "https://api.purpleair.com/v1") {
  status_url <- glue::glue("{api_base_url}/sensors")

  req <- httr2::request(status_url)
  req <- httr2::req_headers(req, "X-API-Key" = api_key)
  req <- httr2::req_url_query(
    req,
    fields = neededFields,
    show_only = sensor_ids
  )
  req <- httr2::req_method(req, "GET")

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)

  if (is.null(resp_body$data) || length(resp_body$data) == 0) {
    stop(paste("Error - history data retrieve empty: Sensor IDs ", paste(sensor_ids, collapse = ", "), sep = ""))
  }

  sensor_fields <- as.vector(resp_body$fields)

  sensor_data <- purrr::map(
    .x = resp_body$data,
    .f = function(x) {
      for (i in seq_along(x)) {
        if (is.null(x[[i]])) {
          x[[i]] <- NA
        }
      }

      names(x) <- sensor_fields
      x
    }
  )

  sensor_data <- dplyr::bind_rows(sensor_data)
  sensor_data <- tibble::as_tibble(sensor_data)
  setNames(sensor_data, sensor_fields)
}

#' Save PurpleAir sensor data to CSV
#'
#' Saves a PurpleAir sensor data frame to a CSV file using the package naming convention.
#'
#' @details
#' Uses the previous month boundaries derived from `current_date` to build the output filename.
#' When `tb` is `NULL`, the function writes a small error file instead of the data table.
#'
#' @param sensorId Character or numeric sensor identifier.
#' @param tb Data frame or tibble to save.
#' @param owner Character string for the sensor owner name.
#' @param shortcode Character string for the sensor short code.
#' @param average Character string describing the averaging interval.
#' @param foldername Character string specifying the destination folder path.
#' @param current_date Date object used to derive the filename date range.
#' @param file_prefix Character string used at the start of the exported filename.
#' @param file_suffix Character string used before the `.csv` extension.
#'
#' @return NULL. Called for side effects.
#'
#' @section Error handling:
#' Writes a small CSV containing an error message when `tb` is `NULL`.
#'
#' @examples
#' \dontrun{
#' save_aq_to_csv(
#'   sensorId = 12345,
#'   tb = data.frame(time_stamp = 1),
#'   owner = "Owner",
#'   shortcode = "ABC",
#'   average = "Daily",
#'   foldername = "CSV/PurpleAir",
#'   current_date = Sys.Date()
#' )
#' }
#'
#' @export
#' @concept role:export
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
save_aq_to_csv <- function(sensorId,
                           tb,
                           owner,
                           shortcode,
                           average,
                           foldername,
                           current_date,
                           file_prefix = "PA",
                           file_suffix = "_PM25-atm-alt-T-H") {
  if (is.null(foldername) || foldername == "") {
    stop("foldername parameter is required.")
  }
  if (is.null(current_date)) {
    stop("current_date parameter is required.")
  }

  start_of_current_month <- lubridate::floor_date(current_date, unit = "month")
  start_of_last_month <- lubridate::floor_date(current_date - months(1), unit = "month")

  start_last <- format(start_of_last_month, "%Y%m%d")
  start_current <- format(start_of_current_month - lubridate::days(1), "%Y%m%d")

  filename <- paste(
    start_last,
    "-",
    start_current,
    "_",
    file_prefix,
    "_",
    sensorId,
    "-",
    owner,
    "_",
    shortcode,
    "_",
    average,
    file_suffix,
    ".csv",
    sep = ""
  )

  if (is.null(tb)) {
    errorMessage <- c("Error: Empty data! Please recheck!")
    write.csv(errorMessage, file = file.path(foldername, filename), row.names = FALSE)
    return()
  }

  write.csv(tb, file = file.path(foldername, filename), row.names = FALSE)
}
