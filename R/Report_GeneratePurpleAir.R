#' Generate PurpleAir instant report
#'
#' Creates the monthly PurpleAir sensor health report, including current status,
#' recent hourly data checks, archive validation, and completeness checks, then
#' renders the result to a PDF report.
#'
#' @details
#' **Run:**
#' 1. Validates the output root folder, records root folder, API key, and report template.
#' 2. Determines the previous month reporting window and output location.
#' 3. Reads PurpleAir monitor metadata from the tracking workbook.
#' 4. Retrieves current status and recent hourly measurements from the PurpleAir API.
#' 5. Evaluates sensor malfunction indicators, archive coverage, and completeness.
#' 6. Renders the report template to a PDF file.
#'
#' **Data processing details:**
#' - Uses the tracked sensor IDs to request status and hourly PurpleAir data.
#' - Normalizes timestamps and quality checks before report rendering.
#' - Splits recent hourly data by sensor ID for per-sensor QA/QC summaries.
#' - Joins monitor short codes into each summary table for display in the report.
#'
#' **File structure:**
#' \preformatted{
#' [UPLOAD_ROOT_FOLDER]/CSV/Instant-Report/YYYY-MM/
#'   Status of Purple Air sensors - YYYY-MM-DD.pdf
#' }
#'
#' @param current_date Date or POSIXt value used to determine the previous month reporting window.
#' @param upload_root_folder Character string specifying the root folder for report output. Defaults to the `UPLOAD_ROOT_FOLDER` environment variable.
#' @param records_root_folder Character string specifying the root folder containing `CAMNMonitorTracking.xlsx`. Defaults to the `RECORDS_ROOT_FOLDER` environment variable.
#' @param api_key Character string containing the PurpleAir API key. Defaults to the `PURPLEAPI` environment variable.
#' @param template_path Character string specifying the R Markdown template used to render the report.
#' @param output_file Character string specifying the final PDF path. Defaults to a dated file under `upload_root_folder`.
#' @param debug_turn_on Logical indicating whether report debugging output should be enabled in the template. Defaults to `TRUE`.
#' @param online_threshold_seconds Numeric threshold in seconds for marking a device as online. Defaults to 600.
#' @param rate_delay_seconds Numeric delay in seconds used for the API rate limiter. Defaults to 2.
#' @param request_pause_seconds Numeric pause in seconds inserted after the status request. Defaults to 2.
#'
#' @return NULL. The function is called for its side effects.
#'
#' @section Error handling:
#' Stops with a clear message if folders, credentials, the Excel tracking file, or the report template are missing or invalid.
#'
#' @examples
#' \dontrun{
#' generate_purple_air_instant_report()
#' }
#'
#' @seealso
#' \code{\link{read_monitor_info_from_monitor_tracking}},
#' \code{\link{test_sensor_malfunction_result_report_purpleAir}},
#' \code{\link{test_full_headers_report_purpleAir}},
#' \code{\link{check_missing_data_report_purpleAir}}
#'
#' @export
#' @concept role:report
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
#' @concept addCheckSetupFolder:true
generate_purple_air_instant_report <- function(
  current_date = lubridate::now(tzone = "UTC"),
  upload_root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER"),
  records_root_folder = Sys.getenv("RECORDS_ROOT_FOLDER"),
  api_key = Sys.getenv("PURPLEAPI"),
  template_path = file.path("data-raw", "Report_PurpleAirTemplate.Rmd"),
  output_file = NULL,
  debug_turn_on = TRUE,
  online_threshold_seconds = 600,
  rate_delay_seconds = 2,
  request_pause_seconds = 2
) {
  if (is.null(upload_root_folder) || !nzchar(upload_root_folder)) {
    stop("upload_root_folder must be provided or set via UPLOAD_ROOT_FOLDER.")
  }
  if (is.null(records_root_folder) || !nzchar(records_root_folder)) {
    stop("records_root_folder must be provided or set via RECORDS_ROOT_FOLDER.")
  }
  if (is.null(api_key) || !nzchar(api_key)) {
    stop("api_key must be provided or set via PURPLEAPI.")
  }
  if (!inherits(current_date, c("Date", "POSIXct", "POSIXt"))) {
    stop("current_date must be a Date or POSIXt object.")
  }
  if (!file.exists(template_path)) {
    stop("template_path does not exist: ", template_path)
  }
  if (!check_folder_and_file_structure(upload_root_folder)) {
    stop("Required folder structure not found under upload_root_folder. Run setup_folder_and_file_structure() first.")
  }
  if (!check_excel_file(records_root_folder, testing = FALSE)) {
    stop("CAMNMonitorTracking.xlsx is missing or has an invalid structure under records_root_folder.")
  }

  old_records_root <- Sys.getenv("RECORDS_ROOT_FOLDER", unset = NA_character_)
  on.exit(
    {
      if (is.na(old_records_root)) {
        Sys.unsetenv("RECORDS_ROOT_FOLDER")
      } else {
        Sys.setenv(RECORDS_ROOT_FOLDER = old_records_root)
      }
    },
    add = TRUE
  )
  Sys.setenv(RECORDS_ROOT_FOLDER = records_root_folder)

  report_date <- lubridate::as_date(current_date)
  current_time_utc <- current_date
  current_time_unix <- as.numeric(current_time_utc)
  last_month <- current_time_utc - months(1)

  start_time_UNIX <- lubridate::floor_date(last_month, unit = "month") %>%
    as.numeric()
  end_time_UNIX <- (lubridate::ceiling_date(last_month, unit = "month") - lubridate::seconds(1)) %>%
    as.numeric()

  calc_time <- previous_month_bounds(report_date, iso8601 = FALSE, date_only = TRUE)
  start_of_last_month <- calc_time$start
  end_of_last_month <- calc_time$end

  calc_time_pa <- previous_month_bounds(report_date, nextMonth = TRUE)
  start_timestamp <- calc_time_pa$start
  end_timestamp <- calc_time_pa$end

  sites_info <- read_monitor_info_from_monitor_tracking("PurpleAir")
  sensor_ids <- sites_info[["DeviceID"]]
  sensor_owners <- sites_info[["Owner"]]
  sensor_shortcode <- sites_info[["ShortCode"]]

  status_data <- get_multi_sensors_status(
    paste(sensor_ids, collapse = ","),
    "last_seen,rssi,pm2.5,confidence",
    api_key
  )
  Sys.sleep(request_pause_seconds)

  status_data <- dplyr::mutate(
    status_data,
    `Data Collected` = dplyr::if_else(is.na(.data[["pm2.5"]]), "No", "Yes"),
    `Wi-Fi Bars` = dplyr::case_when(
      is.na(rssi) ~ 0,
      rssi > -50 ~ 5,
      rssi > -60 ~ 4,
      rssi > -67 ~ 3,
      rssi > -80 ~ 2,
      TRUE ~ 1
    ),
    `Device Online` = dplyr::if_else(current_time_unix - last_seen <= online_threshold_seconds, "Yes", "No"),
    sensor_index = as.character(sensor_index)
  )

  rate <- purrr::rate_delay(rate_delay_seconds)
  slow_get <- purrr::slowly(get_single_sensor_data_custom, rate = rate, quiet = FALSE)

  temp_list_sensors_data <- purrr::map(
    .x = sensor_ids,
    .f = purrr::possibly(slow_get, otherwise = NULL, quiet = FALSE),
    neededFields = "temperature,humidity,pm2.5_alt,pm2.5_atm,pm2.5_cf_1",
    starting = start_timestamp,
    ending = end_timestamp,
    gap = "60",
    api_key = api_key
  )
  temp_list_sensors_data <- setNames(temp_list_sensors_data, sensor_ids)

  summarize_issues <- function(result, month_hours) {
    if (!is.null(result$error) && !is.na(result$error)) {
      return("No data")
    }

    issues <- character()

    if (nrow(result$spikes) > 0) {
      issues <- c(issues, paste("Outliers:", nrow(result$spikes)))
    }
    if (nrow(result$prolonged_sequences) > 0) {
      issues <- c(issues, "Abnormal data")
    }
    if (isTRUE(result$trends_flag)) {
      issues <- c(issues, result$trends_type)
    }
    if (nrow(result$missing_timestamps) > 0) {
      issues <- c(
        issues,
        paste0(
          "Timestamps missing: ",
          formatC((nrow(result$missing_timestamps) / month_hours) * 100, digits = 2, format = "f"),
          "%"
        )
      )
    }
    if (nrow(result$duplicate_timestamps) > 0) {
      issues <- c(issues, paste("Timestamps duplicates:", nrow(result$duplicate_timestamps)))
    }

    if (length(issues) > 0) stringr::str_c(issues, collapse = "\n") else "No problem"
  }

  month_hours <- get_last_month_hours(date_str = report_date)
  graph_test <- tibble::tibble(
    sensor_index = sensor_ids,
    `PM2.5 Graph` = NA_character_,
    `Temperature Graph` = NA_character_,
    `Humidity Graph` = NA_character_
  )

  graph_test <- purrr::reduce2(
    .x = temp_list_sensors_data,
    .y = names(temp_list_sensors_data),
    .init = graph_test,
    .f = function(acc, x, y) {
      acc <- dplyr::mutate(acc, sensor_index = as.character(sensor_index))
      y <- as.character(y)

      results <- list(
        "PM2.5 Graph" = test_sensor_malfunction_result_report_purpleAir(x, "time_stamp", "pm2.5_atm", start_time_UNIX, end_time_UNIX),
        "Temperature Graph" = test_sensor_malfunction_result_report_purpleAir(x, "time_stamp", "temperature", start_time_UNIX, end_time_UNIX),
        "Humidity Graph" = test_sensor_malfunction_result_report_purpleAir(x, "time_stamp", "humidity", start_time_UNIX, end_time_UNIX)
      )

      if (!y %in% acc$sensor_index) {
        acc <- dplyr::bind_rows(acc, tibble::tibble(sensor_index = y))
      }

      dplyr::mutate(
        acc,
        dplyr::across(
          names(results),
          ~ ifelse(
            sensor_index == y,
            summarize_issues(results[[cur_column()]], month_hours),
            .x
          )
        )
      )
    }
  )

  archived <- load_purple_air_data_from_archive(start_of_last_month)
  fields <- c("temperature", "humidity", "pm2.5_alt", "pm2.5_atm", "pm2.5_cf_1")

  archive_test <- tibble::tibble(sensor_index = sensor_ids) %>%
    dplyr::mutate(
      `Daily Label` = purrr::map_chr(sensor_index, ~ {
        if (any(stringr::str_detect(as.character(archived$DailyLabel), .x))) {
          "Exist"
        } else {
          "Non-exist"
        }
      }),
      `Hourly Label` = purrr::map_chr(sensor_index, ~ {
        if (any(stringr::str_detect(as.character(archived$HourlyLabel), .x))) {
          "Exist"
        } else {
          "Non-exist"
        }
      }),
      `Daily Headers` = purrr::map_chr(sensor_index, ~ {
        if (is.null(archived$Daily) || is.null(archived$Daily[[.x]])) {
          "Missing"
        } else if (test_full_headers_report_purpleAir(archived$Daily[[.x]], fields)) {
          "Normal"
        } else {
          "Missing"
        }
      }),
      `Hourly Headers` = purrr::map_chr(sensor_index, ~ {
        if (is.null(archived$Hourly) || is.null(archived$Hourly[[.x]])) {
          "Missing"
        } else if (test_full_headers_report_purpleAir(archived$Hourly[[.x]], fields)) {
          "Normal"
        } else {
          "Missing"
        }
      })
    )

  count_this_month <- get_last_month_hours(date_str = report_date)
  data_to_check_completeness <- archived$Hourly
  complete_test <- tibble::tibble(sensor_index = sensor_ids) %>%
    dplyr::mutate(
      `Timestamp` = purrr::map_chr(sensor_index, ~ check_missing_data_report_purpleAir(.x, "time_stamp", data_to_check_completeness, count_this_month)),
      `PM25 Raw` = purrr::map_chr(sensor_index, ~ check_missing_data_report_purpleAir(.x, "pm2.5_atm", data_to_check_completeness, count_this_month)),
      `PM25 Value` = purrr::map_chr(sensor_index, ~ check_missing_data_report_purpleAir(.x, "pm2.5_alt", data_to_check_completeness, count_this_month)),
      `Temperature` = purrr::map_chr(sensor_index, ~ check_missing_data_report_purpleAir(.x, "temperature", data_to_check_completeness, count_this_month)),
      `Humidity` = purrr::map_chr(sensor_index, ~ check_missing_data_report_purpleAir(.x, "humidity", data_to_check_completeness, count_this_month))
    )

  monitor_info_purple_air <- read_monitor_info_from_monitor_tracking("PurpleAir") %>%
    dplyr::select(DeviceID, ShortCode) %>%
    dplyr::rename(sensor_index = DeviceID)

  processed_report <- list(
    ArchivedData = archived,
    RecentSensorData = temp_list_sensors_data,
    StatusTest = dplyr::full_join(status_data, monitor_info_purple_air, by = dplyr::join_by(sensor_index)),
    GraphTest = dplyr::full_join(graph_test, monitor_info_purple_air, by = dplyr::join_by(sensor_index)),
    ArchiveTest = dplyr::full_join(archive_test, monitor_info_purple_air, by = dplyr::join_by(sensor_index)),
    CompleteTest = dplyr::full_join(complete_test, monitor_info_purple_air, by = dplyr::join_by(sensor_index))
  )

  report_folder <- file.path("CSV", "Instant-Report", format(report_date, "%Y-%m"))
  create_new_folder(report_folder, root_path = upload_root_folder)

  report_title <- paste("Status of Purple Air sensors - ", format(report_date, "%Y-%m-%d"), sep = "")
  if (is.null(output_file)) {
    output_file <- file.path(upload_root_folder, report_folder, paste0(report_title, ".pdf"))
  } else {
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  }

  rmarkdown::render(
    template_path,
    params = list(
      title = report_title,
      myData = processed_report,
      debugTurnOn = debug_turn_on
    ),
    output_file = output_file
  )

  invisible(NULL)
}
