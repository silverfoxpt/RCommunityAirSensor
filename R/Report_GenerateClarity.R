#' Generate Clarity instant report
#'
#' Creates the monthly Clarity sensor health report, including current status,
#' recent hourly data checks, archive validation, and completeness checks, then
#' renders the result to a PDF report.
#'
#' @details
#' **Run:**
#' 1. Validates the output root folder, records root folder, API key, and report template.
#' 2. Determines the previous month reporting window and output location.
#' 3. Reads Clarity monitor metadata from the tracking workbook.
#' 4. Retrieves current status and recent hourly measurements from the Clarity API.
#' 5. Evaluates sensor malfunction indicators, archive coverage, and completeness.
#' 6. Renders the report template to a PDF file.
#'
#' **Data processing details:**
#' - Uses unique organization IDs from monitor tracking data to query the Clarity API.
#' - Normalizes timestamps before report rendering.
#' - Splits recent hourly data by datasource ID for per-sensor quality checks.
#' - Joins monitor short codes into each summary table for display in the report.
#'
#' **File structure:**
#' \preformatted{
#' [UPLOAD_ROOT_FOLDER]/CSV/Instant-Report/YYYY-MM/
#'   Status of Clarity sensors - YYYY-MM-DD.pdf
#' }
#'
#' @param current_date Date or POSIXt value used to determine the previous month reporting window.
#' @param upload_root_folder Character string specifying the root folder for report output. Defaults to the `UPLOAD_ROOT_FOLDER` environment variable.
#' @param records_root_folder Character string specifying the root folder containing `CAMNMonitorTracking.xlsx`. Defaults to the `RECORDS_ROOT_FOLDER` environment variable.
#' @param clarity_api_key Character string containing the Clarity API key. Defaults to the `CLARITYAPI` environment variable.
#' @param template_path Character string specifying the R Markdown template used to render the report.
#' @param output_file Character string specifying the final PDF path. Defaults to a dated file under `upload_root_folder`.
#' @param debug_turn_on Logical indicating whether report debugging output should be enabled in the template. Defaults to `TRUE`.
#' @param status_threshold_seconds Numeric threshold in seconds for marking a device as online. Defaults to 600.
#' @param request_pause_seconds Numeric pause in seconds inserted after the status request. Defaults to 2.
#'
#' @return NULL. The function is called for its side effects.
#'
#' @section Error handling:
#' Stops with a clear message if folders, credentials, the Excel tracking file, or the report template are missing or invalid.
#'
#' @examples
#' \dontrun{
#' generate_clarity_instant_report()
#' }
#'
#' @seealso
#' \code{\link{read_monitor_info_from_monitor_tracking}},
#' \code{\link{test_sensor_malfunction_result_report_clarity}},
#' \code{\link{test_full_headers_report_clarity}},
#' \code{\link{check_missing_data_report_clarity}}
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
generate_clarity_instant_report <- function(
  current_date = lubridate::now(tzone = "UTC"),
  upload_root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER"),
  records_root_folder = Sys.getenv("RECORDS_ROOT_FOLDER"),
  clarity_api_key = Sys.getenv("CLARITYAPI"),
  template_path = file.path("data-raw", "Report_ClarityTemplate.Rmd"),
  output_file = NULL,
  debug_turn_on = TRUE,
  status_threshold_seconds = 600,
  request_pause_seconds = 2
) {
  if (is.null(upload_root_folder) || !nzchar(upload_root_folder)) {
    stop("upload_root_folder must be provided or set via UPLOAD_ROOT_FOLDER.")
  }
  if (is.null(records_root_folder) || !nzchar(records_root_folder)) {
    stop("records_root_folder must be provided or set via RECORDS_ROOT_FOLDER.")
  }
  if (is.null(clarity_api_key) || !nzchar(clarity_api_key)) {
    stop("clarity_api_key must be provided or set via CLARITYAPI.")
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
  current_time_utc <- lubridate::now(tzone = "UTC")

  calc_time <- previous_month_bounds(report_date)
  calc_time_day_only <- previous_month_bounds(report_date, date_only = TRUE)

  start_of_last_month <- calc_time_day_only$start
  start_of_current_month <- calc_time_day_only$end
  start_time_ISO <- calc_time$start
  end_time_ISO <- calc_time$end

  sites_info <- read_monitor_info_from_monitor_tracking("Clarity")

  device_id <- sites_info[["DeviceID"]]
  node_id <- sites_info[["Hardware ID"]]
  dict_for_device_id <- as.list(device_id)
  names(dict_for_device_id) <- node_id

  org_id <- sites_info[["OrgID"]]
  unique_org_id <- unique(org_id)

  clarity_status_data <- purrr::map(unique_org_id, ~ clarity_get_organization_status(.x, clarity_api_key))

  clarity_status_data_clean <- unlist(clarity_status_data, recursive = FALSE) %>%
    purrr::map(
      .f = \(x) {
        x = unlist(x, recursive = FALSE) %>% unlist(recursive = FALSE)
      }
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::rename(
      Power = `summaryPerCategory1.summary`,
      Communication = `summaryPerCategory2.summary`,
      `Internal Sensors` = `summaryPerCategory3.summary`,
      `Accessory Modules` = `summaryPerCategory4.summary`,
      `Activity` = `summaryPerCategory5.summary`
    ) %>%
    dplyr::mutate(
      `Device Online` = dplyr::if_else(
        as.numeric(difftime(current_time_utc, convert_to_time(lastReadingReceivedAt, original_format = "%Y-%m-%dT%H:%M:%OSZ"), units = "secs")) <= status_threshold_seconds,
        "Yes",
        "No"
      )
    ) %>%
    dplyr::mutate(DeviceID = sapply(nodeId, function(id) {
        result <- dict_for_device_id[[id]]
        if (is.null(result)) {
          paste0(id, ", DeviceID not fetchable")
        } else {
          result
        }
      })
    ) %>%
    dplyr::ungroup()

  Sys.sleep(request_pause_seconds)

  clarity_last_month <- purrr::map(
    unique_org_id,
    ~ clarity_get_organization_data(.x, clarity_api_key, "hour", start_time_ISO, end_time_ISO)
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      startOfPeriod = format_timestamp(startOfPeriod),
      endOfPeriod = format_timestamp(endOfPeriod)
    )

  clarity_last_month_data_clean <- split_clarity_data_by_datasource(clarity_last_month, device_id) %>%
    purrr::list_flatten() %>%
    setNames(device_id)

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
    sensor_index = device_id,
    `PM2.5 Graph` = NA_character_,
    `NO2 Graph` = NA_character_,
    `Temperature Graph` = NA_character_,
    `Humidity Graph` = NA_character_
  )

  graph_test <- purrr::reduce2(
    .x = clarity_last_month_data_clean,
    .y = names(clarity_last_month_data_clean),
    .init = graph_test,
    .f = function(acc, x, y) {
      acc <- dplyr::mutate(acc, sensor_index = as.character(sensor_index))
      y <- as.character(y)

      results <- list(
        "PM2.5 Graph" = test_sensor_malfunction_result_report_clarity(x, "startOfPeriod", "pm2_5ConcMass1HourMean.value", start_time_ISO, end_time_ISO),
        "NO2 Graph" = test_sensor_malfunction_result_report_clarity(x, "startOfPeriod", "no2Conc1HourMean.value", start_time_ISO, end_time_ISO),
        "Temperature Graph" = test_sensor_malfunction_result_report_clarity(x, "startOfPeriod", "temperatureInternal1HourMean.raw", start_time_ISO, end_time_ISO),
        "Humidity Graph" = test_sensor_malfunction_result_report_clarity(x, "startOfPeriod", "relHumidInternal1HourMean.raw", start_time_ISO, end_time_ISO)
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

  archived_clarity <- load_clarity_data_from_archive(start_of_last_month)
  daily_fields <- c(
    "datasourceId", "sourceId", "sourceType", "outputFrequency", "startOfPeriod",
    "endOfPeriod", "locationLatitude", "locationLongitude",
    "pm2_5ConcMass24HourMean.value", "no2Conc24HourMean.value",
    "temperatureInternal24HourMean.raw", "relHumidInternal24HourMean.raw"
  )
  hourly_fields <- c(
    "datasourceId", "sourceId", "sourceType", "outputFrequency", "startOfPeriod",
    "endOfPeriod", "locationLatitude", "locationLongitude",
    "pm2_5ConcMass1HourMean.value", "no2Conc1HourMean.value",
    "temperatureInternal1HourMean.raw", "relHumidInternal1HourMean.raw"
  )

  archive_test <- tibble::tibble(DeviceID = device_id) %>%
    dplyr::mutate(
      `Daily Label` = purrr::map_chr(DeviceID, ~ {
        if (any(stringr::str_detect(as.character(archived_clarity$DailyLabel), .x))) {
          "Exist"
        } else {
          "Non-exist"
        }
      }),
      `Hourly Label` = purrr::map_chr(DeviceID, ~ {
        if (any(stringr::str_detect(as.character(archived_clarity$HourlyLabel), .x))) {
          "Exist"
        } else {
          "Non-exist"
        }
      }),
      `Daily Headers` = purrr::map_chr(DeviceID, ~ {
        if (is.null(archived_clarity$Daily) || is.null(archived_clarity$Daily[[.x]])) {
          "Missing"
        } else if (test_full_headers_report_clarity(archived_clarity$Daily[[.x]], daily_fields)) {
          "Normal"
        } else {
          "Missing"
        }
      }),
      `Hourly Headers` = purrr::map_chr(DeviceID, ~ {
        if (is.null(archived_clarity$Hourly) || is.null(archived_clarity$Hourly[[.x]])) {
          "Missing"
        } else if (test_full_headers_report_clarity(archived_clarity$Hourly[[.x]], hourly_fields)) {
          "Normal"
        } else {
          "Missing"
        }
      })
    )

  count_this_month <- get_last_month_hours(date_str = report_date)
  data_to_check_completeness <- archived_clarity$Hourly
  complete_test <- tibble::tibble(sensor_index = device_id) %>%
    dplyr::mutate(
      `Time` = purrr::map_chr(sensor_index, ~ check_missing_data_report_clarity(.x, "startOfPeriod", data_to_check_completeness, count_this_month)),
      `PM25 Raw` = purrr::map_chr(sensor_index, ~ check_missing_data_report_clarity(.x, "pm2_5ConcMass1HourMean.raw", data_to_check_completeness, count_this_month)),
      `PM25 Value` = purrr::map_chr(sensor_index, ~ check_missing_data_report_clarity(.x, "pm2_5ConcMass1HourMean.value", data_to_check_completeness, count_this_month)),
      `NO2 Raw` = purrr::map_chr(sensor_index, ~ check_missing_data_report_clarity(.x, "no2Conc1HourMean.raw", data_to_check_completeness, count_this_month)),
      `NO2 Value` = purrr::map_chr(sensor_index, ~ check_missing_data_report_clarity(.x, "no2Conc1HourMean.value", data_to_check_completeness, count_this_month)),
      `Temp Raw` = purrr::map_chr(sensor_index, ~ check_missing_data_report_clarity(.x, "temperatureInternal1HourMean.raw", data_to_check_completeness, count_this_month)),
      `Temp Value` = purrr::map_chr(sensor_index, ~ check_missing_data_report_clarity(.x, "temperatureInternal1HourMean.value", data_to_check_completeness, count_this_month)),
      `Humid. Raw` = purrr::map_chr(sensor_index, ~ check_missing_data_report_clarity(.x, "relHumidInternal1HourMean.raw", data_to_check_completeness, count_this_month)),
      `Humid. Value` = purrr::map_chr(sensor_index, ~ check_missing_data_report_clarity(.x, "relHumidInternal1HourMean.value", data_to_check_completeness, count_this_month))
    )

  monitor_info_clarity <- read_monitor_info_from_monitor_tracking("Clarity") %>%
    dplyr::select(DeviceID, ShortCode)

  processed_report <- list(
    Archived = archived_clarity,
    RecentSensorData = clarity_last_month_data_clean,
    StatusCheck = dplyr::full_join(clarity_status_data_clean, monitor_info_clarity, by = dplyr::join_by(DeviceID)),
    GraphCheck = dplyr::full_join(graph_test, monitor_info_clarity, by = dplyr::join_by(sensor_index == DeviceID)),
    ArchiveCheck = dplyr::full_join(archive_test, monitor_info_clarity, by = dplyr::join_by(DeviceID)),
    CompleteCheck = dplyr::full_join(complete_test, monitor_info_clarity, by = dplyr::join_by(sensor_index == DeviceID))
  )

  report_folder <- file.path("CSV", "Instant-Report", format(report_date, "%Y-%m"))
  create_new_folder(report_folder, root_path = upload_root_folder)

  report_title <- paste("Status of Clarity sensors - ", format(report_date, "%Y-%m-%d"), sep = "")
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
