#' Load monthly Clarity CSV exports
#'
#' Loads the Clarity and Clarity reference monthly CSV exports for a target month.
#' The function reads the daily and hourly files, keys them by datasource ID, and
#' returns the data structures used by the analysis reports.
#'
#' @details
#' **Run:**
#' 1. Validates the upload root folder input.
#' 2. Calculates the monthly folder name from the supplied date.
#' 3. Reads daily and hourly Clarity CSV files.
#' 4. Reads daily and hourly Clarity reference CSV files.
#' 5. Loads monitor metadata for the Clarity sensors and reference sites.
#'
#' **Data processing details:**
#' - Daily and hourly files are filtered from the monthly CSV export folders.
#' - Files are kept only when they include a `datasourceId` column.
#' - The returned lists are named by the first `datasourceId` value in each file.
#' - Reference data are loaded from the `Clarity-Reference` export folder.
#'
#' **File structure:**
#' \preformatted{
#' upload_root_folder/
#' └── CSV/
#'     ├── Clarity/
#'     │   └── Clarity.YYYY-MM-DD.YYYY-MM-DD/
#'     │       ├── *Daily*.csv
#'     │       └── *Hourly*.csv
#'     └── Clarity-Reference/
#'         └── Clarity.YYYY-MM-DD.YYYY-MM-DD/
#'             ├── *Daily*.csv
#'             └── *Hourly*.csv
#' }
#'
#' @param start_date_of_month Date or date-like value used to determine the target month.
#' @param upload_root_folder Character string specifying the root upload folder.
#'   Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#'
#' @return A named list containing `Day`, `Hour`, `DayReference`, `HourReference`,
#'   `Info`, `ReferenceInfo`, and `Date`.
#'
#' @section Error handling:
#' The function stops with a clear message if `upload_root_folder` is missing or if the
#' monthly Clarity export folders cannot be read.
#'
#' @examples
#' \dontrun{
#' load_data_from_month_clarity(as.Date("2025-01-01"))
#' }
#'
#' @seealso
#' \code{\link{load_data_from_month_purpleAir}}
#'
#' @export
#' @concept role:process
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
load_data_from_month_clarity <- function(start_date_of_month,
                                         upload_root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  if (is.null(upload_root_folder) || !is.character(upload_root_folder) || length(upload_root_folder) != 1L || !nzchar(upload_root_folder)) {
    stop("upload_root_folder must be provided or set via UPLOAD_ROOT_FOLDER")
  }

  start_date <- lubridate::as_date(start_date_of_month)
  end_date <- (start_date + months(1)) - days(1)

  clarity_folder <- file.path(
    upload_root_folder,
    "CSV",
    "Clarity",
    paste("Clarity", start_date, end_date, sep = ".")
  )
  clarity_reference_folder <- file.path(
    upload_root_folder,
    "CSV",
    "Clarity-Reference",
    paste("Clarity", start_date, end_date, sep = ".")
  )

  if (!dir.exists(clarity_folder)) {
    stop("Clarity export folder not found: ", clarity_folder)
  }
  if (!dir.exists(clarity_reference_folder)) {
    stop("Clarity reference export folder not found: ", clarity_reference_folder)
  }

  file_names <- list.files(path = clarity_folder, pattern = "\\.csv$", full.names = TRUE)
  files <- tibble::as_tibble(data.frame(Filename = file_names))

  daily_files <- dplyr::filter(files, grepl("Daily", Filename))
  daily_data <- purrr::map(
    dplyr::pull(daily_files, Filename),
    function(x) {
      df <- read.csv(x)
      if ("datasourceId" %in% names(df)) {
        tibble::as_tibble(df)
      } else {
        NULL
      }
    }
  )
  daily_data <- purrr::compact(daily_data)
  daily_sensor_ids <- purrr::map_chr(
    daily_data,
    function(x) {
      if (nrow(x) == 0L) {
        ""
      } else {
        as.character(dplyr::pull(dplyr::slice(x, 1L), datasourceId))
      }
    }
  )
  names(daily_data) <- daily_sensor_ids

  hourly_files <- dplyr::filter(files, grepl("Hourly", Filename))
  hourly_data <- purrr::map(
    dplyr::pull(hourly_files, Filename),
    function(x) {
      df <- read.csv(x)
      if ("datasourceId" %in% names(df)) {
        tibble::as_tibble(df)
      } else {
        NULL
      }
    }
  )
  hourly_data <- purrr::compact(hourly_data)
  hourly_sensor_ids <- purrr::map_chr(
    hourly_data,
    function(x) {
      if (nrow(x) == 0L) {
        ""
      } else {
        as.character(dplyr::pull(dplyr::slice(x, 1L), datasourceId))
      }
    }
  )
  names(hourly_data) <- hourly_sensor_ids

  reference_file_names <- list.files(path = clarity_reference_folder, pattern = "\\.csv$", full.names = TRUE)
  reference_files <- tibble::as_tibble(data.frame(Filename = reference_file_names))

  reference_daily_files <- dplyr::filter(reference_files, grepl("Daily", Filename))
  daily_reference_data <- purrr::map(
    dplyr::pull(reference_daily_files, Filename),
    function(x) {
      df <- read.csv(x)
      if ("datasourceId" %in% names(df)) {
        tibble::as_tibble(df)
      } else {
        NULL
      }
    }
  )
  daily_reference_data <- purrr::compact(daily_reference_data)
  names(daily_reference_data) <- purrr::map_chr(
    daily_reference_data,
    function(x) {
      if (nrow(x) == 0L) {
        ""
      } else {
        as.character(dplyr::pull(dplyr::slice(x, 1L), datasourceId))
      }
    }
  )

  reference_hourly_files <- dplyr::filter(reference_files, grepl("Hourly", Filename))
  hourly_reference_data <- purrr::map(
    dplyr::pull(reference_hourly_files, Filename),
    function(x) {
      df <- read.csv(x)
      if ("datasourceId" %in% names(df)) {
        tibble::as_tibble(df)
      } else {
        NULL
      }
    }
  )
  hourly_reference_data <- purrr::compact(hourly_reference_data)
  names(hourly_reference_data) <- purrr::map_chr(
    hourly_reference_data,
    function(x) {
      if (nrow(x) == 0L) {
        ""
      } else {
        as.character(dplyr::pull(dplyr::slice(x, 1L), datasourceId))
      }
    }
  )

  sites_info <- read_monitor_info_from_monitor_tracking("Clarity", listAvailableSensor = daily_sensor_ids)
  reference_info <- read_reference_info_from_monitor_tracking()

  list(
    Day = daily_data,
    Hour = hourly_data,
    DayReference = daily_reference_data,
    HourReference = hourly_reference_data,
    Info = sites_info,
    ReferenceInfo = reference_info,
    Date = start_date
  )
}

#' Load monthly PurpleAir CSV exports
#'
#' Loads the PurpleAir monthly CSV exports for a target month. The function reads
#' the daily and hourly files, keys them by datasource ID extracted from the file
#' names, and returns the data structures used by the analysis reports.
#'
#' @details
#' **Run:**
#' 1. Validates the upload root folder input.
#' 2. Calculates the monthly folder name from the supplied date.
#' 3. Reads daily and hourly PurpleAir CSV files.
#' 4. Rewrites the value column names used by the report templates.
#' 5. Loads monitor metadata for the PurpleAir sensors.
#'
#' **Data processing details:**
#' - Datasource IDs are parsed from filenames using the embedded sensor identifier.
#' - Files are kept only when a `time_stamp` column exists.
#' - The returned lists are named by the parsed datasource ID.
#' - The metadata table is filtered to the loaded device IDs before returning.
#'
#' **File structure:**
#' \preformatted{
#' upload_root_folder/
#' └── CSV/
#'     └── PurpleAir/
#'         └── PurpleAir.YYYY-MM-DD.YYYY-MM-DD/
#'             ├── *Daily*.csv
#'             └── *Hourly*.csv
#' }
#'
#' @param start_date_of_month Date or date-like value used to determine the target month.
#' @param upload_root_folder Character string specifying the root upload folder.
#'   Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#'
#' @return A named list containing `PurpleAirDay`, `PurpleAirHour`, and `PurpleAirInfo`.
#'
#' @section Error handling:
#' The function stops with a clear message if `upload_root_folder` is missing or if the
#' monthly PurpleAir export folder cannot be read.
#'
#' @examples
#' \dontrun{
#' load_data_from_month_purpleAir(as.Date("2025-01-01"))
#' }
#'
#' @seealso
#' \code{\link{load_data_from_month_clarity}}
#'
#' @export
#' @concept role:process
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
load_data_from_month_purpleAir <- function(start_date_of_month,
                                           upload_root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  if (is.null(upload_root_folder) || !is.character(upload_root_folder) || length(upload_root_folder) != 1L || !nzchar(upload_root_folder)) {
    stop("upload_root_folder must be provided or set via UPLOAD_ROOT_FOLDER")
  }

  start_date <- lubridate::as_date(start_date_of_month)
  end_date <- (start_date + months(1)) - days(1)

  purple_air_folder <- file.path(
    upload_root_folder,
    "CSV",
    "PurpleAir",
    paste("PurpleAir", start_date, end_date, sep = ".")
  )

  if (!dir.exists(purple_air_folder)) {
    stop("PurpleAir export folder not found: ", purple_air_folder)
  }

  file_names <- list.files(path = purple_air_folder, pattern = "\\.csv$", full.names = TRUE)
  files <- tibble::as_tibble(data.frame(Filename = file_names))

  daily_files <- dplyr::filter(files, grepl("Daily", Filename))
  daily_sensor_ids <- purrr::map_chr(
    dplyr::pull(daily_files, Filename),
    function(x) {
      sensor_id <- stringr::str_extract(x, "(?<=_)\\d{6}(?=-)")
      if (is.na(sensor_id)) {
        ""
      } else {
        sensor_id
      }
    }
  )
  daily_data <- purrr::set_names(dplyr::pull(daily_files, Filename), daily_sensor_ids)
  daily_data <- purrr::map(
    daily_data,
    function(x) {
      df <- read.csv(x)
      if ("time_stamp" %in% names(df)) {
        tibble::as_tibble(df) %>% dplyr::mutate(startOfPeriod = time_stamp)
      } else {
        NULL
      }
    }
  )
  daily_data <- purrr::compact(daily_data)
  daily_data <- purrr::imap(
    daily_data,
    function(x, y) dplyr::mutate(x, datasourceId = y)
  )
  daily_data <- purrr::map(
    daily_data,
    function(x) {
      dplyr::mutate(
        x,
        pm2_5ConcMass24HourMean.value = pm2.5_alt,
        pm2_5ConcMass24HourMean.raw = pm2.5_atm
      )
    }
  )

  hourly_files <- dplyr::filter(files, grepl("Hourly", Filename))
  hourly_sensor_ids <- purrr::map_chr(
    dplyr::pull(hourly_files, Filename),
    function(x) {
      sensor_id <- stringr::str_extract(x, "(?<=_)\\d{6}(?=-)")
      if (is.na(sensor_id)) {
        ""
      } else {
        sensor_id
      }
    }
  )
  hourly_data <- purrr::set_names(dplyr::pull(hourly_files, Filename), hourly_sensor_ids)
  hourly_data <- purrr::map(
    hourly_data,
    function(x) {
      df <- read.csv(x)
      if ("time_stamp" %in% names(df)) {
        tibble::as_tibble(df) %>% dplyr::mutate(startOfPeriod = time_stamp)
      } else {
        NULL
      }
    }
  )
  hourly_data <- purrr::compact(hourly_data)
  hourly_data <- purrr::imap(
    hourly_data,
    function(x, y) dplyr::mutate(x, datasourceId = y)
  )
  hourly_data <- purrr::map(
    hourly_data,
    function(x) {
      dplyr::mutate(
        x,
        pm2_5ConcMass1HourMean.value = pm2.5_alt,
        pm2_5ConcMass24HourMean.raw = pm2.5_atm
      )
    }
  )

  sites_info <- read_monitor_info_from_monitor_tracking("PurpleAir", listAvailableSensor = daily_sensor_ids) %>%
    dplyr::filter(DeviceID %in% names(hourly_data)) %>%
    dplyr::filter(DeviceID %in% names(daily_data))

  list(
    PurpleAirDay = daily_data,
    PurpleAirHour = hourly_data,
    PurpleAirInfo = sites_info
  )
}

#' Render monthly analysis reports
#'
#' Loads the monthly Clarity and PurpleAir data, combines them into the report
#' structure, and renders the PM2.5 and NO2 analysis templates.
#'
#' @details
#' **Run:**
#' 1. Validates the upload root folder and template paths.
#' 2. Loads the monthly Clarity, Clarity reference, and PurpleAir CSV exports.
#' 3. Combines the loaded data with `duplicate_data()`.
#' 4. Renders the PM2.5 analysis report template.
#' 5. Renders the NO2 analysis report template.
#'
#' **Data processing details:**
#' - Monthly data are read from the Clarity and PurpleAir export folders.
#' - The combined list is passed to the report templates as `myData`.
#' - The `debug_turn_on` flag is forwarded to both report templates.
#'
#' **File structure:**
#' \preformatted{
#' upload_root_folder/
#' └── CSV/
#'     ├── Clarity/
#'     │   └── Clarity.YYYY-MM-DD.YYYY-MM-DD/
#'     ├── Clarity-Reference/
#'     │   └── Clarity.YYYY-MM-DD.YYYY-MM-DD/
#'     └── PurpleAir/
#'         └── PurpleAir.YYYY-MM-DD.YYYY-MM-DD/
#' }
#'
#' @param start_date_of_month Date or date-like value used to determine the target month.
#' @param upload_root_folder Character string specifying the root upload folder.
#'   Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @param pm25_template_path Character string specifying the PM2.5 R Markdown template path.
#' @param no2_template_path Character string specifying the NO2 R Markdown template path.
#' @param debug_turn_on Logical value passed to the report templates.
#'
#' @return A named list containing the combined analysis data and the render output paths.
#'
#' @section Error handling:
#' The function stops with clear messages if the upload root is missing, the report
#' templates cannot be found, or the monthly data folders cannot be read.
#'
#' @examples
#' \dontrun{
#' render_monthly_analysis_reports(as.Date("2025-01-01"))
#' }
#'
#' @seealso
#' \code{\link{load_data_from_month_clarity}},
#' \code{\link{load_data_from_month_purpleAir}},
#' \code{\link{duplicate_data}}
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
render_monthly_analysis_reports <- function(start_date_of_month,
                                           upload_root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER"),
                                           pm25_template_path = file.path("data-raw", "Analysis_TemplatePM2_5.Rmd"),
                                           no2_template_path = file.path("data-raw", "Analysis_TemplateNO2.Rmd"),
                                           debug_turn_on = TRUE) {
  if (is.null(upload_root_folder) || !is.character(upload_root_folder) || length(upload_root_folder) != 1L || !nzchar(upload_root_folder)) {
    stop("upload_root_folder must be provided or set via UPLOAD_ROOT_FOLDER")
  }
  if (!file.exists(pm25_template_path)) {
    stop("PM2.5 template not found: ", pm25_template_path)
  }
  if (!file.exists(no2_template_path)) {
    stop("NO2 template not found: ", no2_template_path)
  }

  clarity_data <- load_data_from_month_clarity(
    start_date_of_month = start_date_of_month,
    upload_root_folder = upload_root_folder
  )
  purple_air_data <- load_data_from_month_purpleAir(
    start_date_of_month = start_date_of_month,
    upload_root_folder = upload_root_folder
  )

  combined_data <- duplicate_data(c(clarity_data, purple_air_data))
  current_date <- lubridate::as_date(start_date_of_month)

  pm25_output <- rmarkdown::render(
    input = pm25_template_path,
    params = list(
      title = paste("PM2.5 Data Analysis for CAMN sensors - Month of ", format(current_date, "%Y-%m"), sep = ""),
      myData = combined_data,
      debugTurnOn = debug_turn_on,
      start_date = current_date
    )
  )

  no2_output <- rmarkdown::render(
    input = no2_template_path,
    params = list(
      title = paste("NO2 Data Analysis for CAMN sensors - Month of ", format(current_date, "%Y-%m"), sep = ""),
      myData = combined_data,
      debugTurnOn = debug_turn_on,
      start_date = current_date
    )
  )

  invisible(list(
    data = combined_data,
    pm25_output = pm25_output,
    no2_output = no2_output
  ))
}
