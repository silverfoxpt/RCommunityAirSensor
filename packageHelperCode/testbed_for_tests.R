library(httptest2)

# set temp directory
temp_test_root_folder <- withr::local_tempdir()

# copy extdata file into the temp folder to not muddle the original extdata
test_data_path <- system.file("extdata", package = "CoAirSensor")
file.copy(from = test_data_path,
          to = temp_test_root_folder,
          recursive = TRUE,
          copy.mode = TRUE)

# set env
env_path <- system.file("extdata", ".RExtEnvTest", package = "CoAirSensor")
envs <- read.dcf(env_path)

# use with_envvar to enable temp. env. variables switching
withr::with_envvar(
  c(
    UPLOAD_ROOT_FOLDER = file.path(temp_test_root_folder, "extdata"),
    RECORDS_ROOT_FOLDER = file.path(temp_test_root_folder, "extdata"),
    PURPLEAPI = "REDACTED_PURPLEAIR_SAMPLE_KEY"
  ),
  {
    sitesInfo <- read_monitor_info_from_monitor_tracking("PurpleAir")
  }
)

root_folder <- file.path(temp_test_root_folder, "extdata")
records_folder <- file.path(temp_test_root_folder, "extdata")
api_key <- "REDACTED_PURPLEAIR_SAMPLE_KEY"

# compute previous month bounds (assumes helper `previous_month_bounds` exists)
current_date <- as.Date("2026-05-02")
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
api_key <- "REDACTED_PURPLEAIR_SAMPLE_KEY"

sensor_ids <- sitesInfo[["DeviceID"]]
sensor_owners <- sitesInfo[["Owner"]]
sensor_shortcode <- sitesInfo[["ShortCode"]]

# prepare rate-limited getter
rate <- purrr::rate_delay(2)
slow_get <- purrr::slowly(get_single_sensor_data_custom, rate = rate, quiet = FALSE)

# fetch daily data (gap = 1440 -> daily)
httptest2::set_redactor(
  ~ httptest2::redact_headers(., "X-API-Key")
)
httptest2::.mockPaths("tests/testthat/fixtures")
real_key <- "redacted_bleh"
# httptest2::capture_requests({
#   temp_list_sensors_data <- purrr::map(.x = sensor_ids,
#                                        .f = purrr::possibly(slow_get, otherwise = NULL, quiet = FALSE),
#                                        neededFields = "temperature,humidity,pm2.5_alt,pm2.5_atm,pm2.5_cf_1",
#                                        starting = start_timestamp,
#                                        ending = end_timestamp,
#                                        gap = "1440",
#                                        api_key = real_key)
# })

httptest2::with_mock_api({
  hourly_data_fetch <- purrr::map(.x = sensor_ids,
                                       .f = purrr::possibly(slow_get, otherwise = NULL, quiet = FALSE),
                                       neededFields = "temperature,humidity,pm2.5_alt,pm2.5_atm,pm2.5_cf_1",
                                       starting = start_timestamp,
                                       ending = end_timestamp,
                                       gap = "60",
                                       api_key = "blep-blep-bloop-bloop")
})

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
httptest2::capture_requests({
  temp_list_sensors_data <- purrr::map(.x = sensor_ids,
                                       .f = purrr::possibly(slow_get, otherwise = NULL, quiet = FALSE),
                                       neededFields = "temperature,humidity,pm2.5_alt,pm2.5_atm,pm2.5_cf_1",
                                       starting = start_timestamp,
                                       ending = end_timestamp,
                                       gap = "60",
                                       api_key = real_key)
})

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

