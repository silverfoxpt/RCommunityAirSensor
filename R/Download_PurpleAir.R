#' Download from PurpleAir API, process, and save to CSV files
#' @param current_date Date to determine the previous month for data extraction
#' @return None. Saves processed data to CSV files in specified directories.
#' @examples
#' Example usage:
#' myDate <- Sys.Date()
#' save_purpleAir_to_csv(myDate)
#' @export
#' @concept role:download
#' @concept removedDependencies:false
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:false
#' @concept cleanupComments:false
#' @concept addRoxygenComments:true
save_purpleAir_to_csv <- function(current_date) {
  # Installation and loading
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(readr, lubridate, tidyverse, glue, httr2, jsonlite, install = T, update = F)

  # grab sources
  sourcePath <- file.path(getwd(), "Code", "PurpleAirUtil.R")
  #source(file = sourcePath, echo = FALSE)

  sourcePath <- file.path(getwd(), "Code", "FileUtil.R")
  #source(file = sourcePath, echo = FALSE)

  sourcePath <- file.path(getwd(), "Code", "TimeUtil.R")
  #source(file = sourcePath, echo = FALSE)

  # get timestamp from start of month
  calc_time <- previous_month_bounds(current_date, nextMonth = TRUE)
  calc_time_day_only <- previous_month_bounds(current_date, date_only = TRUE)

  start_of_last_month <- calc_time_day_only$start
  start_of_current_month <- calc_time_day_only$end

  start_timestamp <- calc_time$start
  end_timestamp <- calc_time$end

  # Check if Log has already been collected
  logfile <-
    read.csv(file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Exports", "PurpleAirLog.csv")) %>%
    as_tibble()

  if (logfile %>% dplyr::filter(OriginDate == start_of_last_month) %>% nrow > 0) {
    return()
  }

  api_key <- Sys.getenv("PURPLEAPI")

  # Get DeviceID from CAMNMonitorTracking.xlsx file - synced to Box
  sitesInfo <- read_monitor_info_from_monitor_tracking("PurpleAir")

  # Extract information
  sensor_ids <- sitesInfo[['DeviceID']]
  sensor_owners <- sitesInfo[['Owner']]
  sensor_shortcode <- sitesInfo[['ShortCode']]

  # get daily data
  # map pass each element of vector as parameter of get_single_sensor_data
  rate <- rate_delay(2)
  slow_get <- purrr::slowly(get_single_sensor_data_custom, rate = rate, quiet = F)

  temp_list_sensors_data <-
    purrr::map(.x = sensor_ids,
               .f = purrr::possibly(slow_get, otherwise = NULL, quiet = F),
               neededFields = "temperature,humidity,pm2.5_alt,pm2.5_atm,pm2.5_cf_1",
               starting = start_timestamp,
               ending = end_timestamp,
               gap = "1440", #1440 -> daily (24 hours)
               api_key = api_key)

  # folder name
  newFolderName <- paste("PurpleAir.",
                         as.character(start_of_last_month), ".",
                         as.character(start_of_current_month),
                         sep = "")

  # creating folder
  folderPath <- file.path("CSV", "PurpleAir", newFolderName)
  create_new_folder(folderPath, root_path = Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"))

  # save to file - daily
  purrr::pwalk(.l = list(sensor_ids, temp_list_sensors_data, sensor_owners, sensor_shortcode),
               .f = save_aq_to_csv,
               average = "Daily",
               foldername = file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), folderPath),
               current_date = current_date
  )

  # get hourly data
  temp_list_sensors_data <-
    purrr::map(.x = sensor_ids,
               .f = purrr::possibly(slow_get, otherwise = NULL, quiet = F),
               neededFields = "temperature,humidity,pm2.5_alt,pm2.5_atm,pm2.5_cf_1",
               starting = start_timestamp,
               ending = end_timestamp,
               gap = "60", #60 -> hourly
               api_key = api_key)

  # save to file - hourly
  purrr::pwalk(.l = list(sensor_ids, temp_list_sensors_data, sensor_owners, sensor_shortcode),
               .f = save_aq_to_csv,
               average = "Hourly",
               foldername = file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), folderPath),
               current_date = current_date
  )

  # write to log
  write.table(
    data.frame(
      OriginDate = c(start_of_last_month),
      Complete = c('COMPLETED')
    ),
    file = file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Exports", "PurpleAirLog.csv"),
    sep = ",",
    col.names = FALSE,
    row.names = FALSE,
    append = TRUE
  )
}
#myDate <- Sys.Date()
#myDate <- as.Date("2025-07-02")
#save_purpleAir_to_csv(myDate)

# Finish testing: 22 February 2024
