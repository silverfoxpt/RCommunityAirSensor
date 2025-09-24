save_clarity_to_csv <- function(current_date) {
  # Downloading and installation
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, ggplot2, dplyr, httr2, glue, stats, readr, readxl)

  # get sources
  sourcePath <- file.path(getwd(), "Code", "FileUtil.R")
  #source(file = sourcePath, echo = FALSE)

  sourcePath <- file.path(getwd(), "Code", "ClarityUtil.R")
  #source(file = sourcePath, echo = FALSE)

  sourcePath <- file.path(getwd(), "Code", "TimeUtil.R")
  #source(file = sourcePath, echo = FALSE)

  # get timestamp from start of month
  calc_time <- previous_month_bounds(current_date)
  calc_time_day_only <- previous_month_bounds(current_date, date_only = TRUE)

  start_of_last_month <- calc_time_day_only$start
  start_of_current_month <- calc_time_day_only$end

  start_time_ISO <- calc_time$start
  end_time_ISO <- calc_time$end

  # Check if Log has already been collected
  logfile <-
    read.csv(file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Exports", "ClarityLog.csv")) %>%
    as_tibble()

  if (logfile %>% dplyr::filter(OriginDate == start_of_last_month) %>% nrow > 0) {
    return()
  }

  clarity_api_key <- Sys.getenv("CLARITYAPI")

  # Get DeviceID from CAMNMonitorTracking.xlsx file - synced to Box
  sitesInfo <- read_monitor_info_from_monitor_tracking("Clarity")

  # Extract information
  deviceId <- sitesInfo[['DeviceID']]
  sensor_owners <- sitesInfo[['Owner']]
  sensor_shortcode <- sitesInfo[['ShortCode']]
  orgID <- sitesInfo[["OrgID"]]

  # Distinct OrgID
  uniqueOrgID <- unique(orgID)

  # Get measurements by OrgID
  # make request - daily
  clarity_data <-
    purrr::map(
      .x = uniqueOrgID,
      .f = function(x,y) get_clarity_data_custom_v2_ORG(x, clarity_api_key, "day", start_time_ISO, end_time_ISO)
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(startOfPeriod = format_timestamp(startOfPeriod)) %>%
    dplyr::mutate(endOfPeriod = format_timestamp(endOfPeriod)) %>%
    dplyr::ungroup()

  # Get references sites - daily
  clarity_reference_data <-
    split_clarity_reference_data_by_datasource(clarity_data) %>%
    purrr::list_flatten()
  clarity_reference_data <- clarity_reference_data[!duplicated(names(clarity_reference_data))]

  # split CSV file to distinct datasource IDs
  clarity_data <-
    split_clarity_data_by_datasource(clarity_data, deviceId) %>%
    purrr::list_flatten()

  # folder name
  newFolderName <- paste("Clarity.",
                         as.character(start_of_last_month), ".",
                         as.character(start_of_current_month),
                         sep = "")

  # creating folder - clarity
  folderPath <- file.path("CSV", "Clarity", newFolderName)
  create_new_folder(folderPath, root_path = Sys.getenv("BOX_UPLOAD_ROOT_FOLDER")) # for sensor

  # save to file - daily
  purrr::pwalk(.l = list(deviceId, clarity_data, sensor_owners, sensor_shortcode),
               .f = save_clarity_aq_to_csv,
               average = "Daily",
               foldername = file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), folderPath),
               current_date = current_date
  )

  # creating folder - clarity reference
  referenceFolderPath <- file.path("CSV", "Clarity-Reference", newFolderName)
  create_new_folder(referenceFolderPath, root_path = Sys.getenv("BOX_UPLOAD_ROOT_FOLDER")) # for reference

  # save to file - reference daily
  purrr::walk(.x = clarity_reference_data,
              .f = save_clarity_aq_reference_to_csv,
              average = "Daily",
              foldername = file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), referenceFolderPath),
              current_date = current_date
  )
  #-----------------------------------------------------------------------------

  # Get measurements by OrgID
  # make request - hourly
  clarity_data <-
    purrr::map(
      .x = uniqueOrgID,
      .f = function(x,y) get_clarity_data_custom_v2_ORG(x, clarity_api_key, "hour", start_time_ISO, end_time_ISO)
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(startOfPeriod = format_timestamp(startOfPeriod)) %>%
    dplyr::mutate(endOfPeriod = format_timestamp(endOfPeriod)) %>%
    dplyr::ungroup()

  # Get references sites - hourly
  clarity_reference_data <-
    split_clarity_reference_data_by_datasource(clarity_data)%>%
    purrr::list_flatten()
  clarity_reference_data <- clarity_reference_data[!duplicated(names(clarity_reference_data))]

  # split CSV file to distinct datasource IDs
  clarity_data <-
    split_clarity_data_by_datasource(clarity_data, deviceId)%>%
    purrr::list_flatten()

  # save to file - hourly
  purrr::pwalk(.l = list(deviceId, clarity_data, sensor_owners, sensor_shortcode),
               .f = save_clarity_aq_to_csv,
               average = "Hourly",
               foldername = file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), folderPath),
               current_date = current_date
  )

  # save to file - reference hourly
  purrr::walk(.x = clarity_reference_data,
              .f = save_clarity_aq_reference_to_csv,
              average = "Hourly",
              foldername = file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), referenceFolderPath),
              current_date = current_date
  )

  # write to log
  write.table(
    data.frame(
      OriginDate = c(start_of_last_month),
      Complete = c('COMPLETED')
    ),
    file = file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Exports", "ClarityLog.csv"),
    sep = ",",
    col.names = FALSE,
    row.names = FALSE,
    append = TRUE
  )
}
#myDate <- Sys.Date()
#myDate <- as.Date("2025-07-02")
#tmp <- save_clarity_to_csv(myDate)

# Finish testing: 21 Feb 2025
