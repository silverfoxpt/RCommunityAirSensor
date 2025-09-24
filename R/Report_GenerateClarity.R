# Load libraries and source files
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, glue, httr2, jsonlite, lubridate, here, rmarkdown, 
               data.table, formattable, tidyr, shiny, DT, knitr, kableExtra, 
               tinytex, flextable, officer, stringr, rlang, mailR, geometry,
               readr, readxl, patchwork, scales, ggforce, ggExtra, ggthemes)

sourcePath <- file.path(getwd(), "Code", "FileUtil.R") 
source(file = sourcePath, echo = FALSE)

sourcePath <- file.path(getwd(), "Code", "TimeUtil.R") 
source(file = sourcePath, echo = FALSE)

sourcePath <- file.path(getwd(), "Code", "ClarityUtil.R") 
source(file = sourcePath, echo = FALSE)

sourcePath <- file.path(getwd(), "Code", "Report", "ReportFunctionClarity.R") 
source(file = sourcePath, echo = FALSE)

# Set up variables
current_time <- now() %>% with_tz(tzone = "UTC") %>% format_timestamp()
start_time_2week <- (now() %>% with_tz(tzone = "UTC") - lubridate::weeks(2)) %>% format_timestamp()

# get timestamp from start of month
calc_time <- previous_month_bounds(now())
calc_time_day_only <- previous_month_bounds(now(), date_only = TRUE)

start_of_last_month <- calc_time_day_only$start
start_of_current_month <- calc_time_day_only$end

clarity_api_key <- Sys.getenv("CLARITYAPI")

# Get DeviceID from CAMNMonitorTracking.xlsx file - synced to Box
sitesInfo <- read_monitor_info_from_monitor_tracking("Clarity")

# Extract information
deviceId <- sitesInfo[['DeviceID']]
nodeId <- sitesInfo[['NodeID']]
dictForDeviceID <- as.list(deviceId) %>% setNames(nodeId)

sensor_owners <- sitesInfo[['Owner']]
sensor_shortcode <- sitesInfo[['ShortCode']]
orgID <- sitesInfo[["OrgID"]]

# Combine & extract
uniqueOrgID <- unique(orgID)

# 1. Get current status from Clarity sensors ####
clarity_status_data <-
  purrr::map(
    .x = uniqueOrgID,
    .f = function(x,y) get_for_status_clarity_custom_ORG(x, clarity_api_key)
  ) 

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
  #dplyr::select(-which(grepl("summaryPer", names(clarity_status_data_clean)))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(`Device Online` = ifelse(convert_to_time(current_time, original_format = "%Y-%m-%dT%H:%M:%SZ") - 
                                           convert_to_time(lastReadingReceivedAt, original_format = "%Y-%m-%dT%H:%M:%OSZ") <= 600, 
                                         "Yes", "No")) %>%
  dplyr::mutate(DeviceID = dictForDeviceID[[nodeId]]) %>%
  dplyr::ungroup()

Sys.sleep(2) # Safety purposes

# 2. Get and analyze 2 week HOURLY data from Clarity sensors ####
# make request - hourly
clarity_twoweek_data <-
  purrr::map(
    .x = uniqueOrgID,
    .f = function(x,y) get_clarity_data_custom_v2_ORG(x, clarity_api_key, "hour", start_time_2week, current_time)
  ) %>%
  dplyr::bind_rows() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(startOfPeriod = format_timestamp(startOfPeriod)) %>%
  dplyr::mutate(endOfPeriod = format_timestamp(endOfPeriod)) %>%
  dplyr::ungroup()

# split CSV file to distinct datasource IDs
clarity_twoweek_data_clean <-
  split_clarity_data_by_datasource(clarity_twoweek_data, deviceId) %>%
  purrr::list_flatten() %>%
  setNames(deviceId)

graphTest <- new_tibble(list(sensor_index = deviceId,
                             `PM2.5 Graph` = NA,
                             `NO2 Graph` = NA,
                             `Temperature Graph` = NA,
                             `Humidity Graph` = NA))

graphTest <- purrr::reduce2(
  .x = clarity_twoweek_data_clean,
  .y = names(clarity_twoweek_data_clean),
  .init = graphTest,
  .f = function(acc, x, y) {
    # Ensure sensor_index is character for matching
    acc <- acc %>% mutate(sensor_index = as.character(sensor_index))
    y <- as.character(y)  # Ensure y is also character
    
    # Compute results
    results <- list(
      "PM2.5 Graph" = test_sensor_malfunction_result(x, "startOfPeriod", "pm2_5ConcMass1HourMean.value", start_time_2week, current_time),
      "NO2 Graph" = test_sensor_malfunction_result(x, "startOfPeriod", "no2Conc1HourMean.value", start_time_2week, current_time),
      "Temperature Graph" = test_sensor_malfunction_result(x, "startOfPeriod", "temperatureInternal1HourMean.raw", start_time_2week, current_time),
      "Humidity Graph" = test_sensor_malfunction_result(x, "startOfPeriod", "relHumidInternal1HourMean.raw", start_time_2week, current_time)
    )
    
    # Ensure sensor_index exists in acc (otherwise, add it)
    if (!y %in% acc$sensor_index) {
      acc <- bind_rows(acc, tibble(sensor_index = y))
    }
    
    # Update relevant rows
    acc %>%
      mutate(across(
        names(results),
        ~ifelse(sensor_index == y,
                {
                  if (!is.na(results[[cur_column()]]$error)) {
                    issues <- "No data"
                  }
                  
                  else { # Data retrieved
                    issues <- c()
                    if (nrow(results[[cur_column()]]$spikes) > 0) issues <- c(issues, paste("Outliers:", nrow(results[[cur_column()]]$spikes)))
                    if (nrow(results[[cur_column()]]$prolonged_sequences) > 0) {
                      issues <- c(issues, "Abnormal data")
                      #print(results[[cur_column()]]$prolonged_sequences)
                    }
                    if (results[[cur_column()]]$trends_flag) issues <- c(issues, results[[cur_column()]]$trends_type)
                    if (nrow(results[[cur_column()]]$missing_timestamps) > 0) {
                      issues <- c(issues, paste("Timestamps missing:", 
                                                (nrow(results[[cur_column()]]$missing_timestamps) / (24 * 14) * 100) %>% formatC(digits = 2, format = "f")),
                                  "%"
                      )
                    }
                    if (nrow(results[[cur_column()]]$duplicate_timestamps) > 0) issues <- c(issues, paste("Timestamps duplicates:", nrow(results[[cur_column()]]$duplicate_timestamps)))
                  }
                  
                  if (length(issues) > 0) str_c(issues, collapse = "\n") else "No problem"
                },
                .x
        )
      ))
  }
)

# 3. Check archived Claritydata ####  
archived_clarity <- load_clarity_data_from_archive(start_of_last_month)
dailyFields <- c(
  "datasourceId", "sourceId", "sourceType", "outputFrequency", "startOfPeriod", 
  "endOfPeriod", "locationLatitude", "locationLongitude", 
  "pm2_5ConcMass24HourMean.value", "no2Conc24HourMean.value", 
  "temperatureInternal24HourMean.raw", "relHumidInternal24HourMean.raw"
)
hourlyFields <- c(
  "datasourceId", "sourceId", "sourceType", "outputFrequency", "startOfPeriod", 
  "endOfPeriod", "locationLatitude", "locationLongitude", 
  "pm2_5ConcMass1HourMean.value", "no2Conc1HourMean.value", 
  "temperatureInternal1HourMean.raw", "relHumidInternal1HourMean.raw"
)

archiveTest <- tibble(DeviceID = deviceId) %>%
  mutate(`Daily Label` = map_chr(DeviceID, ~ ifelse(
    any(str_detect(archived_clarity$DailyLabel %>% as.character(), .x)),
    "Exist",
    "Non-exist"
  ))) %>%
  mutate(`Hourly Label` = map_chr(DeviceID, ~ ifelse(
    any(str_detect(archived_clarity$HourlyLabel %>% as.character(), .x)),
    "Exist",
    "Non-exist"
  ))) %>%
  dplyr::rowwise() %>%
  mutate(`Daily Headers` = map_chr(DeviceID, ~ ifelse(
    is.null(archived_clarity$Daily) || is.null(archived_clarity$Daily[[.x]]), 
    "Missing",
    ifelse(test_full_headers(archived_clarity$Daily[[.x]], dailyFields), "Normal", "Missing")
  ))) %>%
  mutate(`Hourly Headers` = map_chr(DeviceID, ~ ifelse(
    is.null(archived_clarity$Hourly) || is.null(archived_clarity$Hourly[[.x]]), 
    "Missing",
    ifelse(test_full_headers(archived_clarity$Hourly[[.x]], hourlyFields), "Normal", "Missing")
  ))) %>%
  dplyr::ungroup()

# 4. Completeness test ####
countThisMonth <- get_last_month_hours(now())
dataToCheckCompleteness <- archived_clarity$Hourly
completeTest <- tibble(sensor_index = deviceId) %>% 
  mutate(`Time` = map_chr(sensor_index, ~ check_missing_data(.x, 'startOfPeriod', dataToCheckCompleteness, countThisMonth)),
         `PM25 Raw` = map_chr(sensor_index, ~ check_missing_data(.x, 'pm2_5ConcMass1HourMean.raw', dataToCheckCompleteness, countThisMonth)),
         `PM25 Value` = map_chr(sensor_index, ~ check_missing_data(.x, 'pm2_5ConcMass1HourMean.value', dataToCheckCompleteness, countThisMonth)),
         `NO2 Raw` = map_chr(sensor_index, ~ check_missing_data(.x, 'no2Conc1HourMean.raw', dataToCheckCompleteness, countThisMonth)),
         `NO2 Value` = map_chr(sensor_index, ~ check_missing_data(.x, 'no2Conc1HourMean.value', dataToCheckCompleteness, countThisMonth)),
         `Temp Raw` = map_chr(sensor_index, ~ check_missing_data(.x, 'temperatureInternal1HourMean.raw', dataToCheckCompleteness, countThisMonth)),
         `Temp Value` = map_chr(sensor_index, ~ check_missing_data(.x, 'temperatureInternal1HourMean.value', dataToCheckCompleteness, countThisMonth)),
         `Humid. Raw` = map_chr(sensor_index, ~ check_missing_data(.x, 'relHumidInternal1HourMean.raw', dataToCheckCompleteness, countThisMonth)),
         `Humid. Value` = map_chr(sensor_index, ~ check_missing_data(.x, 'relHumidInternal1HourMean.value', dataToCheckCompleteness, countThisMonth))
        )

# Compose final variable
processed_report <- list(
  Archived = archived_clarity,
  RecentSensorData = clarity_twoweek_data_clean,
  StatusCheck = clarity_status_data_clean,
  GraphCheck = graphTest,
  ArchiveCheck = archiveTest,
  CompleteCheck = completeTest
)

# Generate report and save to file
folderPath <- file.path("CSV", "Instant-Report", format(now(), "%Y-%m"))
create_new_folder(folderPath, root_path = Sys.getenv("BOX_UPLOAD_ROOT_FOLDER")) 

rmarkdown::render(
  file.path("Code", "Report", "ClarityInstantReport.Rmd"),
  params = list(
    title = paste("Status of Clarity sensors - ", format(now(), "%Y-%m-%d"), sep = ""),
    myData = processed_report,
    debugTurnOn = T
  ),
  output_file = file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), folderPath,
                          paste("Status of Clarity sensors - ", format(now(), "%Y-%m-%d"), ".pdf", sep = ""))
)
