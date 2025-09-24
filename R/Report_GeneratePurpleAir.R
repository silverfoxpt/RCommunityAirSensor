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

sourcePath <- file.path(getwd(), "Code", "PurpleAirUtil.R") 
source(file = sourcePath, echo = FALSE)

sourcePath <- file.path(getwd(), "Code", "Report", "ReportFunctionPurpleAir.R") 
source(file = sourcePath, echo = FALSE)

#last_seen: <= 10min -> device still online
#rssi: Wi-Fi
#pm2.5: != NA -> Data collected
#confidence: >= 70 -> Confident

# Set up variables
api_key <- Sys.getenv("PURPLEAPI")

current_time <- now() %>% with_tz(tzone = "UTC") %>% as.numeric()
start_time <- current_time - lubridate::weeks(2) %>% as.numeric()

# get timestamp from start of month
calc_time <- previous_month_bounds(now(), iso8601 = F, date_only = T)
start_of_last_month <- calc_time$start
end_of_last_month <- calc_time$end

# Get DeviceID from CAMNMonitorTracking.xlsx file - synced to Box
sitesInfo <- read_monitor_info_from_monitor_tracking("PurpleAir")

# Extract information
sensor_ids <- sitesInfo[['DeviceID']] 
sensor_owners <- sitesInfo[['Owner']]
sensor_shortcode <- sitesInfo[['ShortCode']]

# Get monitor status ####
statusData <- get_multi_sensors_status(paste(sensor_ids, collapse = ','), 
                                 "last_seen,rssi,pm2.5,confidence",
                                 api_key)
Sys.sleep(2) # Safety purposes

# Process data
statusData <- statusData %>%
  dplyr::mutate(`Data Collected` = ifelse(is.na(pm2.5), "No", "Yes")) %>%
  dplyr::mutate(`Wi-Fi Bars` = case_when(
    is.na(rssi) ~ 0,  # Assign 1 if signal strength is missing (rest)
    rssi > -50  ~ 5,  # Strongest signal
    rssi > -60  ~ 4,
    rssi > -67  ~ 3,
    rssi > -80  ~ 2,
    TRUE ~ 1          # Default case (rest)
  )) %>%
  dplyr::mutate(`Device Online` = ifelse(current_time - last_seen <= 600, "Yes", "No")) %>% # 10 minutes
  dplyr::mutate(sensor_index = as.character(sensor_index))

# Check hourly 2-week data
rate <- rate_delay(2)
slow_get <- purrr::slowly(get_single_sensor_data_custom, rate = rate, quiet = F)

temp_list_sensors_data <-
  purrr::map(.x = sensor_ids,
             .f = purrr::possibly(slow_get, otherwise = NULL, quiet = F),
             neededFields = "temperature,humidity,pm2.5_alt,pm2.5_atm,pm2.5_cf_1",
             starting = start_time,
             ending = current_time,
             gap = "60", #60 -> 1 hour
             api_key = api_key)

temp_list_sensors_data <- temp_list_sensors_data %>% set_names(sensor_ids)
graphTest <- new_tibble(list(sensor_index = sensor_ids,
                             `PM2.5 Graph` = NA,
                             `Temperature Graph` = NA,
                             `Humidity Graph` = NA))

graphTest <- purrr::reduce2(
  .x = temp_list_sensors_data,
  .y = names(temp_list_sensors_data),
  .init = graphTest,
  .f = function(acc, x, y) {
      # Ensure sensor_index is character for matching
      acc <- acc %>% mutate(sensor_index = as.character(sensor_index))
      y <- as.character(y)  # Ensure y is also character
      
      # Compute results
      results <- list(
        "PM2.5 Graph" = test_sensor_malfunction_result(x, "time_stamp", "pm2.5_atm", start_time, current_time),
        "Temperature Graph" = test_sensor_malfunction_result(x, "time_stamp", "temperature", start_time, current_time),
        "Humidity Graph" = test_sensor_malfunction_result(x, "time_stamp", "humidity", start_time, current_time)
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
                    
                    else { # Check everything else
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

# Collect info from archived data
archived <- load_purple_air_data_from_archive(start_of_last_month)
fields <- c("temperature","humidity","pm2.5_alt","pm2.5_atm","pm2.5_cf_1")

archiveTest <- tibble(sensor_index = sensor_ids) %>% 
  mutate(`Daily Label` = map_chr(sensor_index, ~ ifelse(
    any(str_detect(archived$DailyLabel %>% as.character(), .x)),
    "Exist",
    "Non-exist"
  ))) %>%
  mutate(`Hourly Label` = map_chr(sensor_index, ~ ifelse(
    any(str_detect(archived$HourlyLabel %>% as.character(), .x)),
    "Exist",
    "Non-exist"
  ))) %>%
  dplyr::rowwise() %>%
  mutate(`Daily Headers` = map_chr(sensor_index, ~ ifelse(
    is.null(archived$Daily) || is.null(archived$Daily[[.x]]),
    "Missing",
    ifelse(test_full_headers(archived$Daily[[.x]], fields), "Normal", "Missing")
  ))) %>%
  mutate(`Hourly Headers` = map_chr(sensor_index, ~ ifelse(
    is.null(archived$Hourly) || is.null(archived$Hourly[[.x]]),
    "Missing",
    ifelse(test_full_headers(archived$Hourly[[.x]], fields), "Normal", "Missing")
  ))) %>%
  dplyr::ungroup()

# Completeness test
countThisMonth <- get_last_month_hours(now())
dataToCheckCompleteness <- archived$Hourly
completeTest <- tibble(sensor_index = sensor_ids) %>% 
  mutate(`Timestamp` = map_chr(sensor_index, ~ check_missing_data(.x, 'time_stamp', dataToCheckCompleteness, countThisMonth)),
         `PM25 Raw` = map_chr(sensor_index, ~ check_missing_data(.x, 'pm2.5_atm', dataToCheckCompleteness, countThisMonth)),
         `PM25 Value` = map_chr(sensor_index, ~ check_missing_data(.x, 'pm2.5_alt', dataToCheckCompleteness, countThisMonth)),
         `Temperature` = map_chr(sensor_index, ~ check_missing_data(.x, 'temperature', dataToCheckCompleteness, countThisMonth)),
         `Humidity` = map_chr(sensor_index, ~ check_missing_data(.x, 'humidity', dataToCheckCompleteness, countThisMonth)))

# testing
processed_report <- list(
  ArchivedData = archived,
  RecentSensorData = temp_list_sensors_data,
  
  StatusTest = statusData,
  GraphTest = graphTest,
  ArchiveTest = archiveTest,
  CompleteTest = completeTest
)

# Generate report and save to file
folderPath <- file.path("CSV", "Instant-Report", format(now(), "%Y-%m"))
create_new_folder(folderPath, root_path = Sys.getenv("BOX_UPLOAD_ROOT_FOLDER")) 

rmarkdown::render(
  file.path("Code", "Report", "PurpleAirInstantReport.Rmd"),
  params = list(
    title = paste("Status of Purple Air sensors - ", format(now(), "%Y-%m-%d"), sep = ""),
    myData = processed_report,
    debugTurnOn = T
  ),
  output_file = file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), folderPath,
                          paste("Status of Purple Air sensors - ", format(now(), "%Y-%m-%d"), ".pdf", sep = ""))
)
