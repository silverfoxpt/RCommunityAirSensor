# Import libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, glue, httr2, jsonlite, lubridate, here, rmarkdown, 
               data.table, formattable, tidyr, shiny, DT, knitr, kableExtra, 
               tinytex, flextable, officer, stringr, rlang, mailR, geometry,
               readr, readxl, patchwork, scales, ggforce, ggExtra, ggthemes)

# Get source functions
sourcePath <- file.path(getwd(), "Code", "FileUtil.R") 
source(file = sourcePath, echo = FALSE)

sourcePath <- file.path(getwd(), "Code", "Analysis", "DataAnalysis.R") 
source(file = sourcePath, echo = FALSE)

# Date is assumed to be YYYY-MM-DD
load_data_from_month_clarity <- function(startDateOfMonth) {
  # Load Clarity sensor data!
  sensorType <- "Clarity"
  startDate <- lubridate::as_date(startDateOfMonth) 
  endDate <- (startDate + months(1)) - days(1)
  
  folderName <- file.path(
    Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"),
    "CSV", as.character(sensorType),
    paste(sensorType, startDate, endDate, sep = ".")
    
  )
  fileNames <- list.files(path = folderName, pattern = "\\.csv$", full.names = TRUE)
  files <- data.frame(Filename = fileNames) %>% as_tibble()
  
  # Load daily data
  dailyFiles <- files %>%
    dplyr::filter(grepl("Daily", Filename))
  
  dailyData <- purrr::map(
      .x = dailyFiles %>% dplyr::pull(Filename),
      .f = \(x) {
        df <- read.csv(x)
        return(if ("datasourceId" %in% colnames(df)) as_tibble(df) else NULL)
      }
    ) %>%
    purrr::compact()
  
  sensorIDList <- purrr::map_chr(
    .x = dailyData,
    .f = \(x) { (x %>% dplyr::slice(1) %>% dplyr::pull("datasourceId") %>% as.character()) }
  )
  names(dailyData) <- sensorIDList
  
  # Load hourly data
  hourlyFiles <- files %>%
    dplyr::filter(grepl("Hourly", Filename))
  
  hourlyData <- purrr::map(
    .x = hourlyFiles %>% dplyr::pull(Filename),
    .f = \(x) {
      df <- read.csv(x)
      return(if ("datasourceId" %in% colnames(df)) as_tibble(df) else NULL)
    }
  ) %>%
    purrr::compact()
  
  names(hourlyData) <- purrr::map_chr(
    .x = hourlyData,
    .f = \(x) { (x %>% dplyr::slice(1) %>% dplyr::pull("datasourceId") %>% as.character()) }
  )
  
  #-------------------------------------------------------------------------------------
  # Load Clarity reference sensor data!
  sensorType <- "Clarity"
  startDate <- lubridate::as_date(startDateOfMonth) 
  endDate <- (startDate + months(1)) - days(1)
  
  folderName <- file.path(
    Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"),
    "CSV", as.character("Clarity-Reference"),
    paste(sensorType, startDate, endDate, sep = ".")
    
  )
  fileNames <- list.files(path = folderName, pattern = "\\.csv$", full.names = TRUE)
  files <- data.frame(Filename = fileNames) %>% as_tibble()
  
  # Load daily data
  dailyFiles <- files %>%
    dplyr::filter(grepl("Daily", Filename))
  
  dailyReferenceData <- purrr::map(
    .x = dailyFiles %>% dplyr::pull(Filename),
    .f = \(x) {
      df <- read.csv(x)
      return(if ("datasourceId" %in% colnames(df)) as_tibble(df) else NULL)
    }
  ) %>%
    purrr::compact()
  
  names(dailyReferenceData) <- purrr::map_chr(
    .x = dailyReferenceData,
    .f = \(x) { (x %>% dplyr::slice(1) %>% dplyr::pull("datasourceId") %>% as.character()) }
  )
  
  # Load hourly data
  hourlyFiles <- files %>%
    dplyr::filter(grepl("Hourly", Filename))
  
  hourlyReferenceData <- purrr::map(
    .x = hourlyFiles %>% dplyr::pull(Filename),
    .f = \(x) {
      df <- read.csv(x)
      return(if ("datasourceId" %in% colnames(df)) as_tibble(df) else NULL)
    }
  ) %>%
    purrr::compact()
  
  names(hourlyReferenceData) <- purrr::map_chr(
    .x = hourlyReferenceData,
    .f = \(x) { (x %>% dplyr::slice(1) %>% dplyr::pull("datasourceId") %>% as.character()) }
  )
  
  # -----------------------------------------------------
  # Load Clarity monitors info
  sitesInfo <- read_monitor_info_from_monitor_tracking("Clarity", listAvailableSensor = sensorIDList)
  
  # Load reference sites info
  referenceInfo <- read_reference_info_from_monitor_tracking()
  
  # Craft return
  return(list(
    Day = dailyData,
    Hour = hourlyData,
    DayReference = dailyReferenceData,
    HourReference = hourlyReferenceData,
    Info = sitesInfo,
    ReferenceInfo = referenceInfo,
    Date = startDate
  ))
}

# PurpleAir -----------------------------------------------------------
load_data_from_month_purpleAir <- function(startDateOfMonth) {
  # Load Purple Air sensor data!
  sensorType <- "PurpleAir"
  startDate <- lubridate::as_date(startDateOfMonth) 
  endDate <- (startDate + months(1)) - days(1)
  
  folderName <- file.path(
    Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"),
    "CSV", as.character(sensorType),
    paste(sensorType, startDate, endDate, sep = ".")
    
  )
  fileNames <- list.files(path = folderName, pattern = "\\.csv$", full.names = TRUE)
  files <- data.frame(Filename = fileNames) %>% as_tibble()
  
  # Load daily data
  dailyFiles <- files %>%
    dplyr::filter(grepl("Daily", Filename))
  
  sensorIDList <- map_chr(dailyFiles$Filename, \(x) stringr::str_extract(x, "(?<=_)\\d{6}(?=-)") %||% "")
  dailyData <- dailyFiles %>%
    dplyr::pull(Filename) %>%
    purrr::set_names(., map_chr(., \(x) stringr::str_extract(x, "(?<=_)\\d{6}(?=-)") %||% "")) %>%
    purrr::map(
      .f = \(x) {
        df <- read.csv(x)
        if ("time_stamp" %in% colnames(df)) (as_tibble(df) %>% mutate(startOfPeriod = time_stamp)) 
          else NULL
      }
    ) %>%
    purrr::compact() %>%
    purrr::imap(~.x %>% dplyr::mutate(datasourceId = .y))
  
  dailyData <- dailyData %>%
    purrr::map(
      .f = \(x) { return (x %>% dplyr::mutate(pm2_5ConcMass24HourMean.value = pm2.5_alt,
                                              pm2_5ConcMass24HourMean.raw = pm2.5_atm)) }
    )
  
  # ------------------------------------------------------------------------
  # Load hourly data
  hourlyFiles <- files %>%
    dplyr::filter(grepl("Hourly", Filename))
  
  hourlyData <- hourlyFiles %>%
    dplyr::pull(Filename) %>%
    purrr::set_names(., map_chr(., \(x) stringr::str_extract(x, "(?<=_)\\d{6}(?=-)") %||% "")) %>%
    purrr::map(
      .f = \(x) {
        df <- read.csv(x)
        if ("time_stamp" %in% colnames(df)) (as_tibble(df) %>% 
                                               mutate(startOfPeriod = time_stamp)) 
          else NULL
      }
    ) %>%
    purrr::compact() %>%
    purrr::imap(~.x %>% dplyr::mutate(datasourceId = .y)) 
  
  hourlyData <- hourlyData %>%
    purrr::map(
      .f = \(x) { return (x %>% dplyr::mutate(pm2_5ConcMass1HourMean.value = pm2.5_alt,
                                              pm2_5ConcMass24HourMean.raw = pm2.5_atm)) }
    )
  
  # -----------------------------------------------------
  # Load PurpleAir monitors info
  sitesInfo <- read_monitor_info_from_monitor_tracking("PurpleAir", listAvailableSensor = sensorIDList) %>%
    dplyr::filter(DeviceID %in% names(hourlyData)) %>% # filter out empty vars
    dplyr::filter(DeviceID %in% names(dailyData)) 
  
  # Craft return
  return(list(
    PurpleAirDay = dailyData,
    PurpleAirHour = hourlyData,
    PurpleAirInfo = sitesInfo
  ))
}

# Main -----------------------------------------------------------

strDate <- "2025-01-01"
tmp <- c(
  load_data_from_month_clarity(strDate),
  load_data_from_month_purpleAir(strDate)
)

tmp <- duplicate_data(tmp)
currentDate <- lubridate::as_date(strDate)

# Render PM2.5 
rmarkdown::render(
  file.path("Code", "Analysis", "DataAnalysisPM2_5.Rmd"),
  params = list(
    title = paste("PM2.5 Data Analysis for CAMN sensors - Month of ", format(currentDate, "%Y-%m"), sep = ""),
    myData = tmp,
    debugTurnOn = T
  )#,
  #output_file = pdfFilePath 
)

# Render NO2
rmarkdown::render(
  file.path("Code", "Analysis", "DataAnalysisNO2.Rmd"),
  params = list(
    title = paste("NO2 Data Analysis for CAMN sensors - Month of ", format(currentDate, "%Y-%m"), sep = ""),
    myData = tmp,
    debugTurnOn = T
  )#,
  #output_file = pdfFilePath 
)

# Tested: 3/2/2025