create_new_folder <- function(folderPath, root_path = NULL) {
  if (is.null(root_path)) {
    root_path <- getwd()
  }
  data_dir <- file.path(root_path, folderPath)
  
  if (!file.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
}

read_monitor_info_from_monitor_tracking <- function(type, listAvailableSensor = NULL) {
  # Read from XLSX
  readMonitorTracking <- 
    readxl::read_xlsx(
      path = file.path(Sys.getenv("BOX_RECORDS_ROOT_FOLDER"), "CAMNMonitorTracking.xlsx"), 
      sheet = "MonitorStatus", # IN TESTING, THIS WILL CHANGE TO TESTINGONLY
      range = "A10:J100"
    ) %>%
    as_tibble() %>%
    dplyr::rename("DeviceID" = "API ID") %>%
    dplyr::rename("OrgID" = "Dashboard/API Organization ID") %>%
    dplyr::rename("ShortCode" = "Location short code") %>%
    dplyr::rename("SiteName" = "Deployed Site Location") 
  
  if (type == "Clarity") {
    sitesInfo <- readMonitorTracking %>%
      dplyr::filter(substr(Label, 1, 2) == "CN")  %>%
      dplyr::filter(substr(DeviceID, 1, 1) == "D") %>%
      dplyr::filter(OrgID != "", !is.na(OrgID), nchar(OrgID) >= 6) %>%
      rename_with(~ ifelse(str_detect(., "ID Number"), "NodeID", .)) %>%
      dplyr::mutate(Type = "Clarity") %>%
      dplyr::mutate(Subtype = case_when(
        ShortCode %in% c("MNR", "SYD") ~ "Co-located",
        grepl("park", SiteName, ignore.case = TRUE) ~ "Park",
        TRUE ~ "Non-park"
      ))
  }
  else if (type == "PurpleAir") {
    sitesInfo <- readMonitorTracking %>%
      dplyr::filter(substr(Label, 1, 2) == "PA")  %>%
      dplyr::filter(grepl("^\\d{5,}$", DeviceID)) %>%
      dplyr::filter(grepl("public", `Data sharing setting`)) %>%
      dplyr::mutate(Type = "PurpleAir") %>%
      dplyr::mutate(Subtype = case_when(
        ShortCode %in% c("MNR", "SYD") ~ "Co-located",
        grepl("park", SiteName, ignore.case = TRUE) ~ "Park",
        TRUE ~ "Non-park"
      ))
  }
  else if (type == "Qualtrics") {
    numCol <- ncol(readMonitorTracking)
    sitesInfo <- readMonitorTracking %>%
      dplyr::filter(rowSums(is.na(.) | . == "") < numCol) %>%
      dplyr::filter(nchar(ShortCode) >= 3) %>%
      dplyr::filter(grepl("public", `Data sharing setting`))
  }
  
  if (!is.null(listAvailableSensor)) {
    sitesInfo <- sitesInfo %>% dplyr::filter(DeviceID %in% listAvailableSensor)
  }
  return(sitesInfo)
}

read_reference_info_from_monitor_tracking <- function() {
  # Read from XLSX
  readReferenceTracking <- 
    readxl::read_xlsx(
      path = file.path(Sys.getenv("BOX_RECORDS_ROOT_FOLDER"), "CAMNMonitorTracking.xlsx"), 
      sheet = "ReferenceSiteData",
      range = "A3:E100"
    ) %>%
    as_tibble() %>%
    dplyr::rename("DatasourceID" = "Datasource ID") %>%
    dplyr::mutate(DeviceID = DatasourceID) %>%
    dplyr::rename("ShortCode" = "Short Code") %>%
    dplyr::rename("SiteName" = "Site Name") %>%
    dplyr::rename("CollectPM25" = "Collect PM2.5") %>%
    dplyr::rename("CollectNO2" = "Collect NO2") %>%
    dplyr::filter(!is.na(DatasourceID)) %>%
    dplyr::mutate(Type = "Reference") %>%
    dplyr::mutate(Subtype = "Reference")
  
  return(readReferenceTracking)
}
#tmp <- read_reference_info_from_monitor_tracking()

load_purple_air_data_from_archive <- function(startDateOfMonth) {
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
  
  return(list(
    Daily = dailyData,
    DailyLabel = dailyFiles,
    Hourly = hourlyData,
    HourlyLabel = hourlyFiles
  ))
}

load_clarity_data_from_archive <- function(startDateOfMonth) {
  # Load Purple Air sensor data!
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
  
  return(list(
    Daily = dailyData,
    DailyLabel = dailyFiles,
    Hourly = hourlyData,
    HourlyLabel = hourlyFiles
  ))
}

# Need update from above function for Qualtrics! Refactor it!
# Finish testing: 03 November 2024

