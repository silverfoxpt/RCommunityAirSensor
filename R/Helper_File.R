#' Create New Folder Directory
#'
#' Creates a new folder directory with optional root path specification.
#' Uses recursive creation to build nested directory structures.
#'
#' @param folderPath Character string specifying the folder path to create
#' @param root_path Character string specifying root directory (default: current working directory)
#'
#' @return NULL (function called for side effects)
#'
#' @details
#' If root_path is NULL, uses current working directory as base.
#' Creates nested directories recursively if they don't exist.
#'
#' @examples
#' \dontrun{
#' create_new_folder("data/csv")
#' create_new_folder("output", "/path/to/project")
#' }
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
create_new_folder <- function(folderPath, root_path = NULL) {
  if (is.null(root_path)) {
    root_path <- getwd()
  }
  data_dir <- file.path(root_path, folderPath)

  if (!file.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
}

#' Read Monitor Information from Tracking File
#'
#' Extracts sensor information from CAMNMonitorTracking.xlsx based on sensor type.
#' Filters and processes data according to type-specific criteria.
#'
#' @param type Character string specifying sensor type ("Clarity", "PurpleAir", "Qualtrics")
#' @param listAvailableSensor Character vector to filter sensors (optional)
#'
#' @return Tibble containing filtered monitor information with standardized columns
#'
#' @details
#' Reads from MonitorStatus sheet and applies type-specific filtering:
#' - Clarity: CN prefix, device ID starts with "D", valid organization ID
#' - PurpleAir: PA prefix, numeric device ID, public data sharing
#' - Qualtrics: Non-empty rows, valid short codes, public sharing
#'
#' @examples
#' \dontrun{
#' clarity_sensors <- read_monitor_info_from_monitor_tracking("Clarity")
#' pa_sensors <- read_monitor_info_from_monitor_tracking("PurpleAir")
#' }
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
read_monitor_info_from_monitor_tracking <- function(type, listAvailableSensor = NULL) {
  # Read monitor tracking data from Excel file and standardize column names
  readMonitorTracking <-
    readxl::read_xlsx(
      path = file.path(Sys.getenv("RECORDS_ROOT_FOLDER"), "CAMNMonitorTracking.xlsx"),
      sheet = "MonitorStatus",
      range = "A2:K100"
    ) %>%
    dplyr::as_tibble() %>%
    dplyr::rename("DeviceID" = "API ID") %>%
    dplyr::rename("OrgID" = "Dashboard/API Organization ID") %>%
    dplyr::rename("ShortCode" = "Location Short Code") %>%
    dplyr::rename("SiteName" = "Deployed Site Location")

  # Filter and process data based on sensor type
  if (type == "Clarity") {
    # Get reference site short codes for co-located identification
    reference_shortcodes <- get_reference_site_shortcodes()
    
    # Filter for Clarity sensors: CN prefix, device ID starts with D, valid org ID
    sitesInfo <- readMonitorTracking %>%
      dplyr::filter(substr(Label, 1, 2) == "CN")  %>%
      dplyr::filter(substr(DeviceID, 1, 1) == "D") %>%
      dplyr::filter(OrgID != "", !is.na(OrgID), nchar(OrgID) >= 6) %>%
      dplyr::rename_with(~ ifelse(stringr::str_detect(., "ID Number"), "NodeID", .)) %>%
      dplyr::mutate(Type = "Clarity") %>%
      # Classify sites by location type for analysis (using dynamic reference codes)
      dplyr::mutate(Subtype = case_when(
        ShortCode %in% reference_shortcodes ~ "Co-located",
        grepl("park", SiteName, ignore.case = TRUE) ~ "Park",
        TRUE ~ "Non-park"
      ))
  }
  else if (type == "PurpleAir") {
    # Get reference site short codes for co-located identification
    reference_shortcodes <- get_reference_site_shortcodes()
    
    # Filter for PurpleAir sensors: PA prefix, numeric device ID, public data sharing
    sitesInfo <- readMonitorTracking %>%
      dplyr::filter(substr(Label, 1, 2) == "PA")  %>%
      dplyr::filter(grepl("^\\d{5,}$", DeviceID)) %>%
      dplyr::filter(grepl("public", `Data Sharing Setting`)) %>%
      dplyr::mutate(Type = "PurpleAir") %>%
      # Classify sites by location type for analysis (using dynamic reference codes)
      dplyr::mutate(Subtype = case_when(
        ShortCode %in% reference_shortcodes ~ "Co-located",
        grepl("park", SiteName, ignore.case = TRUE) ~ "Park",
        TRUE ~ "Non-park"
      ))
  }
  else if (type == "Qualtrics") {
    # Filter for Qualtrics surveys: non-empty rows, valid short codes, public sharing
    numCol <- ncol(readMonitorTracking)
    sitesInfo <- readMonitorTracking %>%
      dplyr::filter(rowSums(is.na(.) | . == "") < numCol) %>%
      dplyr::filter(nchar(ShortCode) >= 3) %>%
      dplyr::filter(grepl("public", `Data sharing setting`))
  }

  # Apply additional sensor filtering if provided
  if (!is.null(listAvailableSensor)) {
    sitesInfo <- sitesInfo %>% dplyr::filter(DeviceID %in% listAvailableSensor)
  }
  return(sitesInfo)
}

#' Get Reference Site Short Codes
#'
#' Extracts short codes from reference sites for identifying co-located sensors.
#' Used to dynamically determine which sensors are co-located with reference stations.
#'
#' @return Character vector of short codes from all reference sites
#'
#' @details
#' Reads reference site data and extracts short codes to identify co-located sensors.
#' This eliminates hardcoded values and ensures accuracy when reference sites change.
#'
#' @examples
#' \dontrun{
#' ref_codes <- get_reference_site_shortcodes()
#' }
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_reference_site_shortcodes <- function() {
  # Read reference site data to get current short codes
  reference_data <- read_reference_info_from_monitor_tracking()
  
  # Extract unique short codes and remove any empty/NA values
  shortcodes <- reference_data %>%
    dplyr::pull(ShortCode) %>%
    unique() %>%
    .[!is.na(.) & . != ""]
  
  return(shortcodes)
}

#' Read Reference Site Information from Tracking File
#'
#' Extracts reference station information from CAMNMonitorTracking.xlsx.
#' Processes ReferenceSiteData sheet for air quality reference stations.
#'
#' @return Tibble containing reference site information with standardized columns
#'
#' @details
#' Reads from ReferenceSiteData sheet and standardizes column names.
#' Filters out rows with missing datasource IDs and adds type classification.
#'
#' @examples
#' \dontrun{
#' reference_sites <- read_reference_info_from_monitor_tracking()
#' }
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
read_reference_info_from_monitor_tracking <- function() {
  # Read reference site data from Excel and standardize column names
  readReferenceTracking <-
    readxl::read_xlsx(
      path = file.path(Sys.getenv("RECORDS_ROOT_FOLDER"), "CAMNMonitorTracking.xlsx"),
      sheet = "ReferenceSiteData",
      range = "A2:E100"
    ) %>%
    dplyr::as_tibble() %>%
    # Standardize column names to match sensor data format
    dplyr::rename("DatasourceID" = "Datasource ID") %>%
    dplyr::mutate(DeviceID = DatasourceID) %>%
    dplyr::rename("ShortCode" = "Short Code") %>%
    dplyr::rename("SiteName" = "Site Name") %>%
    dplyr::rename("CollectPM25" = "Collect PM2.5") %>%
    dplyr::rename("CollectNO2" = "Collect NO2") %>%
    # Remove rows with missing datasource IDs
    dplyr::filter(!is.na(DatasourceID)) %>%
    # Add type classification for consistency
    dplyr::mutate(Type = "Reference") %>%
    dplyr::mutate(Subtype = "Reference")

  return(readReferenceTracking)
}

#' Load PurpleAir Data from Archive
#'
#' Loads previously saved PurpleAir sensor data from CSV archive files.
#' Processes both daily and hourly data for the specified month.
#'
#' @param startDateOfMonth Date object representing the first day of the target month
#'
#' @return List containing Daily and Hourly data frames with corresponding file labels
#'
#' @details
#' Searches for CSV files in the PurpleAir archive folder and loads data
#' that contains valid time_stamp columns. Returns organized lists for
#' both daily and hourly aggregation periods.
#'
#' @examples
#' \dontrun{
#' data <- load_purple_air_data_from_archive(as.Date("2025-01-01"))
#' daily_data <- data$Daily
#' hourly_data <- data$Hourly
#' }
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
load_purple_air_data_from_archive <- function(startDateOfMonth) {
  # Calculate date range for the specified month
  sensorType <- "PurpleAir"
  startDate <- lubridate::as_date(startDateOfMonth)
  endDate <- (startDate + months(1)) - lubridate::days(1)

  # Build folder path for archived CSV files
  folderName <- file.path(
    Sys.getenv("UPLOAD_ROOT_FOLDER"),
    "CSV", as.character(sensorType),
    paste(sensorType, startDate, endDate, sep = ".")
  )

  # Get list of CSV files in the archive folder
  fileNames <- list.files(path = folderName, pattern = "\\.csv$", full.names = TRUE)
  files <- data.frame(Filename = fileNames) %>% dplyr::as_tibble()

  # Process daily data files
  dailyFiles <- files %>%
    dplyr::filter(grepl("Daily", Filename))

  # Extract sensor IDs from filenames for labeling
  sensorIDList <- purrr::map_chr(dailyFiles$Filename, \(x) stringr::str_extract(x, "(?<=_)\\d{6}(?=-)") %||% "")
  # Load daily CSV files and organize by sensor ID
  dailyData <- dailyFiles %>%
    dplyr::pull(Filename) %>%
    purrr::set_names(., purrr::map_chr(., \(x) stringr::str_extract(x, "(?<=_)\\d{6}(?=-)") %||% "")) %>%
    purrr::map(
      .f = \(x) {
        df <- read.csv(x)
        # Only process files with valid time_stamp column
        if ("time_stamp" %in% colnames(df)) (dplyr::as_tibble(df) %>% dplyr::mutate(startOfPeriod = time_stamp))
        else NULL
      }
    ) %>%
    purrr::compact() %>%
    # Add datasource ID to each dataset
    purrr::imap(~.x %>% dplyr::mutate(datasourceId = .y))

  # Load hourly data (same process as daily)
  hourlyFiles <- files %>%
    dplyr::filter(grepl("Hourly", Filename))

  # Load hourly CSV files and organize by sensor ID
  hourlyData <- hourlyFiles %>%
    dplyr::pull(Filename) %>%
    purrr::set_names(., purrr::map_chr(., \(x) stringr::str_extract(x, "(?<=_)\\d{6}(?=-)") %||% "")) %>%
    purrr::map(
      .f = \(x) {
        df <- read.csv(x)
        # Only process files with valid time_stamp column
        if ("time_stamp" %in% colnames(df)) (dplyr::as_tibble(df) %>%
                                               dplyr::mutate(startOfPeriod = time_stamp))
        else NULL
      }
    ) %>%
    purrr::compact() %>%
    # Add datasource ID to each dataset
    purrr::imap(~.x %>% dplyr::mutate(datasourceId = .y))

  return(list(
    Daily = dailyData,
    DailyLabel = dailyFiles,
    Hourly = hourlyData,
    HourlyLabel = hourlyFiles
  ))
}

#' Load Clarity Data from Archive
#'
#' Loads previously saved Clarity sensor data from CSV archive files.
#' Processes both daily and hourly data for the specified month.
#'
#' @param startDateOfMonth Date object representing the first day of the target month
#'
#' @return List containing Daily and Hourly data frames with corresponding file labels
#'
#' @details
#' Searches for CSV files in the Clarity archive folder and loads data
#' that contains valid datasourceId columns. Returns organized lists for
#' both daily and hourly aggregation periods.
#'
#' @examples
#' \dontrun{
#' data <- load_clarity_data_from_archive(as.Date("2025-01-01"))
#' daily_data <- data$Daily
#' hourly_data <- data$Hourly
#' }
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
load_clarity_data_from_archive <- function(startDateOfMonth) {
  # Calculate date range for the specified month  
  sensorType <- "Clarity"
  startDate <- lubridate::as_date(startDateOfMonth)
  endDate <- (startDate + months(1)) - lubridate::days(1)

  # Build folder path for archived CSV files
  folderName <- file.path(
    Sys.getenv("UPLOAD_ROOT_FOLDER"),
    "CSV", as.character(sensorType),
    paste(sensorType, startDate, endDate, sep = ".")
  )

  # Get list of CSV files in the archive folder
  fileNames <- list.files(path = folderName, pattern = "\\.csv$", full.names = TRUE)
  files <- data.frame(Filename = fileNames) %>% dplyr::as_tibble()

  # Process daily data files
  dailyFiles <- files %>%
    dplyr::filter(grepl("Daily", Filename))

  # Load daily CSV files (Clarity uses datasourceId instead of time_stamp)
  dailyData <- purrr::map(
    .x = dailyFiles %>% dplyr::pull(Filename),
    .f = \(x) {
      df <- read.csv(x)
      return(if ("datasourceId" %in% colnames(df)) dplyr::as_tibble(df) else NULL)
    }
  ) %>%
    purrr::compact()

  # Extract sensor IDs from data and use as names for the list
  sensorIDList <- purrr::map_chr(
    .x = dailyData,
    .f = \(x) { (x %>% dplyr::slice(1) %>% dplyr::pull("datasourceId") %>% as.character()) }
  )
  names(dailyData) <- sensorIDList

  # Process hourly data files (same process as daily)
  hourlyFiles <- files %>%
    dplyr::filter(grepl("Hourly", Filename))

  # Load hourly CSV files
  hourlyData <- purrr::map(
    .x = hourlyFiles %>% dplyr::pull(Filename),
    .f = \(x) {
      df <- read.csv(x)
      return(if ("datasourceId" %in% colnames(df)) dplyr::as_tibble(df) else NULL)
    }
  ) %>%
    purrr::compact()

  # Extract sensor IDs from data and use as names for the list
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

# Update - 11/Sep/2025
# - Temp. fix read_monitor_info_from_monitor_tracking() - columns range from A -> K instead of J.
# - Need to update through parameter.

# Update: 23 Oct 2025
# - Added comprehensive roxygen2 documentation
# - Added namespace prefixes for all non-base functions
# - Cleaned up rouge comments and code structure
# - Added concept tags for consistency
# - Added helpful inline comments to explain code chunks
# - Created get_reference_site_shortcodes() function for dynamic co-located site identification
# - Replaced hardcoded reference short codes with dynamic lookup from Excel file

