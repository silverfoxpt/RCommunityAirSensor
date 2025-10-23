# ---
# CLARITY API HELPER FUNCTIONS
# Helper functions for downloading, processing, and saving Clarity air quality data
# ---

# CSV export functionalities ----

#' Save Clarity sensor data to CSV file
#'
#' Exports Clarity air quality sensor data to a standardized CSV file format.
#' Handles null data by creating error files and uses standardized naming convention.
#'
#' @param sensorId Character string of the sensor/device ID
#' @param tb Data frame containing the sensor data to save
#' @param owner Character string identifying the sensor owner
#' @param shortcode Character string with the site short code
#' @param average Character string specifying aggregation period ("Daily", "Hourly")
#' @param foldername Character string path to output directory
#' @param current_date Date object used to determine file naming dates
#'
#' @return NULL.
#'
#' @details
#' File naming convention: YYYYMMDD-YYYYMMDD_CN_sensorId-owner_shortcode_average.csv
#' Where CN stands for Clarity and dates represent the data period.
#'
#' @examples
#' \dontrun{
#' save_clarity_aq_to_csv(
#'   sensorId = "CLARITY123",
#'   tb = sensor_data,
#'   owner = "Smith",
#'   shortcode = "SITE01",
#'   average = "Daily",
#'   foldername = "/path/to/output",
#'   current_date = Sys.Date()
#' )
#' }
#'
#' @keywords internal
save_clarity_aq_to_csv <- function(sensorId, tb, owner, shortcode, average, foldername, current_date) {
  start_of_current_month <- lubridate::floor_date(current_date, unit = "month")
  start_of_last_month <- lubridate::floor_date(current_date - months(1), unit = "month")

  start_last <- format(start_of_last_month, "%Y%m%d")
  start_current <- format(start_of_current_month - days(1), "%Y%m%d")

  filename <-
    paste(
      start_last, "-", start_current, "_",
      "CN", "_", sensorId, "-", owner, "_", shortcode, "_",
      average, ".csv",
      sep = ""
    )

  # Handle NULL data by creating error file
  if (is.null(tb)) {
    errorMessage <- data.frame(Error = "Empty data! Please recheck sensor configuration.")
    write.csv(errorMessage, file = file.path(foldername, filename), row.names = FALSE)
    return(invisible(NULL))
  }
  write.csv(tb, file = file.path(foldername, filename), row.names = FALSE)
}

#' Save Clarity reference station data to CSV file
#'
#' Exports Clarity reference station data to a standardized CSV file format.
#' Uses 2-digit year format and extracts datasource ID from the data.
#'
#' @param tb Data frame containing reference station data with datasourceId column
#' @param average Character string specifying aggregation period ("Daily", "Hourly")
#' @param foldername Character string path to output directory
#' @param current_date Date object used to determine file naming dates
#'
#' @return NULL (function called for side effects)
#'
#' @details
#' File naming convention: YYMMDD-YYMMDD_datasourceId_average.csv
#' Where datasourceId is extracted from the first row of the data.
#'
#' @examples
#' \dontrun{
#' save_clarity_aq_reference_to_csv(
#'   tb = reference_data,
#'   average = "Hourly",
#'   foldername = "/path/to/reference",
#'   current_date = Sys.Date()
#' )
#' }
#'
#' @keywords internal
save_clarity_aq_reference_to_csv <- function(tb, average, foldername, current_date) {
  start_of_current_month <- lubridate::floor_date(current_date, unit = "month")
  start_of_last_month <- lubridate::floor_date(current_date - months(1), unit = "month")

  start_last <- format(start_of_last_month, "%y%m%d")
  start_current <- format(start_of_current_month - lubridate::days(1), "%y%m%d")

  filename <-
    paste(
      start_last, "-", start_current, "_", tb %>% dplyr::slice(1) %>% dplyr::pull(datasourceId), "_",
      average, ".csv",
      sep = ""
    )

  # Handle NULL data by creating error file
  if (is.null(tb)) {
    errorMessage <- data.frame(Error = "Empty reference data! Please recheck station configuration.")
    write.csv(errorMessage, file = file.path(foldername, filename), row.names = FALSE)
    return(invisible(NULL))
  }
  write.csv(tb, file = file.path(foldername, filename), row.names = FALSE)
}

# API Clarity request - single ----

#' Create Clarity API report request for single device
#'
#' Submits a POST request to the Clarity API v2 to generate a data report
#' for a specific device within an organization.
#'
#' @param deviceID Character string of the device ID to request data for
#' @param organization Character string of the organization ID
#' @param clarityKey Character string containing the Clarity API key
#' @param averageTime Character string specifying aggregation ("day", "hour")
#' @param startT Character string with ISO start timestamp
#' @param endT Character string with ISO end timestamp
#'
#' @return List containing the API response with reportId
#'
#' @keywords internal
clarity_post_single_device_report <- function(deviceID, organization, clarityKey, averageTime, startT, endT) {
  body <- list(
    org = organization,
    outputFrequency = averageTime,
    report = "datasource-measurements",
    startTime = startT,
    endTime = endT,
    datasourceIds = list(deviceID)
  )

  req <- httr2::request("https://clarity-data-api.clarity.io/v2/report-requests")
  req <- req %>%
    httr2::req_headers("x-api-key" = clarityKey) %>%   #set headers for API_KEY
    httr2::req_method("POST") %>%                            #set HTTP method
    httr2::req_body_json(body)

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)  #get response's body

  return(resp_body)
}

#' Get Clarity data for single device
#'
#' Retrieves air quality data for a specific Clarity device by creating a report
#' request and polling until completion, then downloading the CSV data.
#'
#' @param deviceID Character string of the device ID
#' @param orgID Character string of the organization ID
#' @param clarityKey Character string containing the Clarity API key
#' @param averageTime Character string specifying aggregation ("day", "hour")
#' @param startT Character string with ISO start timestamp
#' @param endT Character string with ISO end timestamp
#'
#' @return Data frame with sensor measurements or "EmptyData" string if failed
#'
#' @examples
#' \dontrun{
#' data <- clarity_get_single_device_data(
#'   deviceID = "CLARITY123",
#'   orgID = "ORG456",
#'   clarityKey = "api_key",
#'   averageTime = "hour",
#'   startT = "2025-01-01T00:00:00Z",
#'   endT = "2025-01-31T23:59:59Z"
#' )
#' }
#'
#' @export
clarity_get_single_device_data <- function(deviceID, orgID, clarityKey, averageTime, startT, endT) {
  fetchReport <- clarity_post_single_device_report(deviceID, orgID,
                                             clarityKey, averageTime,
                                             startT, endT)
  print("Fetched report")

  getReport <- clarity_poll_report_status(fetchReport$reportId, clarityKey)
  print("Got report")

  if (getReport$reportStatus == "succeeded") {
    mainData <- clarity_fetch_csv_from_url(getReport$urls[[1]])
    print("Got CSV file")
    return(mainData)
  } else { #do something here?
    return("EmptyData")
  }
}

# Data processing functions ----

#' Fetch Clarity data from URL
#'
#' Downloads CSV data from a Clarity API-provided URL.
#'
#' @param clarityURL Character string URL to the CSV data file
#'
#' @return Data frame with the downloaded data
#'
#' @keywords internal
clarity_fetch_csv_from_url <- function(clarityURL) {
  data <- readr::read_csv(clarityURL)
  return(data)
}

#' Poll Clarity API for report completion
#'
#' Polls the Clarity API report endpoint until the report is ready or timeout.
#' Waits up to 500 seconds (50 iterations × 10 seconds) for completion.
#'
#' @param reportId Character string of the report ID to check
#' @param clarityKey Character string containing the Clarity API key
#'
#' @return List containing the report status and download URLs when ready
#'
#' @keywords internal
clarity_poll_report_status <- function(reportId, clarityKey) {
  for (c in 1:50) {
    req <- glue("https://clarity-data-api.clarity.io/v2/report-requests/{reportId}") %>%
      httr2::request() %>%
      httr2::req_headers("x-api-key" = clarityKey) %>%
      httr2::req_method("GET")

    response <- httr2::req_perform(req)
    resp_body <- httr2::resp_body_json(response)  #get response's body

    if (resp_body[['reportStatus']] != "in-progress") {
      return(resp_body)
    }
    Sys.sleep(10)
  }
}

#' Split Clarity data by datasource IDs
#'
#' Separates a combined Clarity dataset into individual data frames
#' for each specified datasource ID (sensor).
#'
#' @param data Data frame containing combined Clarity sensor data
#' @param datasourceIds Character vector of datasource IDs to split by
#'
#' @return List of data frames, one per datasource ID. Empty data returns error frame.
#'
#' @details
#' If no data is found for a datasource ID, returns a data frame with error message.
#' This helps identify missing sensors during processing.
#'
#' @examples
#' \dontrun{
#' sensor_list <- split_clarity_data_by_datasource(
#'   data = combined_data,
#'   datasourceIds = c("SENSOR001", "SENSOR002")
#' )
#' }
#'
#' @export
split_clarity_data_by_datasource <- function(data, datasourceIds) {
  result <- purrr::map(
    .x = datasourceIds,
    .f = function(x) {
      filteredData <- data %>% dplyr::filter(datasourceId == x)

      if (nrow(filteredData) == 0) {
        filteredData <- data.frame("Error: No data found!")  # Empty error frame
      }

      return(filteredData)
    }
  )

  return(result)
}

#' Split Clarity reference station data by datasource
#'
#' Filters and separates reference station data from combined Clarity dataset.
#' Only includes reference stations that start with "R" and are in the tracking file.
#'
#' @param data Data frame containing combined Clarity data with sensor and reference stations
#'
#' @return Named list of data frames, split by reference station datasource ID
#'
#' @details
#' - Filters for sourceId starting with "R" (reference stations)
#' - Cross-references with monitor tracking file for valid reference stations
#' - Returns error frame if no reference data found
#'
#' @examples
#' \dontrun{
#' ref_stations <- split_clarity_reference_data_by_datasource(combined_data)
#' }
#'
#' @export
split_clarity_reference_data_by_datasource <- function(data) {
  referenceSiteInfo <- read_reference_info_from_monitor_tracking()
  filteredData <- data %>%
    dplyr::filter(grepl("^R", sourceId)) %>% # Only take reference sites
    dplyr::filter(datasourceId %in% (referenceSiteInfo %>% pull("DatasourceID")))

  if (nrow(filteredData) == 0) {
    filteredData <- data.frame("Error: No data found!")
  }

  return(split(filteredData, filteredData$datasourceId))
}

# API Clarity request - Organization ----

#' Create Clarity API report request for all organization devices
#'
#' Submits a POST request to get data for all devices within an organization.
#' Uses allDatasources=TRUE to retrieve data from all sensors in the org.
#'
#' @param organization Character string of the organization ID
#' @param clarityKey Character string containing the Clarity API key
#' @param averageTime Character string specifying aggregation ("day", "hour")
#' @param startT Character string with ISO start timestamp
#' @param endT Character string with ISO end timestamp
#'
#' @return List containing the API response with reportId
#'
#' @keywords internal
clarity_post_organization_report <- function(organization, clarityKey, averageTime, startT, endT) {
  body <- list(
    org = organization,
    outputFrequency = averageTime,
    report = "datasource-measurements",
    allDatasources = TRUE,
    startTime = startT,
    endTime = endT
  )

  req <- httr2::request("https://clarity-data-api.clarity.io/v2/report-requests")
  req <- req %>%
    httr2::req_headers("x-api-key" = clarityKey) %>%   #set headers for API_KEY
    httr2::req_method("POST") %>%                            #set HTTP method
    httr2::req_body_json(body)

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)  #get response's body

  return(resp_body)
}

#' Get Clarity data for all devices in organization
#'
#' Retrieves air quality data for all devices within a Clarity organization.
#' Creates report request, polls for completion, then downloads the data.
#'
#' @param orgID Character string of the organization ID
#' @param clarityKey Character string containing the Clarity API key
#' @param averageTime Character string specifying aggregation ("day", "hour")
#' @param startT Character string with ISO start timestamp
#' @param endT Character string with ISO end timestamp
#'
#' @return Data frame with all organization sensor data or "EmptyData" if failed
#'
#' @details
#' This is the primary function used by save_clarity_to_csv() to download
#' data for entire organizations rather than individual sensors, which reduce API calls.
#'
#' @examples
#' \dontrun{
#' org_data <- get_clarity_data_custom_v2_ORG(
#'   orgID = "ORG123",
#'   clarityKey = "api_key",
#'   averageTime = "day",
#'   startT = "2025-01-01T00:00:00Z",
#'   endT = "2025-01-31T23:59:59Z"
#' )
#' }
#'
#' @export
clarity_get_organization_data <- function(orgID, clarityKey, averageTime, startT, endT) {
  fetchReport <- clarity_post_organization_report(orgID,
                                                     clarityKey, averageTime,
                                                     startT, endT)
  print("Fetched report")

  getReport <- clarity_poll_report_status(fetchReport$reportId, clarityKey)
  print("Got report")

  if (getReport$reportStatus == "succeeded") {
    mainData <- clarity_fetch_csv_from_url(getReport$urls[[1]])
    print("Got CSV file")
    return(mainData)
  } else {
    warning("Report generation failed. Check API key and parameters.")
    return("EmptyData")
  }
}

# Status monitoring ----

#' Get Clarity device status summary for organization
#'
#' Retrieves the current operational status of all devices within a Clarity organization.
#' Used for monitoring sensor health and connectivity.
#'
#' @param organization Character string of the organization ID
#' @param clarityKey Character string containing the Clarity API key
#'
#' @return List containing device status information from the API
#'
#' @details
#' Returns status information including device connectivity, last data timestamp,
#' and operational health metrics for all sensors in the organization.
#'
#' @examples
#' \dontrun{
#' status <- clarity_get_organization_status(
#'   organization = "ORG123",
#'   clarityKey = "api_key"
#' )
#' }
#'
#' @export
clarity_get_organization_status <- function(organization, clarityKey) {
  req <- httr2::request("https://clarity-data-api.clarity.io/v2/devices/nodes/status-summary")
  req <- req %>%
    httr2::req_headers("x-api-key" = clarityKey) %>%
    httr2::req_method("GET") %>%
    httr2::req_url_query(
      org = organization
    )

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)  #get response's body

  return(resp_body)
}

# Development notes: Finish testing: 30 Sep 2025
# Updates
# - Change functions naming convention
# - Add roxygen documentatation
# - Clean up code
# - Removed deprecated functions (Clarity v1)
# - Removed outdated comments
# - Update error messages
