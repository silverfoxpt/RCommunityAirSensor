# Deprecated
# get_clarity_metadata <- function(clarityKey) {
#   req <- httr2::request("https://clarity-data-api.clarity.io/v1/devices")
#   req <- req %>%
#     httr2::req_headers("X-API-Key" = clarityKey) %>%   #set headers for API_KEY
#     httr2::req_method("GET") # HTTP Method
#
#   response <- httr2::req_perform(req) %>% print()
#   resp_body <- httr2::resp_body_json(response)  #get response's body
#
#   clarity_sensor_meta <-
#     do.call(rbind, lapply(resp_body, unlist)) %>%
#     as_tibble() %>%
#     dplyr::mutate("location.type" = NULL) %>%
#     dplyr::rename(
#       longitude = "location.coordinates1",
#       latitude = "location.coordinates2"
#     )
#   return(clarity_sensor_meta)
# }

# Deprecated
# get_clarity_data_custom_deprecated <- function(deviceID, clarityKey, averageTime, startT, endT) {
#   req <- httr2::request("https://clarity-data-api.clarity.io/v1/measurements")
#   req <- req %>%
#     httr2::req_headers("X-API-Key" = clarityKey) %>%   #set headers for API_KEY
#     httr2::req_method("GET") %>%                            #set HTTP method
#     httr2::req_url_query(
#       code = deviceID,
#       average = averageTime,
#       startTime = startT,
#       endTime = endT,
#       limit = 1000
#     )
#
#   response <- httr2::req_perform(req)
#   resp_body <- httr2::resp_body_json(response)  #get response's body
#
#   clarity_sensor_data <-
#     do.call(rbind, lapply(resp_body, unlist)) %>%
#     as_tibble()
#   filtered_columns <- clarity_sensor_data %>%
#     select(starts_with("characteristics.")) %>%
#     colnames()
#   clarity_sensor_data <- clarity_sensor_data %>%
#     dplyr::rename_with(
#       ~str_remove(., "characteristics\\."),
#       .cols = all_of(filtered_columns)
#     )
#   return(clarity_sensor_data)
# }

# Clarity file to CSV save file ####
save_clarity_aq_to_csv <- function(sensorId, tb, owner, shortcode, average, foldername, current_date) {
  start_of_current_month <- floor_date(current_date, unit = "month")
  start_of_last_month <- floor_date(current_date - months(1), unit = "month")

  start_last <- format(start_of_last_month, "%Y%m%d")
  start_current <- format(start_of_current_month - days(1), "%Y%m%d")

  filename <-
    paste(
      start_last, "-", start_current, "_",
      "CN", "_", sensorId, "-", owner, "_", shortcode, "_",
      average, ".csv",
      sep = ""
    )

  #TODO: Define procedure for NULL return
  if (is.null(tb)) {
    errorMessage <- c("Error: Empty data! Please recheck!")
    write.csv(errorMessage, file = file.path(foldername, filename), row.names = F)
    return()
  }
  write.csv(tb, file = file.path(foldername, filename), row.names = F)
}

save_clarity_aq_reference_to_csv <- function(tb, average, foldername, current_date) {
  start_of_current_month <- floor_date(current_date, unit = "month")
  start_of_last_month <- floor_date(current_date - months(1), unit = "month")

  start_last <- format(start_of_last_month, "%y%m%d")
  start_current <- format(start_of_current_month - days(1), "%y%m%d")

  filename <-
    paste(
      start_last, "-", start_current, "_", tb %>% dplyr::slice(1) %>% dplyr::pull(datasourceId), "_",
      average, ".csv",
      sep = ""
    )

  #TODO: Define procedure for NULL return
  if (is.null(tb)) {
    errorMessage <- c("Error: Empty data! Please recheck!")
    write.csv(errorMessage, file = file.path(foldername, filename), row.names = F)
    return()
  }
  write.csv(tb, file = file.path(foldername, filename), row.names = F)
}

# Functions for Single-Device-Download ####
post_for_report_clarity_data_custom <- function(deviceID, organization, clarityKey, averageTime, startT, endT) {
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

get_clarity_data_custom_v2 <- function(deviceID, orgID, clarityKey, averageTime, startT, endT) {
  fetchReport <- post_for_report_clarity_data_custom(deviceID, orgID,
                                             clarityKey, averageTime,
                                             startT, endT)
  print("Fetched report")

  getReport <- get_for_report_clarity_data_custom(fetchReport$reportId, clarityKey)
  print("Got report")

  if (getReport$reportStatus == "succeeded") {
    mainData <- fetch_data_clarity_from_url(getReport$urls[[1]])
    print("Got CSV file")
    return(mainData)
  } else { #do something here?
    return("EmptyData")
  }
}

#%_________________________________________%
# Helper function
fetch_data_clarity_from_url <- function(clarityURL) {
  data <- readr::read_csv(clarityURL)
  return(data)
}

get_for_report_clarity_data_custom <- function(reportId, clarityKey) {
  for (c in 1:50) {
    req <- glue("https://clarity-data-api.clarity.io/v2/report-requests/{reportId}") %>%
      httr2::request() %>%
      httr2::req_headers("x-api-key" = clarityKey) %>%   #set headers for API_KEY
      httr2::req_method("GET")                            #set HTTP method

    response <- httr2::req_perform(req)
    resp_body <- httr2::resp_body_json(response)  #get response's body

    if (resp_body[['reportStatus']] != "in-progress") {
      return(resp_body)
    }
    Sys.sleep(10)
  }
}

#Split data to different tibbles based on datasource IDs
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

#%-----------------------------------------%
# Functions for All-Organization-Device-Download ####
post_for_report_clarity_data_custom_ORG <- function(organization, clarityKey, averageTime, startT, endT) {
  body <- list(
    org = organization,
    outputFrequency = averageTime,
    report = "datasource-measurements",
    allDatasources = T,
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

get_clarity_data_custom_v2_ORG <- function(orgID, clarityKey, averageTime, startT, endT) {
  fetchReport <- post_for_report_clarity_data_custom_ORG(orgID,
                                                     clarityKey, averageTime,
                                                     startT, endT)
  print("Fetched report")

  getReport <- get_for_report_clarity_data_custom(fetchReport$reportId, clarityKey)
  print("Got report")

  if (getReport$reportStatus == "succeeded") {
    mainData <- fetch_data_clarity_from_url(getReport$urls[[1]])
    print("Got CSV file")
    return(mainData)
  } else { #do something here?
    return("EmptyData")
  }

  return(getReport)
}

## Functions for status summary ####
get_for_status_clarity_custom_ORG <- function(organization, clarityKey) {
  req <- httr2::request("https://clarity-data-api.clarity.io/v2/devices/nodes/status-summary")
  req <- req %>%
    httr2::req_headers("x-api-key" = clarityKey) %>%   #set headers for API_KEY
    httr2::req_method("GET") %>%                            #set HTTP method
    httr2::req_url_query(
      org = organization
    )

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)  #get response's body

  return(resp_body)
}
