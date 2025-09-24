get_single_sensor_data_custom <- function(sensor_id, neededFields, starting, ending, gap, api_key) {
  req <- glue("https://api.purpleair.com/v1/sensors/{sensor_id}/history") %>% 
    httr2::request() %>%
    httr2::req_headers("X-API-Key" = api_key) %>%
    httr2::req_url_query(
      fields = neededFields,
      start_timestamp = starting,
      end_timestamp = ending,
      average = gap
    ) %>%
    httr2::req_method("GET") 
  response <- httr2::req_perform(req) 
  resp_body <- httr2::resp_body_json(response)  #get response's body
  
  #TODO: Define procedure for dealing with this error
  if (length(resp_body$data) == 0) { #no data receive
    stop(paste("Error - history data retrieve empty: Sensor ID ", as.character(sensor_id), sep = ""))
  }
  
  sensor_fields <- resp_body$fields %>% as.vector() 
  sensor_data <- 
    purrr::map(
      .x = resp_body$data,
      .f = \(x) { 
        for (i in 1:length(x)) {
          if (is.null(x[[i]])) { 
            x[[i]] = NA 
          }
        } 
        names(x) <- sensor_fields
        return(x)
      }
    ) %>%
    dplyr::bind_rows() %>%
    as_tibble() %>%
    stats::setNames(sensor_fields) %>% # set columns' name
    dplyr::arrange(desc(time_stamp)) %>% # sort by timestamp
    #dplyr::mutate(time_stamp = as_datetime(as.double(time_stamp))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(time_stamp = format_timestamp(time_stamp)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sensor_index = sensor_id)
  
  return(sensor_data)
}

get_multi_sensors_status <- function(sensor_ids, neededFields, api_key) {
  req <- glue("https://api.purpleair.com/v1/sensors") %>% 
    httr2::request() %>%
    httr2::req_headers("X-API-Key" = api_key) %>%
    httr2::req_url_query(
      fields = neededFields,
      show_only = sensor_ids
    ) %>%
    httr2::req_method("GET") 
  response <- httr2::req_perform(req) 
  resp_body <- httr2::resp_body_json(response)  #get response's body
  
  #TODO: Define procedure for dealing with this error
  if (length(resp_body$data) == 0) { #no data receive
    stop(paste("Error - history data retrieve empty: Sensor ID ", as.character(sensor_id), sep = ""))
  }
  
  sensor_fields <- resp_body$fields %>% as.vector() 
  sensor_data <- 
    purrr::map(
      .x = resp_body$data,
      .f = \(x) { 
        for (i in 1:length(x)) {
          if (is.null(x[[i]])) { 
            x[[i]] = NA 
          }
        } 
        names(x) <- sensor_fields
        return(x)
      }
    ) %>%
    dplyr::bind_rows() %>%
    as_tibble() %>%
    stats::setNames(sensor_fields) %>% # set columns' name
  
  return(sensor_data)
}

save_aq_to_csv <- function(sensorId, tb, owner, shortcode, average, foldername, current_date) {
  start_of_current_month <- floor_date(current_date, unit = "month") 
  start_of_last_month <- floor_date(current_date - months(1), unit = "month") 
  
  start_last <- format(start_of_last_month, "%Y%m%d")
  start_current <- format(start_of_current_month - days(1), "%Y%m%d")
  
  filename <- 
    paste(
      start_last, "-", start_current, "_",
      "PA", "_", sensorId, "-", owner, "_", shortcode, "_",
      average, "_PM25-atm-alt-T-H", ".csv",
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