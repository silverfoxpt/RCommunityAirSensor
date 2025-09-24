test_sensor_malfunction_result <- function(data, timestampName, valueName, start_time, current_time) {
  # Check if data contains the required columns
  if (!(timestampName %in% names(data))) {
    warning(paste("Timestamp column", timestampName, "not found in data. Returning empty result."))
    return(list(error = "Timestamp column missing"))
  }
  
  if (!(valueName %in% names(data))) {
    warning(paste("Value column", valueName, "not found in data. Returning empty result."))
    return(list(error = "Value column missing"))
  }
  
  # Transform data
  data <- data %>%
    dplyr::rename(`timestamp` := !!timestampName) %>%
    dplyr::rename(`value` := !!valueName) %>%
    dplyr::mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) %>%
    arrange(timestamp)  # Ensure data is sorted by timestamp
  
  # Type boolean check
  pm25 <- grepl("pm2.5", valueName, ignore.case = TRUE)
  temperature <- grepl("temperature", valueName, ignore.case = TRUE)
  humidity <- grepl("humidity", valueName, ignore.case = TRUE)
  
  # --- Time Thresholds ---
  prolonged_seq_hours <- 12
  #trend_duration_hours <- 48
  #max_timestamp_gap_hours <- 3
  trend_domination_threshold <- 0.95
  
  # --- Sensor Type Thresholds ---
  pm25_threshold <- 100
  temperature_threshold <- 140
  humidity_threshold <- 101
  
  # Decide which threshold applies based on the booleans
  spike_threshold <- case_when(
    pm25 ~ pm25_threshold,
    temperature ~ temperature_threshold,
    humidity ~ humidity_threshold,
    TRUE ~ 100  # Default fallback
  )
  
  # --- 1. Identify Data Points Exceeding Threshold ----
  spikes <- data %>%
    filter(value > spike_threshold)
  
  ## --- 1.1. Detect Duplicates Timestamps ----
  timestamp_diff_df <- data %>%
    mutate(
      time_diff = as.numeric(difftime(timestamp, lag(timestamp), units = "hours")),
      duplicate_flag = time_diff == 0
    )
  
  duplicate_timestamps <- timestamp_diff_df %>%
    filter(duplicate_flag) %>%
    select(timestamp, time_diff)
  
  ## --- 1.2. Detect Missing Timestamps ----
  # Convert UNIX timestamps (numeric) to POSIXct before rounding
  start_time <- as.POSIXct(start_time, origin = "1970-01-01", tz = "UTC")
  current_time <- as.POSIXct(current_time, origin = "1970-01-01", tz = "UTC") - 
    lubridate::hours(1) # Only for Purple Air, they are bonkers
  
  # Ensure rounding to full hours
  start_time <- ceiling_date(start_time, unit = "hour")
  current_time <- floor_date(current_time, unit = "hour")
  
  # Generate expected timestamps
  expected_timestamps <- tibble(timestamp = seq(from = start_time, to = current_time, by = 3600)) %>%
    dplyr::mutate(timestamp = as.POSIXct(timestamp, tz = "UTC"))
  
  # Identify missing timestamps by performing an anti-join
  missing_timestamps <- expected_timestamps %>%
    anti_join(data, by = "timestamp")
  
  # --- 2. Detect prolonged sequence of constant or NA values----
  flag_sequences <- function(values, timestamps, threshold) {
    values <- as.numeric(values)
    
    # Replace NA with -9999 so rle() can treat them as a distinct value
    filled_values <- tidyr::replace_na(values, -9999)
    rle_data <- rle(filled_values)
    
    # Build a tibble from the run-length encoding
    rle_df <- tibble::tibble(
      value       = rle_data$values,
      length      = rle_data$lengths,
      start_index = cumsum(c(1, head(rle_data$lengths, -1))),
      end_index   = cumsum(rle_data$lengths)
    ) %>%
      # We’re looking for runs where the value is 0, -9999 (NA), or repeated
      # and the time span of that run is longer than `threshold` hours
      dplyr::filter(
        (value == 0 | value == -9999 | length > 1),
        as.numeric(difftime(timestamps[end_index],
                            timestamps[start_index],
                            units = "hours")) > threshold
      ) %>%
      dplyr::mutate(
        start_time     = timestamps[start_index],
        end_time       = timestamps[end_index],
        duration_hours = as.numeric(difftime(end_time, start_time, units = "hours"))
      )
    
    return(rle_df)
  }
  
  # Only check prolonged sequences if sensor is PM2.5 or temperature
  prolonged_sequences <- if (pm25 || temperature) {
    flag_sequences(data$value, data$timestamp, prolonged_seq_hours)
  } else {
    # If humidity (or unknown sensor), skip or return an empty tibble
    tibble::tibble()
  }
  
  # --- 3. Check for Dominant Trend Over Entire Dataset (95% or More) ----
  # 1) Classify every reading as inc/dec/constant (again), but *don't* group/summarize by consecutive runs:
  trend_check_raw <- data %>%
    dplyr::mutate(
      diff = value - dplyr::lag(value),
      trend = dplyr::case_when(
        diff > 0 ~ "Increasing trend",
        diff < 0 ~ "Decreasing trend",
        TRUE     ~ "constant"
      )
    )
  
  # 2) Calculate how many rows fall into each trend category
  n_total <- nrow(trend_check_raw)
  
  dominant_trend_flag <- FALSE
  dominant_trend_type <- NA  # just to store whether it's "increasing" or "decreasing"
  
  if (n_total > 0) {
    trend_counts <- trend_check_raw %>%
      dplyr::filter(!is.na(trend)) %>%
      dplyr::group_by(trend) %>%
      dplyr::summarize(count = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(percent = count / n_total)
    
    # 3) Check if "increasing" or "decreasing" alone covers at least 95%
    #    (ignoring "constant" for the threshold check)
    dominating_trend <- trend_counts %>%
      dplyr::filter(trend %in% c("Increasing trend", "Decreasing trend"), percent >= trend_domination_threshold)
    
    if (nrow(dominating_trend) > 0) {
      dominant_trend_flag <- TRUE
      dominant_trend_type <- paste(dominating_trend$trend, collapse = ", ")
      # E.g. if both "increasing" and "decreasing" are each >= 95% (unlikely),
      # you'd see them comma-separated here.
    }
  }
  
  # Output results
  test <- list(
    error = NA,
    spikes = spikes,
    prolonged_sequences = prolonged_sequences,
    trends_flag = dominant_trend_flag,
    trends_type = dominant_trend_type,
    missing_timestamps = missing_timestamps,
    duplicate_timestamps = duplicate_timestamps
  )
}

test_full_timestamp_range <- function(data, timestampName, startDay, endDay, gap, returnDiff = FALSE) {
  # Convert input timestamps to POSIXct
  data <- data %>%
    mutate(!!timestampName := parse_date_time(.data[[timestampName]], 
                                              orders = c("ymd_HMS", "ymd")) %>% 
                              as.numeric())
  
  # Generate expected sequence
  full_range <- seq(from = force_tz(ymd(startDay), tzone = "UTC"), 
                    to = force_tz(ymd(endDay), tzone = "UTC"), 
                    by = gap)
  
  # Get actual timestamps
  actual_timestamps <- unique(pull(data, timestampName))
  
  # Find missing timestamps
  missing_timestamps <- setdiff(full_range, actual_timestamps)
  
  if (length(missing_timestamps) > 0) {
    if (returnDiff) { return(length(missing_timestamps)) }
    else { return(FALSE) }
  }
  
  if (returnDiff) { return(0) }
  else { return(TRUE) }
}

test_full_headers <- function(data, headers) {
  return(all(headers %in% colnames(data)))
}

check_missing_data <- function(sensor_index, field_name, hourly_data, countThisMonth) {
  if (is.null(hourly_data) || is.null(hourly_data[[sensor_index]]) || is.null(hourly_data[[sensor_index]][[field_name]])) {
    return("Null")
  }
  
  missing_count <- sum(is.na(hourly_data[[sensor_index]][[field_name]]))
  total_rows <- nrow(hourly_data[[sensor_index]])
  
  if (total_rows - missing_count == countThisMonth) {
    return("Full")
  }
  
  missing_pct <- ((countThisMonth - total_rows + missing_count) / countThisMonth * 100) %>%
    formatC(digits = 2, format = "f")
  
  return(paste0("Miss: ", missing_pct, " %"))
}

