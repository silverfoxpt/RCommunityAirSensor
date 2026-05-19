#' Detect Sensor Malfunction Indicators in Clarity API Data
#'
#' Analyzes sensor data for multiple malfunction indicators including spikes, prolonged
#' constant values, missing data points, duplicate timestamps, and dominant trends.
#' Designed for Clarity API datasets with hourly air quality measurements.
#'
#' @details
#' **Run:**
#' 1. **Validation**: Checks that required columns exist in the data
#' 2. **Data transformation**: Renames columns and converts timestamps to POSIXct format
#' 3. **Spike detection**: Identifies data points exceeding sensor-specific thresholds
#' 4. **Timestamp validation**: Detects duplicate and missing timestamps
#' 5. **Sequence detection**: Identifies prolonged sequences of constant or zero values
#' 6. **Trend analysis**: Calculates if readings are dominated by increasing/decreasing trends
#'
#' **Data processing details:**
#' - Timestamps are expected in ISO 8601 format ("%Y-%m-%dT%H:%M:%SZ")
#' - Sensor type (PM2.5, NO2, temperature, humidity) is auto-detected from \code{valueName}
#' - Missing timestamps assume expected hourly data points
#' - Prolonged sequences include runs of zero values, NA values, or repeated values
#' - Trend analysis examines the full dataset for dominant monotonic patterns
#'
#' @param data Data frame containing sensor measurements with timestamp and value columns.
#' @param timestampName Character string naming the timestamp column in \code{data}.
#' @param valueName Character string naming the measurement column in \code{data}.
#' @param start_time POSIXct or character timestamp (ISO 8601 format) for expected data range start.
#' @param current_time POSIXct or character timestamp (ISO 8601 format) for expected data range end.
#' @param prolonged_seq_hours Numeric threshold (hours) for detecting prolonged constant sequences. Defaults to 12.
#' @param trend_domination_threshold Numeric threshold (0-1) for percentage of readings in a dominant trend. Defaults to 0.95.
#' @param pm25_threshold Numeric spike threshold for PM2.5 measurements. Defaults to 100.
#' @param no2_threshold Numeric spike threshold for NO2 measurements. Defaults to 100.
#' @param temperature_threshold Numeric spike threshold for temperature measurements. Defaults to 140.
#' @param humidity_threshold Numeric spike threshold for humidity measurements. Defaults to 101.
#'
#' @return A list with elements:
#'   \item{error}{Character string indicating validation errors, or NA if none.}
#'   \item{spikes}{Data frame of readings exceeding sensor thresholds.}
#'   \item{prolonged_sequences}{Data frame of prolonged constant/zero value runs.}
#'   \item{trends_flag}{Logical indicating if trend dominance was detected.}
#'   \item{trends_type}{Character string describing dominant trend direction, or NA.}
#'   \item{missing_timestamps}{Data frame of missing timestamps within the expected range.}
#'   \item{duplicate_timestamps}{Data frame of duplicate consecutive timestamps.}
#'
#' @section Error handling:
#' Returns \code{list(error = "Timestamp column missing")} or \code{list(error = "Value column missing")}
#' if required columns are not found. Otherwise returns \code{error = NA}.
#'
#' @examples
#' \dontrun{
#' # Analyze PM2.5 data from Clarity API
#' result <- test_sensor_malfunction_result_report_clarity(
#'   data = clarity_data,
#'   timestampName = "timestamp",
#'   valueName = "pm2_5_value",
#'   start_time = "2024-01-01T00:00:00Z",
#'   current_time = "2024-01-31T23:00:00Z"
#' )
#' }
#'
#' @export
#' @concept role:validation
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
test_sensor_malfunction_result_report_clarity <- function(
  data,
  timestampName,
  valueName,
  start_time,
  current_time,
  prolonged_seq_hours = 12,
  trend_domination_threshold = 0.95,
  pm25_threshold = 100,
  no2_threshold = 100,
  temperature_threshold = 140,
  humidity_threshold = 101
) {
  # Check if data contains the required columns
  if (!(timestampName %in% names(data))) {
    warning(paste("Timestamp column", timestampName, "not found in data. Returning empty result."))
    return(list(error = "Timestamp column missing"))
  }
  
  if (!(valueName %in% names(data))) {
    warning(paste("Value column", valueName, "not found in data. Returning empty result."))
    return(list(error = "Value column missing"))
  }
  
  data <- data %>%
    dplyr::rename(`timestamp` := !!timestampName) %>%
    dplyr::rename(`value` := !!valueName) %>%
    dplyr::mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) %>%
    dplyr::arrange(timestamp)
  
  # Type boolean check
  pm25 <- grepl("pm2_5", valueName, ignore.case = TRUE)
  no2 <- grepl("no2", valueName, ignore.case = TRUE)
  temperature <- grepl("temperature", valueName, ignore.case = TRUE)
  humidity <- grepl("humidity", valueName, ignore.case = TRUE)
  
  # Decide which threshold applies based on the sensor type
  spike_threshold <- dplyr::case_when(
    pm25 ~ pm25_threshold,
    no2 ~ no2_threshold,
    temperature ~ temperature_threshold,
    humidity ~ humidity_threshold,
    TRUE ~ 100  # Default fallback
  )
  
  # --- 1. Identify Data Points Exceeding Threshold ----
  spikes <- data %>%
    dplyr::filter(value > spike_threshold)
  
  ## --- 1.1. Detect Duplicates Timestamps ----
  timestamp_diff_df <- data %>%
    dplyr::mutate(
      time_diff = as.numeric(difftime(timestamp, dplyr::lag(timestamp), units = "hours")),
      duplicate_flag = time_diff == 0
    )
  
  duplicate_timestamps <- timestamp_diff_df %>%
    dplyr::filter(duplicate_flag) %>%
    dplyr::select(timestamp, time_diff)
  
  ## --- 1.2. Detect Missing Timestamps ----
  # Ensure rounding to full hours
  start_time <- lubridate::ceiling_date(start_time %>% as.POSIXct(format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), unit = "hour")
  current_time <- lubridate::floor_date(current_time %>% as.POSIXct(format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") - lubridate::hours(1), 
                             unit = "hour")
  
  # Generate expected timestamps
  expected_timestamps <- tibble::tibble(timestamp = seq(from = start_time, to = current_time, by = 3600)) %>%
    dplyr::mutate(timestamp = as.POSIXct(timestamp, tz = "UTC"))
  
  # Identify missing timestamps by performing an anti-join
  missing_timestamps <- expected_timestamps %>%
    dplyr::anti_join(data, by = "timestamp")
  
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
      # Identify runs where the value is 0, -9999 (NA), or repeated
      # and the time span of that run exceeds the threshold
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
  
  # Only check prolonged sequences if sensor is PM2.5, NO2, or temperature
  prolonged_sequences <- if (pm25 || temperature || no2) {
    flag_sequences(data$value, data$timestamp, prolonged_seq_hours)
  } else {
    # If humidity (or unknown sensor), skip or return an empty tibble
    tibble::tibble()
  }
  
  # --- 3. Check for Dominant Trend Over Entire Dataset ----
  # Classify every reading as inc/dec/constant (but don't group by consecutive runs)
  trend_check_raw <- data %>%
    dplyr::mutate(
      diff = value - dplyr::lag(value),
      trend = dplyr::case_when(
        diff > 0 ~ "Increasing trend",
        diff < 0 ~ "Decreasing trend",
        TRUE     ~ "constant"
      )
    )
  
  # Calculate trend category percentages
  n_total <- nrow(trend_check_raw)
  
  dominant_trend_flag <- FALSE
  dominant_trend_type <- NA
  
  if (n_total > 0) {
    trend_counts <- trend_check_raw %>%
      dplyr::filter(!is.na(trend)) %>%
      dplyr::group_by(trend) %>%
      dplyr::summarize(count = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(percent = count / n_total)
    
    # Check if increasing or decreasing trend dominates (>= trend_domination_threshold)
    dominating_trend <- trend_counts %>%
      dplyr::filter(trend %in% c("Increasing trend", "Decreasing trend"), percent >= trend_domination_threshold)
    
    if (nrow(dominating_trend) > 0) {
      dominant_trend_flag <- TRUE
      dominant_trend_type <- paste(dominating_trend$trend, collapse = ", ")
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

#' Check for Required Data Frame Columns
#'
#' Verifies that all required columns are present in a data frame.
#' Optionally prints debug messages about missing headers.
#'
#' @param data Data frame to check.
#' @param headers Character vector of required column names.
#' @param debug Logical. If TRUE, prints messages about missing headers. Defaults to FALSE.
#'
#' @return Logical. TRUE if all headers are present, FALSE otherwise.
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
test_full_headers_report_clarity <- function(data, headers, debug = F) {
  if (debug) {
    missing_headers <- headers[!headers %in% colnames(data)]
    
    if (length(missing_headers) > 0) {
      message("Debug: Missing headers -> ", paste(missing_headers, collapse = ", "))
    } else {
      message("Debug: All headers present.")
    }
  }
  
  return(all(headers %in% colnames(data)))
}

#' Check Data Completeness for a Sensor Field
#'
#' Evaluates the completeness of a sensor data field by comparing expected and
#' actual row counts and missing data percentages.
#'
#' @param sensor_index Integer index of the sensor in the hourly data list.
#' @param field_name Character string naming the field to check.
#' @param hourly_data List of data frames containing sensor measurements.
#' @param countThisMonth Integer count of expected measurements for the month.
#'
#' @return Character string indicating completeness status:
#'   \item{"Full"}{All expected measurements present.}
#'   \item{"Null"}{Data or field not found.}
#'   \item{"Miss: X.XX %"}{Percentage of missing data.}
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
check_missing_data_report_clarity <- function(sensor_index, field_name, hourly_data, countThisMonth) {
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