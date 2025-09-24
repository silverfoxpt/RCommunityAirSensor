previous_month_bounds <- function(time, tz = "UTC", iso8601 = TRUE, date_only = FALSE, nextMonth = FALSE) {
  # Convert the input time to a POSIXct object in the specified timezone
  time <- as.POSIXct(time, tz = tz)
  
  # Determine the first moment of the current month at midnight
  current_month_start <- as.POSIXct(format(time, "%Y-%m-01 00:00:00"), tz = tz)
  
  # The last moment of the previous month is one second before the current month begins
  previous_month_end <- current_month_start - 1
  
  # The first moment of the previous month is at midnight on its first day
  previous_month_start <- as.POSIXct(
    paste0(format(previous_month_end, "%Y-%m-01"), " 00:00:00"),
    tz = tz
  )
  
  # Choose the appropriate end timestamp based on the nextMonth parameter
  chosen_end <- if (nextMonth) current_month_start else previous_month_end
  
  # If only the day is required, return in "YYYY-MM-DD" format
  if (date_only) {
    start_str <- format(previous_month_start, "%Y-%m-%d")
    end_str   <- format(chosen_end, "%Y-%m-%d")
    return(list(start = start_str, end = end_str))
  }
  
  # Otherwise, follow the existing ISO 8601 formatting logic
  if (iso8601) {
    if (tz == "UTC") {
      # Format using 'Z' for UTC timezone
      start_str <- format(previous_month_start, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      end_str   <- format(chosen_end, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    } else {
      # Format dates as ISO 8601 with numeric timezone offset
      start_str <- format(previous_month_start, "%Y-%m-%dT%H:%M:%S%z", tz = tz)
      end_str   <- format(chosen_end, "%Y-%m-%dT%H:%M:%S%z", tz = tz)
    }
    return(list(start = start_str, end = end_str))
  } else {
    return(list(start = previous_month_start, end = chosen_end))
  }
}

format_timestamp <- function(timestamp, tz = "UTC", original_format = "%Y-%m-%dT%H:%M:%SZ") {
  # Convert the input to a POSIXct object using the specified timezone
  timestamp <- as.POSIXct(timestamp, format = original_format, tz = tz)
  
  if (tz == "UTC") {
    # Format in ISO 8601 with 'Z' for UTC
    formatted <- format(timestamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  } else {
    # Format in ISO 8601 with numeric timezone offset
    formatted <- format(timestamp, "%Y-%m-%dT%H:%M:%S%z", tz = tz)
    # Insert a colon into the timezone offset (e.g., "-0500" becomes "-05:00")
    formatted <- sub("([+-]\\d{2})(\\d{2})$", "\\1:\\2", formatted)
  }
  
  return(formatted)
}

convert_to_time <- function(timestamp, tz = "UTC", original_format = "%Y-%m-%dT%H:%M:%SZ") {
  return(as.POSIXct(timestamp, format = original_format, tz = tz))
}

get_last_month_hours <- function(date_str) {
  date <- as.Date(date_str)
  last_month <- floor_date(date, "month") - days(1)  # Get last month's last day
  hours <- day(last_month) * 24  # Convert days to hours
  return(hours)
}

# Example usage:
# Get the start and end boundaries of the month preceding "2025-02-21 15:30:00" in the "UTC" timezone.
# result <- previous_month_bounds("2025-02-21 15:30:00", tz = "UTC")
# print(result)