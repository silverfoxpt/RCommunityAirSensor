#' Calculate Previous Month Date Boundaries
#'
#' Calculates the start and end timestamps for the previous month relative
#' to a given date, with flexible formatting options.
#'
#' @param time POSIXct or character string of the reference date
#' @param tz Character string specifying timezone (default: "UTC")
#' @param iso8601 Logical indicating whether to format as ISO 8601 strings (default: TRUE)
#' @param date_only Logical indicating whether to return only date strings (default: FALSE)
#' @param nextMonth Logical indicating whether end should be current month start (default: FALSE)
#'
#' @return List with 'start' and 'end' elements containing formatted timestamps
#'
#' @details
#' When date_only=TRUE, returns "YYYY-MM-DD" format strings.
#' When iso8601=TRUE, returns ISO 8601 formatted timestamp strings.
#' Otherwise returns POSIXct objects.
#'
#' @examples
#' \dontrun{
#' # Get previous month boundaries for current date
#' bounds <- previous_month_bounds(Sys.Date())
#'
#' # Get date-only boundaries
#' date_bounds <- previous_month_bounds(Sys.Date(), date_only = TRUE)
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

#' Format Timestamp to ISO 8601 Standard
#'
#' Converts timestamps to standardized ISO 8601 format with proper timezone handling.
#' Handles both UTC (with 'Z' suffix) and other timezones (with offset).
#'
#' @param timestamp Character string or POSIXct timestamp to format
#' @param tz Character string specifying target timezone (default: "UTC")
#' @param original_format Character string specifying input format (default: "%Y-%m-%dT%H:%M:%SZ")
#'
#' @return Character string in ISO 8601 format
#'
#' @details
#' UTC timestamps use 'Z' suffix, other timezones use numeric offset (e.g., "-05:00").
#' Automatically inserts colon in timezone offset for proper ISO 8601 compliance.
#'
#' @examples
#' \dontrun{
#' formatted <- format_timestamp("2025-01-15T12:00:00Z")
#' formatted_est <- format_timestamp("2025-01-15T12:00:00Z", tz = "America/New_York")
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

#' Convert Timestamp String to POSIXct Object
#'
#' Parses timestamp strings into POSIXct objects with specified timezone.
#' Wrapper around as.POSIXct with standardized defaults for consistency.
#'
#' @param timestamp Character string timestamp to convert
#' @param tz Character string specifying timezone (default: "UTC")
#' @param original_format Character string specifying input format (default: "%Y-%m-%dT%H:%M:%SZ")
#'
#' @return POSIXct object in specified timezone
#'
#' @examples
#' \dontrun{
#' time_obj <- convert_to_time("2025-01-15T12:00:00Z")
#' time_est <- convert_to_time("2025-01-15T12:00:00Z", tz = "America/New_York")
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
convert_to_time <- function(timestamp, tz = "UTC", original_format = "%Y-%m-%dT%H:%M:%SZ") {
  return(as.POSIXct(timestamp, format = original_format, tz = tz))
}

#' Calculate Total Hours in Previous Month
#'
#' Calculates the total number of hours in the month preceding the given date.
#' Useful for time-based calculations and data validation.
#'
#' @param date_str Character string or Date object representing the reference date
#'
#' @return Numeric value representing total hours in previous month
#'
#' @examples
#' \dontrun{
#' hours <- get_last_month_hours("2025-02-15")  # Returns hours in January 2025
#' hours <- get_last_month_hours(Sys.Date())    # Hours in previous month
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
get_last_month_hours <- function(date_str) {
  date <- as.Date(date_str)
  last_month <- lubridate::floor_date(date, "month") - lubridate::days(1)  # Get last month's last day
  hours <- lubridate::day(last_month) * 24  # Convert days to hours
  return(hours)
}

# Example usage:
# Get the start and end boundaries of the month preceding "2025-02-21 15:30:00" in the "UTC" timezone.
# result <- previous_month_bounds("2025-02-21 15:30:00", tz = "UTC")
# print(result)

# Update: 23/10/2025
# - Added comprehensive roxygen2 documentation
# - Added lubridate:: namespace prefixes
# - Added concept tags for consistency