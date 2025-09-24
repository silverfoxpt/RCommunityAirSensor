# Function to fetch and process AirNow API data
fetch_airnow_data <- function(api_key, start_date,end_date,
                              parameters = c("PM25", "NO2"),
                              bbox
) {
  # Input validation
  if (length(bbox) != 4) {
    stop("BBOX must contain exactly 4 values: west, south, east, north coordinates")
  }

  # Format parameters string
  parameters_str <- paste(parameters, collapse = ",")

  # Format bbox string
  bbox_str <- paste(bbox, collapse = ",")

  # Format dates to match API requirements (UTC)
  start_date_utc <- format(with_tz(start_date, "UTC"), "%Y-%m-%dT%H")
  end_date_utc <- format(with_tz(end_date, "UTC"), "%Y-%m-%dT%H")

  # Create and perform request
  response <- request("https://www.airnowapi.org/aq/data/") %>%
    req_url_query(
      startDate = start_date_utc,
      endDate = end_date_utc,
      parameters = parameters_str,
      BBOX = bbox_str,
      dataType = "B",
      format = "application/json",
      verbose = 1,
      monitorType = 0,
      includerawconcentrations = 1,
      API_KEY = api_key
    ) %>%
    req_retry(max_tries = 3) %>%  # Add retry capability
    req_error(is_error = function(resp) FALSE) %>%  # Handle errors manually
    req_perform()

  # Check response status
  if (resp_status(response) != 200) {
    stop("API request failed with status code: ", resp_status(response))
  }

  # Parse JSON response
  data_json <- resp_body_json(response) %>%
    bind_rows() %>%
    as_tibble() %>%
    mutate(
      # Convert UTC to datetime and add EST column
      UTC = ymd_hm(UTC),
      EST = with_tz(UTC, tzone = "America/New_York"),

      # Ensure numeric values
      Value = as.numeric(Value),
      RawConcentration = as.numeric(RawConcentration),
      AQI = as.numeric(AQI),
      Category = as.factor(Category)
    )

  return(data_json)
}
