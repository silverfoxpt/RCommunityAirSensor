library(httptest2)

# set temp directory
temp_test_root_folder <- withr::local_tempdir()

# copy extdata file into the temp folder to not muddle the original extdata
test_data_path <- system.file("extdata", package = "AirSensorQAWorkflow")
file.copy(from = test_data_path,
          to = temp_test_root_folder,
          recursive = TRUE,
          copy.mode = TRUE)

# set env
env_path <- system.file("extdata", ".RExtEnvTest", package = "AirSensorQAWorkflow")
envs <- read.dcf(env_path)

# use with_envvar to enable temp. env. variables switching
withr::with_envvar(
  c(
    UPLOAD_ROOT_FOLDER = file.path(temp_test_root_folder, "extdata"),
    RECORDS_ROOT_FOLDER = file.path(temp_test_root_folder, "extdata"),
    CLARITYAPI = "REDACTED_CLARITY_SAMPLE_KEY"
  ),
  {
    sitesInfo <- read_monitor_info_from_monitor_tracking("Clarity")
  }
)

root_folder <- file.path(temp_test_root_folder, "extdata")
records_folder <- file.path(temp_test_root_folder, "extdata")
clarity_api_key <- "REDACTED_CLARITY_SAMPLE_KEY"

# set vars
current_date <- as.Date("2026-05-02")
aggregation_periods <- c("day", "hour")

# Get timestamp from start of month
calc_time <- previous_month_bounds(current_date)
calc_time_day_only <- previous_month_bounds(current_date, date_only = TRUE)

start_of_last_month <- calc_time_day_only$start
start_of_current_month <- calc_time_day_only$end

start_time_ISO <- calc_time$start
end_time_ISO <- calc_time$end

# Extract information
deviceId <- sitesInfo[['DeviceID']]
sensor_owners <- sitesInfo[['Owner']]
sensor_shortcode <- sitesInfo[['ShortCode']]
orgID <- sitesInfo[["OrgID"]]

uniqueOrgID <- unique(orgID)

# Create folder name
newFolderName <- paste("Clarity",
                       as.character(start_of_last_month),
                       as.character(start_of_current_month),
                       sep = ".")

# Create folder paths
folderPath <- file.path("CSV", "Clarity", newFolderName)
referenceFolderPath <- file.path("CSV", "Clarity-Reference", newFolderName)

# Create folders
create_new_folder(folderPath, root_path = root_folder)
create_new_folder(referenceFolderPath, root_path = root_folder)

# Check if Log has already been collected
log_file_full_path <- file.path(root_folder, "CSV", "Exports", "ClarityLog.csv")
logfile <- read.csv(log_file_full_path) %>% dplyr::as_tibble()

httptest2::set_redactor(
  ~ httptest2::redact_headers(., "x-api-key")
)

# Capture first time
# api_key_real <- "<redacted_key>"
# httptest2::capture_requests({
#   for (period in aggregation_periods) {
#     period_name <- ifelse(period == "day", "Daily",
#                           ifelse(period == "hour", "Hourly", tools::toTitleCase(period))
#     )
#     clarity_data <- purrr::map(
#       .x = uniqueOrgID,
#       .f = function(x, y) clarity_get_organization_data(x, api_key_real, period, start_time_ISO, end_time_ISO)
#     )
#
#   }
# })

# Mock tests
mock_fetch_csv <- function(url, reportId) {
  expect_type(url, "character")
  expect_true(length(url) == 1)

  sensor_data <- readr::read_csv(
    testthat::test_path(
      file.path(
        "fixtures", "combined-measurements-export-prd.s3.amazonaws.com", "historical",
        reportId,
        paste0(reportId, ".csv")
      )
    ),
    show_col_types = FALSE,
    name_repair = "unique_quiet"
  )
  return(sensor_data)
}

httptest2::with_mock_api({
  for (period in aggregation_periods) {
    period_name <- ifelse(period == "day", "Daily",
                          ifelse(period == "hour", "Hourly", tools::toTitleCase(period)))

    # Get measurements by OrgID
    clarity_data <- purrr::map(
      .x = uniqueOrgID,
      .f = function(x, y) clarity_get_organization_data(x, clarity_api_key, period, start_time_ISO, end_time_ISO,
                                                        fetch_csv_func = mock_fetch_csv)
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::rowwise() %>%
      dplyr::mutate(startOfPeriod = format_timestamp(startOfPeriod)) %>%
      dplyr::mutate(endOfPeriod = format_timestamp(endOfPeriod)) %>%
      dplyr::ungroup()

    # Get reference sites
    clarity_reference_data <- split_clarity_reference_data_by_datasource(clarity_data) %>%
      purrr::list_flatten()
    clarity_reference_data <- clarity_reference_data[!duplicated(names(clarity_reference_data))]

    # Split CSV file to distinct datasource IDs
    clarity_data <- split_clarity_data_by_datasource(clarity_data, deviceId) %>%
      purrr::list_flatten()

    # Save sensor data to file
    purrr::pwalk(
      .l = list(deviceId, clarity_data, sensor_owners, sensor_shortcode),
      .f = save_clarity_aq_to_csv,
      average = period_name,
      foldername = file.path(root_folder, folderPath),
      current_date = current_date
    )

    # Save reference data to file
    purrr::walk(
      .x = clarity_reference_data,
      .f = save_clarity_aq_reference_to_csv,
      average = period_name,
      foldername = file.path(root_folder, referenceFolderPath),
      current_date = current_date
    )
  }
})

# all da tests for Clarity files
expect_clarity_output_files <- function(folder, expected_n, filename_prefix) {
  expected_columns <- c(
    "datasourceId",
    "sourceId",
    "sourceType",
    "outputFrequency",
    "startOfPeriod",
    "endOfPeriod"
  )

  output_folder <- file.path(
    folder,
    "Clarity.2026-04-01.2026-04-30"
  )

  expect_true(dir.exists(output_folder))

  output_files <- list.files(
    output_folder,
    pattern = "\\.csv$",
    full.names = TRUE
  )

  expect_length(output_files, expected_n)

  output_filenames <- basename(output_files)

  expect_true(all(startsWith(output_filenames, filename_prefix)))

  expect_true(all(
    grepl("(Daily|Hourly)\\.csv$", output_filenames)
  ))

  purrr::walk(output_files, function(file) {
    data <- readr::read_csv(file, show_col_types = FALSE, name_repair = "unique_quiet")

    expect_true(all(expected_columns %in% names(data)))
  })
}

test_that("Clarity output files are created correctly", {
  clarity_folder <- file.path(
    temp_test_root_folder,
    "extdata",
    "CSV",
    "Clarity"
  )

  expect_clarity_output_files(
    folder = clarity_folder,
    expected_n = 46,
    filename_prefix = "20260401"
  )
})

test_that("Clarity reference output files are created correctly", {
  reference_folder <- file.path(
    temp_test_root_folder,
    "extdata",
    "CSV",
    "Clarity-Reference"
  )

  expect_clarity_output_files(
    folder = reference_folder,
    expected_n = 4,
    filename_prefix = "260401"
  )
})

# Write to log
write.table(
  data.frame(
    OriginDate = c(start_of_last_month),
    Complete = c('COMPLETED')
  ),
    file = log_file_full_path,
  sep = ",",
  col.names = FALSE,
  row.names = FALSE,
  append = TRUE
)

test_that("Clarity log file is created correctly", {
  log_file <- file.path(
    temp_test_root_folder,
    "extdata",
    "CSV",
    "Exports",
    "ClarityLog.csv"
  )

  expect_true(file.exists(log_file))

  log_data <- readr::read_csv(
    log_file,
    show_col_types = FALSE,
    name_repair = "unique_quiet"
  )

  expect_named(
    log_data,
    c("OriginDate", "Complete")
  )

  expect_equal(
    nrow(log_data),
    1
  )

  expect_equal(
    log_data,
    tibble::tibble(
      OriginDate = as.Date("2026-04-01"),
      Complete = "COMPLETED"
    )
  )
})

