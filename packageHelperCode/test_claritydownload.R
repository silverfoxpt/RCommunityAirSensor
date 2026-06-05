library(httptest2)

# set temp directory
temp_test_root_folder <- withr::local_tempdir()

# copy extdata file into the temp folder to not muddle the original extdata
test_data_path <- system.file("extdata", package = "testPackage")
file.copy(from = test_data_path,
          to = temp_test_root_folder,
          recursive = TRUE,
          copy.mode = TRUE)

# set env
env_path <- system.file("extdata", ".RExtEnvTest", package = "testPackage")
envs <- read.dcf(env_path)

# use with_envvar to enable temp. env. variables switching
withr::with_envvar(
  as.list(envs[1, ]),
  {
    root_folder <- file.path(test_data_path, "extdata")
    records_folder <- file.path(test_data_path, "extdata")
    clarity_api_key <- "REDACTED_CLARITY_SAMPLE_KEY"
  }
)

# set vars
current_date <- "2026-05-02"
aggregation_periods <- c("day", "hour")

# Get timestamp from start of month
calc_time <- previous_month_bounds(current_date)
calc_time_day_only <- previous_month_bounds(current_date, date_only = TRUE)

start_of_last_month <- calc_time_day_only$start
start_of_current_month <- calc_time_day_only$end

start_time_ISO <- calc_time$start
end_time_ISO <- calc_time$end

sitesInfo <- read_monitor_info_from_monitor_tracking("Clarity")

# Extract information
deviceId <- sitesInfo[['DeviceID']]
sensor_owners <- sitesInfo[['Owner']]
sensor_shortcode <- sitesInfo[['ShortCode']]
orgID <- sitesInfo[["OrgID"]]

uniqueOrgID <- unique(orgID)

httptest2::set_redactor(
  ~ httptest2::redact_headers(., "x-api-key")
)

# httptest2::capture_requests({
#   for (period in aggregation_periods) {
#     period_name <- ifelse(period == "day", "Daily",
#                           ifelse(period == "hour", "Hourly", tools::toTitleCase(period))
#     )
#     clarity_data <- purrr::map(
#       .x = uniqueOrgID,
#       .f = function(x, y) clarity_get_organization_data(x, clarity_api_key, period, start_time_ISO, end_time_ISO)
#     )
#
#   }
# })

httptest2::with_mock_api({
  for (period in aggregation_periods) {
    test_clarity_data <- purrr::map(
      .x = uniqueOrgID,
      .f = function(x) {
        clarity_get_organization_data(
          x,
          clarity_api_key,
          period,
          start_time_ISO,
          end_time_ISO
        )
      }
    )

    expect_type(test_clarity_data, "list")
    expect_length(test_clarity_data, length(uniqueOrgID))
    expect_s3_class(test_clarity_data[[1]], "tbl_df")

    expect_true(all(
      c("datasourceId", "sourceId", "sourceType") %in%
        names(test_clarity_data[[1]])
    ))
  }
})

