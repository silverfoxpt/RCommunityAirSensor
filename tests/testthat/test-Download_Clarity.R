library(httptest2)

#----------------------------------- Setup -----
# set temp directory
temp_test_root_folder <- withr::local_tempdir()

# copy extdata file into the temp folder to not muddle the original extdata
test_data_path <- system.file("extdata", package = "testPackage")
file.copy(from = test_data_path,
          to = temp_test_root_folder,
          recursive = TRUE,
          copy.mode = TRUE)

# get env
env_path <- system.file("extdata", ".RExtEnvTest", package = "testPackage")
envs <- read.dcf(env_path)

# mock function for fetching csv file
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

#----------------------------------- Testing ----
httptest2::.mockPaths(testthat::test_path("fixtures"))

withr::local_envvar(c(
  UPLOAD_ROOT_FOLDER = file.path(temp_test_root_folder, "extdata"),
  RECORDS_ROOT_FOLDER = file.path(temp_test_root_folder, "extdata"),
  CLARITYAPI = "REDACTED_CLARITY_SAMPLE_KEY"
))

local_mocked_bindings(
  clarity_fetch_csv_from_url_through_httr2 = mock_fetch_csv,
  .package = "testPackage"
)

httptest2::with_mock_api({
  current_date <- as.Date("2026-05-02")
  save_clarity_to_csv(current_date)
})

testthat::test_that("Clarity output files are created correctly", {
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

testthat::test_that("Clarity reference output files are created correctly", {
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

testthat::test_that("Clarity log file is created correctly", {
  log_file <- file.path(
    temp_test_root_folder,
    "extdata",
    "CSV",
    "Exports",
    "ClarityLog.csv"
  )

  expect_true(file.exists(log_file))

  log_data <- readr::read_csv(log_file, show_col_types = FALSE, name_repair = "unique_quiet")

  expect_equal(
    log_data,
    tibble::tibble(
      OriginDate = as.Date("2026-04-01"),
      Complete = "COMPLETED"
    )
  )
})
