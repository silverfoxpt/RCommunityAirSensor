library(httptest2)

#----------------------------------- Setup -----
# set temp directory
temp_test_root_folder <- withr::local_tempdir()

# copy extdata file into the temp folder to not muddle the original extdata
test_data_path <- system.file("extdata", package = "AirSensorQAWorkflow")
file.copy(from = test_data_path,
          to = temp_test_root_folder,
          recursive = TRUE,
          copy.mode = TRUE)

# get env
env_path <- system.file("extdata", ".RExtEnvTest", package = "AirSensorQAWorkflow")
envs <- read.dcf(env_path)

expect_purpleAir_output_files <- function(folder, expected_n, filename_prefix) {
  expected_columns <- c(
    "time_stamp",
    "humidity",
    "temperature",
    "pm2.5_alt",
    "pm2.5_atm",
    "pm2.5_cf_1",
    "sensor_index"
  )

  output_folder <- file.path(
    folder,
    "PurpleAir.2026-04-01.2026-04-30"
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
    grepl("(atm-alt-T-H)\\.csv$", output_filenames)
  ))

  purrr::walk(output_files, function(file) {
    data <- readr::read_csv(
      file,
      show_col_types = FALSE,
      name_repair = "unique_quiet"
    )

    # one of the file has no data, it is expected
    if (grepl("150464", basename(file))) {
      expect_equal(
        data,
        tibble::tibble(
          x = "Error: Empty data! Please recheck!"
        )
      )
    } else {
      expect_true(all(expected_columns %in% names(data)))
    }
  })
}

#----------------------------------- Testing ----
httptest2::.mockPaths(testthat::test_path("fixtures"))

withr::local_envvar(c(
  UPLOAD_ROOT_FOLDER = file.path(temp_test_root_folder, "extdata"),
  RECORDS_ROOT_FOLDER = file.path(temp_test_root_folder, "extdata"),
  PURPLEAPI = "REDACTED_PURPLEAIR_SAMPLE_KEY"
))

httptest2::with_mock_api({
  current_date <- as.Date("2026-05-02")
  save_purpleAir_to_csv(current_date, is_testing = TRUE)
})

testthat::test_that("Purple Air output files are created correctly", {
  clarity_folder <- file.path(
    temp_test_root_folder,
    "extdata",
    "CSV",
    "PurpleAir"
  )

  expect_purpleAir_output_files(
    folder = clarity_folder,
    expected_n = 16,
    filename_prefix = "20260401"
  )
})

testthat::test_that("Purple Air log file is created correctly", {
  log_file <- file.path(
    temp_test_root_folder,
    "extdata",
    "CSV",
    "Exports",
    "PurpleAirLog.csv"
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
