library(testthat)
library(withr)
library(dplyr)
library(tibble)
library(mockery)
library(lubridate)

test_that("create_new_folder creates nested directories under given root", {
  td <- local_tempdir()
  newpath <- file.path("sub", "inner")

  # call the function with explicit root_path
  create_new_folder(newpath, root_path = td)

  expect_true(dir.exists(file.path(td, newpath)))
})

test_that("read_reference_info_from_monitor_tracking parses and normalizes Excel data", {
  # Prepare fake data that readxl::read_xlsx would return
  fake <- tibble(
    `Datasource ID` = c("SRC1", "SRC2"),
    `Short Code` = c("SC1", "SC2"),
    `Site Name` = c("Site A", "Site B"),
    `Collect PM2.5` = c(TRUE, FALSE),
    `Collect NO2` = c(FALSE, TRUE)
  )

  stubbed <- function(...) fake

  # Stub readxl::read_xlsx only for the duration of the call
  stub(read_reference_info_from_monitor_tracking, 'readxl::read_xlsx', stubbed)

  res <- read_reference_info_from_monitor_tracking()

  expect_s3_class(res, "tbl_df")
  required_cols <- c("DatasourceID", "DeviceID", "ShortCode", "SiteName", "CollectPM25", "CollectNO2", "Type", "Subtype")
  expect_named(res, required_cols, ignore.order = TRUE)
  expect_equal(res$DatasourceID, c("SRC1", "SRC2"))
  expect_true(all(res$Type == "Reference"))
})

test_that("get_reference_site_shortcodes returns unique non-empty short codes", {
  # Stub the upstream read function to control the reference data
  fake_ref <- tibble(ShortCode = c("A", "B", "A", NA, ""))
  # Because get_reference_site_shortcodes calls read_reference_info_from_monitor_tracking
  stub(get_reference_site_shortcodes, 'read_reference_info_from_monitor_tracking', function() fake_ref)

  res <- get_reference_site_shortcodes()
  expect_type(res, "character")
  expect_true(all(res %in% c("A", "B")))
  expect_length(res, 2)
})

test_that("read_monitor_info_from_monitor_tracking handles Clarity and PurpleAir branches", {
  # Prepare a fake MonitorStatus sheet
  fake <- tibble(
    Label = c("CN-1", "PA-1"),
    `API ID` = c("D100", "123456"),
    `Dashboard/API Organization ID` = c("ORG0001", ""),
    `Location Short Code` = c("REF1", "NOTREF"),
    `Deployed Site Location` = c("Central Park", "Somewhere")
  )

  stub(read_monitor_info_from_monitor_tracking, 'readxl::read_xlsx', function(...) fake)
  stub(read_monitor_info_from_monitor_tracking, 'get_reference_site_shortcodes', function() c("REF1"))

  clarity <- read_monitor_info_from_monitor_tracking("Clarity")
  expect_s3_class(clarity, "tbl_df")
  expect_true(all(clarity$Type == "Clarity"))
  expect_true(all(clarity$Subtype %in% c("Co-located", "Park", "Non-park")))

  # For PurpleAir branch, prepare different fake sheet (re-stub)
  fake_pa <- tibble(
    Label = c("PA-1"),
    `API ID` = c("123456"),
    `Dashboard/API Organization ID` = c("ORG0002"),
    `Location Short Code` = c("REF1"),
    `Deployed Site Location` = c("Park West"),
    `Data Sharing Setting` = c("public")
  )
  stub(read_monitor_info_from_monitor_tracking, 'readxl::read_xlsx', function(...) fake_pa)
  stub(read_monitor_info_from_monitor_tracking, 'get_reference_site_shortcodes', function() c("REF1"))

  pa <- read_monitor_info_from_monitor_tracking("PurpleAir")
  expect_s3_class(pa, "tbl_df")
  expect_true(all(pa$Type == "PurpleAir"))
})

test_that("load_purple_air_data_from_archive reads daily and hourly CSVs and extracts IDs", {
  td <- local_tempdir()
  with_envvar(new = c(UPLOAD_ROOT_FOLDER = td), code = {
    start <- as.Date("2025-01-01")
    end <- (start + months(1)) - 1
    folder <- file.path(td, "CSV", "PurpleAir", paste0("PurpleAir.", start, ".", end))
    dir.create(folder, recursive = TRUE)

    # create Daily CSV with time_stamp and sensor id in filename
    daily_file <- file.path(folder, "PurpleAir_Daily_123456-2025.csv")
    write.csv(data.frame(time_stamp = as.POSIXct("2025-01-01 00:00:00"), val = 1), daily_file, row.names = FALSE)

    # create Hourly CSV
    hourly_file <- file.path(folder, "PurpleAir_Hourly_123456-2025.csv")
    write.csv(data.frame(time_stamp = as.POSIXct("2025-01-01 01:00:00"), val = 2), hourly_file, row.names = FALSE)

    res <- load_purple_air_data_from_archive(start)

    expect_true("123456" %in% names(res$Daily))
    expect_true("123456" %in% names(res$Hourly))
    expect_s3_class(res$Daily$`123456`, "tbl_df")
    expect_s3_class(res$Hourly$`123456`, "tbl_df")
  })
})

test_that("load_clarity_data_from_archive reads clarity CSVs and names lists by datasourceId", {
  td <- local_tempdir()
  with_envvar(new = c(UPLOAD_ROOT_FOLDER = td), code = {
    start <- as.Date("2025-02-01")
    end <- (start + months(1)) - 1
    folder <- file.path(td, "CSV", "Clarity", paste0("Clarity.", start, ".", end))
    dir.create(folder, recursive = TRUE)

    # create Daily CSV with datasourceId column
    daily_file <- file.path(folder, "Clarity_Daily_1.csv")
    write.csv(data.frame(datasourceId = "D200", x = 1), daily_file, row.names = FALSE)

    # create Hourly CSV
    hourly_file <- file.path(folder, "Clarity_Hourly_1.csv")
    write.csv(data.frame(datasourceId = "D200", x = 2), hourly_file, row.names = FALSE)

    res <- load_clarity_data_from_archive(start)

    expect_true("D200" %in% names(res$Daily))
    expect_true("D200" %in% names(res$Hourly))
    expect_s3_class(res$Daily$D200, "tbl_df")
  })
})
