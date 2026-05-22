test_that("previous_month_bounds returns UTC ISO 8601 month boundaries", {
  result <- previous_month_bounds("2025-03-15 12:34:56", tz = "UTC")

  expect_named(result, c("start", "end"))
  expect_equal(result$start, "2025-02-01T00:00:00Z")
  expect_equal(result$end, "2025-02-28T23:59:59Z")
})

test_that("previous_month_bounds supports date-only output and nextMonth", {
  result <- previous_month_bounds(
    "2025-03-15 12:34:56",
    tz = "UTC",
    date_only = TRUE,
    nextMonth = TRUE
  )

  expect_equal(result$start, "2025-02-01")
  expect_equal(result$end, "2025-03-01")
})

test_that("previous_month_bounds can return POSIXct values", {
  result <- previous_month_bounds(
    "2025-03-15 12:34:56",
    tz = "UTC",
    iso8601 = FALSE
  )

  expect_s3_class(result$start, "POSIXct")
  expect_s3_class(result$end, "POSIXct")
  expect_equal(format(result$start, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "2025-02-01T00:00:00Z")
  expect_equal(format(result$end, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "2025-02-28T23:59:59Z")
})

test_that("previous_month_bounds formats non-UTC timestamps with offsets", {
  result <- previous_month_bounds("2025-01-15 12:00:00", tz = "America/Los_Angeles")

  expect_equal(result$start, "2024-12-01T00:00:00-0800")
  expect_equal(result$end, "2024-12-31T23:59:59-0800")
})

test_that("format_timestamp returns UTC timestamps with a Z suffix", {
  result <- format_timestamp("2025-01-15T12:00:00Z")

  expect_equal(result, "2025-01-15T12:00:00Z")
})

test_that("format_timestamp formats non-UTC timestamps with a colon in the offset", {
  result <- format_timestamp("2025-01-15T12:00:00Z", tz = "America/New_York")

  expect_match(result, "^2025-01-15T[0-9]{2}:[0-9]{2}:[0-9]{2}-[0-9]{2}:[0-9]{2}$")
  expect_false(grepl("Z$", result))
})

test_that("convert_to_time parses timestamps into POSIXct objects", {
  result <- convert_to_time("2025-01-15 12:00:00", original_format = "%Y-%m-%d %H:%M:%S")

  expect_s3_class(result, "POSIXct")
  expect_equal(format(result, "%Y-%m-%dT%H:%M:%S", tz = "UTC"), "2025-01-15T12:00:00")
})

test_that("get_last_month_hours returns hours for normal and leap years", {
  expect_equal(get_last_month_hours("2025-03-15"), 672)
  expect_equal(get_last_month_hours("2024-03-15"), 696)
})