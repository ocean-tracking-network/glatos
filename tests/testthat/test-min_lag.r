time <- c(
  "2010/10/11 08:14:22",
  "2010/10/11 08:15:22",
  "2010/10/11 09:00:00",
  "2010/10/11 10:23:55",
  "2010/10/11 11:23:55",
  "2010/10/11 11:24:55",
  "2010/10/11 11:24:56",
  "2010/10/11 11:52:44",
  "2010/10/11 11:58:44",
  "2010/10/11 11:59:45"
)

time <- as.POSIXct(time, tz = "UTC")
trans1 <- c(121, 151, 161, 151, 151, 161, 121, 151, 161, 121)
trans2 <- c(121, 151, 161, 151, 151, 161, 121, 151, 161, 121)
rec <- c(4, 2, 3, 2, 2, 2, 4, 2, 2, 4)

sampleMinLag <- data.frame(
  detection_timestamp_utc = time,
  transmitter_codespace = trans1,
  transmitter_id = trans2,
  receiver_sn = rec
)


# Testing column that results from getMinLag using testthat library
test_that("min_lag column gives expected result", {
  minLagData <- min_lag(sampleMinLag)

  # Expected results
  # Check if expected and actual results are the same
  minLagShouldBe <- c(11434, 7713, NA, 3600, 1729, 2029, 2089, 1729, 2029, 2089)
  expect_equal(minLagData$min_lag, minLagShouldBe)

  # Check that original data is untouched
  expect_equal(
    minLagData[, 1:(ncol(minLagData) - 1)],
    sampleMinLag
  )
})


# Test that min_lag returns input S3 class
test_that("data.frame returns correct classes and types", {
  minLag_df <- min_lag(sampleMinLag)
  # Expected classes
  expect_s3_class(minLag_df, "data.frame")
  expect_type(minLag_df$min_lag, "double")
})


test_that("data.table returns correct classes and types", {
  minLag_dt <- min_lag(
    data.table::as.data.table(sampleMinLag)
  )

  # Expected classes
  expect_s3_class(minLag_dt, "data.table")
  expect_s3_class(minLag_dt, "data.frame")
  expect_type(minLag_dt$min_lag, "double")
})


test_that("tibble returns correct classes and types", {
  minLag_tbl <- min_lag(
    dplyr::as_tibble(sampleMinLag)
  )

  # Expected classes
  expect_s3_class(minLag_tbl, "tbl_df")
  expect_s3_class(minLag_tbl, "tbl")
  expect_s3_class(minLag_tbl, "data.frame")
  expect_type(minLag_tbl$min_lag, "double")
})
