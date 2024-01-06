context("Check min_lag")

# Sample data for min_lag
time <- c(
  "2010/10/11 08:14:22", "2010/10/11 08:15:22", "2010/10/11 09:00:00",
  "2010/10/11 10:23:55", "2010/10/11 11:23:55", "2010/10/11 11:24:55",
  "2010/10/11 11:24:56", "2010/10/11 11:52:44", "2010/10/11 11:58:44",
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
minLagData <- min_lag(sampleMinLag)

# Expected results
minLagShouldBe <- c(11434, 7713, NA, 3600, 1729, 2029, 2089, 1729, 2029, 2089)

# Testing column that results from getMinLag using testthat library
test_that("min_lag column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(minLagData$min_lag, minLagShouldBe)
})
