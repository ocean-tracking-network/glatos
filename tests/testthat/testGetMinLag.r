# Sample data for getMinLag.R
id <- 1:10
time <- c("2010/10/11 08:14:22", "2010/10/11 08:15:22", "2010/10/11 09:00:00", "2010/10/11 10:23:55", "2010/10/11 11:23:55", "2010/10/11 11:24:55", "2010/10/11 11:24:56", "2010/10/11 11:52:44", "2010/10/11 11:58:44", "2010/10/11 11:59:45")
time <- as.POSIXct(time, tz="UCT")
trans <- c(121, 151, 161, 151, 151, 161, 121, 151, 161, 121)
rec <- c(4, 2, 3, 2, 2, 2, 4, 2, 2, 4)
sampleMinLag <- data.frame(id=id, time=time, transmitter=trans, receiver=rec)
minLagData <- getMinLag(sampleMinLag, "sample")

# Expected results
minLagShouldBe <- c(11434, 7713, NA, 3600, 1729, 2029, 2089, 1729, 2029, 2089)

# Testing column that results from getMinLag using testthat library
test_that("min_lag column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(minLagData$min_lag, minLagShouldBe)
})
