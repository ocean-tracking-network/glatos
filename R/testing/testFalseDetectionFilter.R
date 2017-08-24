# To run:
# test_file("/Users/dinian/Desktop/glatos-git/R/testing/testFalseDetectionFilter.R")

# Sample data for falseDetectionFilter.R
id <- 1:10
date <- c("2010/10/11 08:14:22", "2010/10/11 08:15:22", "2010/10/11 09:00:00", "2010/10/11 10:23:55", "2010/10/11 11:23:55", "2010/10/11 11:24:55", "2010/10/11 11:24:56", "2010/10/11 11:52:44", "2010/10/11 11:58:44", "2010/10/11 11:59:45")
date <- as.POSIXct(time, tz="UCT")
tr <- c(121, 151, 161, 151, 151, 161, 121, 151, 161, 121)
rec <- c(4, 2, 3, 2, 2, 2, 4, 2, 2, 4)
sampleFD <- data.frame(id=id, time=time, transmitter=tr, receiver=rec)
fdData <- falseDetectionFilter(sampleFD, "sample", tf=3600)

# Expected results
minLagShouldBe <- c(11434, 7713, NA, 3600, 1729, 2029, 2089, 1729, 2029, 2089)
validShouldBe <- c(0, 0, 0, 1, 1, 1, 1, 1, 1, 1)

# Test columns that result from falseDetectionFilter using testthat library
test_that("min_lag column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(fdData$min_lag, minLagShouldBe)
})
test_that("passedFilter column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(fdData$passedFilter, validShouldBe)
})

# Test that some columns do not change
test_that("id column of data without NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(fdData$id, id)
})
test_that("time column of data without NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(fdData$time, date)
})
test_that("transmitter column of data without NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(fdData$transmitter, tr)
})
test_that("receiver column of data without NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(fdData$receiver, rec)
})

# Test message
# test_that("Proper message is seen", {
#   # Check if expected and actual results are the same
#   expect_message(fdData <- falseDetectionFilter(sampleFD, "sample", tf=3600), 
#                  "The filter identified 3 (30%) of 10 detections as potentially false.\n")
# })