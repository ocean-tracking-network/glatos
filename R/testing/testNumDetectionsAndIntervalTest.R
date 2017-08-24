# To run:
# test_file("/Users/dinian/Desktop/glatos-git/R/testing/testNumDetectionsAndIntervalTest.R")

# Sample data for numDetectionsAndIntervalTest.R (without NA)
id <- 1:15
date <- c("2010/10/11 11:11:11", "2010/10/11 11:12:52", "2010/10/11 11:13:11", "2010/10/11 12:52:13", "2010/10/12 07:23:52", "2010/10/12 11:12:52", "2010/10/12 12:16:43", "2010/10/12 12:52:13", "2010/10/12 12:52:14", "2010/10/12 14:16:44", "2010/10/12 19:23:37", "2010/10/12 19:24:37", "2010/10/12 21:24:36", "2010/10/13 11:11:11", "2010/10/13 15:12:23")
date <- as.POSIXct(date, tz="UCT")
tr <- c(11, 12, 11, 11, 12, 12, 12, 11, 11, 12, 11, 11, 11, 11, 12)
rec <- c(21, 22, 21, 22, 21, 22, 22, 22, 21, 22, 22, 22, 22, 21, 22)
sampleInterval <- data.frame(id=id, time = date, transmitter=tr, receiver=rec)
intervalData <- numIntervalTest(sampleInterval, "sample")

# Sample data for numDetectionsAndIntervalTest.R (with NA)
idNA <- 1:15
dateNA <- c("2010/10/11 11:11:11", "2010/10/11 11:12:52", "2010/10/11 11:13:11", "2010/10/11 12:52:13", "2010/10/12 07:23:52", "2010/10/12 11:12:52", "2010/10/12 12:16:43", "2010/10/12 12:52:13", "2010/10/12 12:52:14", "2010/10/12 14:16:44", "2010/10/12 19:23:37", "2010/10/12 19:24:37", "2010/10/12 21:24:36", "2010/10/13 11:11:11", "2010/10/13 15:12:23")
dateNA <- as.POSIXct(dateNA, tz="UCT")
trNA <- c(11, NA, 11, 11, 12, 12, 12, NA, 11, 12, 11, 11, 11, 11, 12)
recNA <- c(21, 22, NA, 22, 21, 22, 22, 22, 21, 22, 22, 22, 22, NA, 22)
sampleIntervalNA <- data.frame(id=idNA, time=dateNA, transmitter=trNA, receiver=recNA)
intervalDataNA <- numIntervalTest(sampleIntervalNA, "sample")

# Expected results
validShouldBe <- c(1, 2, 1, 1, 2, 2, 2, 1, 1, 2, 1, 1, 1, 1, 2)
validNAShouldBe <- c(2, 3, 3, 1, 2, 1, 1, 3, 2, 1, 1, 1, 1, 3, 1)

# Test column that results from numDetectionsAndIntervalTest using testthat library
test_that("valid column gives expected result (without NA)", {
  # Check if expected and actual results are the same
  expect_equal(intervalData$numIntervalValid, validShouldBe)
})
test_that("valid column gives expected result (with NA)", {
  # Check if expected and actual results are the same
  expect_equal(intervalDataNA$numIntervalValid, validNAShouldBe)
})

# Test that some columns do not change
test_that("id column of data without NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(intervalData$id, id)
})
test_that("time column of data without NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(intervalData$time, date)
})
test_that("transmitter column of data without NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(intervalData$transmitter, tr)
})
test_that("receiver column of data without NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(intervalData$receiver, rec)
})
test_that("id column of data with NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(intervalDataNA$id, idNA)
})
test_that("time column of data with NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(intervalDataNA$time, dateNA)
})
test_that("transmitter column of data with NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(intervalDataNA$transmitter, trNA)
})
test_that("receiver column of data with NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(intervalDataNA$receiver, recNA)
})


# # Test message
# test_that("Proper message is seen", {
#   # Check if expected and actual results are the same
#   expect_message(intervalData <- numIntervalTest(sampleInterval, "sample"),
#                  "The filter identified 6 (40%) of 15 detections as invalid using the number of detections and interval ratio test.\n")
# })