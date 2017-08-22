# To run:
# test_file("/Users/dinian/Desktop/glatos-git/R/testing/testNumDetectionsAndIntervalTest.R")

# Sample data for numDetectionsAndIntervalTest.R (without NA)
id <- 1:15
time <- c("2010/10/11 11:11:11", "2010/10/11 11:12:52", "2010/10/11 11:13:11", "2010/10/11 12:52:13", "2010/10/12 07:23:52", "2010/10/12 11:12:52", "2010/10/12 12:16:43", "2010/10/12 12:52:13", "2010/10/12 12:52:14", "2010/10/12 14:16:44", "2010/10/12 19:23:37", "2010/10/12 19:24:37", "2010/10/12 21:24:36", "2010/10/13 11:11:11", "2010/10/13 15:12:23")
time <- as.POSIXct(time, tz="UCT")
trans <- c(11, 12, 11, 11, 12, 12, 12, 11, 11, 12, 11, 11, 11, 11, 12)
rec <- c(21, 22, 21, 22, 21, 22, 22, 22, 21, 22, 22, 22, 22, 21, 22)
sampleInterval <- data.frame(id=id, time=time, transmitter=trans, receiver=rec)
intervalData <- numIntervalTest(sampleInterval, "sample")

# Sample data for numDetectionsAndIntervalTest.R (with NA)
id <- 1:15
time <- c("2010/10/11 11:11:11", "2010/10/11 11:12:52", "2010/10/11 11:13:11", "2010/10/11 12:52:13", "2010/10/12 07:23:52", "2010/10/12 11:12:52", "2010/10/12 12:16:43", "2010/10/12 12:52:13", "2010/10/12 12:52:14", "2010/10/12 14:16:44", "2010/10/12 19:23:37", "2010/10/12 19:24:37", "2010/10/12 21:24:36", "2010/10/13 11:11:11", "2010/10/13 15:12:23")
time <- as.POSIXct(time, tz="UCT")
trans <- c(11, NA, 11, 11, 12, 12, 12, NA, 11, 12, 11, 11, 11, 11, 12)
rec <- c(21, 22, NA, 22, 21, 22, 22, 22, 21, 22, 22, 22, 22, NA, 22)
sampleIntervalNA <- data.frame(id=id, time=time, transmitter=trans, receiver=rec)
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

# # Test message
# test_that("Proper message is seen", {
#   # Check if expected and actual results are the same
#   expect_message(intervalData <- numIntervalTest(sampleInterval, "sample"),
#                  "The filter identified 6 (40%) of 15 detections as invalid using the number of detections and interval ratio test.\n")
# })