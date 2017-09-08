# To run:
# test_file("/Users/dinian/Desktop/glatos-git/R/testing/testDetectionEventFilter.R")

# Sample data for detectionEventFilter.R
loc <- c("TTB", "OSC", "PRS", "OSC", "OSC", "TTB", "PRS", "PRS", "PRS", "PRS")
anId <- rep(x=7, times=10)
time <- c("2010/10/11 11:11:11", "2010/10/11 23:12:53", "2010/10/12 14:23:12", "2010/10/13 07:24:52", "2010/10/13 13:17:13", "2010/10/13 20:20:20", "2010/10/14 17:43:22", "2010/10/15 18:19:23", "2010/10/16 10:56:56", "2010/10/16 22:11:33")
time <- as.POSIXct(time, tz="UCT")
lat <- c(43.392, 43.387, 41.571, 41.574, 41.576, 43.610, 43.612, 41.635, 41.644, 44.145)
long <- c(-83.993, -83.987, -83.618, -83.607, -83.611, -83.887, -83.861, -83.531, -83.534, -83.466)
sampleDEF <- data.frame(location=loc, animal=anId, time=time, latitude=lat, longitude=long)
sampleDEF$locationDBP <- as.character(sampleDEF$location)
defData <- detectionEventFilter(sampleDEF, "sample")

# Expected results
eventShouldBe <- 1:6
indShouldBe <- rep(x=7, times=6)
locShouldBe <- c("TTB", "OSC", "PRS", "OSC", "TTB", "PRS")
meanLatShouldBe <- c(43.392, 43.387, 41.571, 41.575, 43.610, 42.759)
meanLongShouldBe <- c(-83.993, -83.987, -83.618, -83.609, -83.887, -83.598)
firstDetShouldBe <- c("2010-10-11 11:11:11", "2010-10-11 23:12:53", "2010-10-12 14:23:12", "2010-10-13 07:24:52", "2010-10-13 20:20:20", "2010-10-14 17:43:22")
firstDetShouldBe <- as.POSIXct(firstDetShouldBe, tz="UCT")
lastDetShouldBe <- c("2010-10-11 11:11:11", "2010-10-11 23:12:53", "2010-10-12 14:23:12", "2010-10-13 13:17:13", "2010-10-13 20:20:20", "2010-10-16 22:11:33")
lastDetShouldBe <- as.POSIXct(lastDetShouldBe, tz="UCT")
numDetShouldBe <- c(1, 1, 1, 2, 1, 4)
resTimeShouldBe <- c(0, 0, 0, 21141, 0, 188891)

# Testing columns that result from detectionEventFilter using testthat library
test_that("Event column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(defData$Event, eventShouldBe)
})
test_that("Individual column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(defData$Individual, indShouldBe)
})
test_that("Data column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(defData$Location, locShouldBe)
})
test_that("MeanLatitude column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(defData$MeanLatiude, meanLatShouldBe)
})
test_that("MeanLongitude column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(defData$MeanLongitude, meanLongShouldBe)
})
test_that("FirstDetection column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(defData$FirstDetection, firstDetShouldBe)
})
test_that("LastDetection column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(defData$LastDetection, lastDetShouldBe)
})
test_that("NumDetections column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(defData$NumDetections, numDetShouldBe)
})
test_that("ResTime_sec column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(defData$ResTime_sec, resTimeShouldBe)
})

# Testing message
test_that("Proper message is seen", {
  # Check if expected and actual results are the same
  expect_message(defData <- detectionEventFilter(sampleDEF, "sample"),
                 "The event filter distilled 10 detections down to 6 distinct detection events.\n")
})