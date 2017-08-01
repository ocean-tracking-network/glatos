# To run:
# test_file("/Users/dinian/Desktop/glatos-git/R/testing/testEventPlot.R")

# Sample data for detectionEventFilter.R
event <- 1:6
ind <- rep(x=7, times=6)
loc <- c("TTB", "OSC", "PRS", "OSC", "TTB", "PRS")
meanLat <- c(43.392, 43.387, 41.571, 41.575, 43.610, 42.759)
meanLong <- c(-83.993, -83.987, -83.618, -83.609, -83.887, -83.598)
firstDet <- c("2010/10/11 11:11:11", "2010/10/11 23:12:53", "2010/10/12 14:23:12", "2010/10/13 07:24:52", "2010/10/13 20:20:20", "2010/10/14 17:43:22")
firstDet <- as.POSIXct(firstDet, tz="UCT")
lastDet <- c("2010/10/11 11:11:11", "2010/10/11 23:12:53", "2010/10/12 14:23:12", "2010/10/13 13:17:13", "2010/10/13 20:20:20", "2010/10/16 22:11:33")
lastDet <- as.POSIXct(lastDet, tz="UCT")
numDet <- c(1, 1, 1, 2, 1, 4)
resTime <- c(0, 0, 0, 21141, 0, 188891)
sampleEP <- data.frame(Event=event, Individual=ind, Location=loc, MeanLatitude=meanLat, MeanLongitude=meanLong, FirstDetection=firstDet, LastDetection=lastDet, NumDetections=numDet, ResTime_sec=resTime)
sampleEP$Location <- as.character(sampleEP$Location)
 eventPlot(sampleEP)

# Expected results


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