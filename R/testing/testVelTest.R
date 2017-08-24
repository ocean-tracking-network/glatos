# To run:
# test_file("/Users/dinian/Desktop/glatos-git/R/testing/testVelTest.R")

# Sample data for velTestFunc.R (without NA)
id <- 1:10
date <- c("2010/10/10 10:52:07", "2010/10/10 10:59:23", "2010/10/10 11:03:26", "2010/10/10 11:06:15", "2010/10/10 11:07:15", "2010/10/10 11:09:10", "2010/10/10 11:11:11", "2010/10/10 11:16:16", "2010/10/10 11:19:27", "2010/10/10 11:28:17")
date <- as.POSIXct(date, tz="UCT")
tr <- c(111, 111, 111, 111, 111, 111, 111, 111, 111,111)
rec <- c(11, 22, 33, 44, 55, 66, 77, 33, 44, 11)
lat <- c(44.6358, 44.6362, 44.6373, 44.6368, 44.6371, 44.6375, 44.6380, 44.6373, 44.6368, 44.6358)
long <- c(-63.5949, -63.5931, -63.5912, -63.5890, -63.5882, -63.5873, -63.5885, -63.5912, -63.5890, -63.5949)
sampleVel <- data.frame(id=id, time=date, transmitter=tr, receiver=rec, longitude=long, latitude=lat)
velData <- velTest(sampleVel, "sample", minVelValue = 1)

# Sample data for velTestFunc.R (with NA)
idNA <- 1:10
dateNA <- c("2010/10/10 10:01:00", "2010/10/10 10:02:00", "2010/10/10 10:03:00", "2010/10/10 10:04:00", "2010/10/10 10:05:00", "2010/10/10 10:06:00", "2010/10/10 10:07:00", "2010/10/10 10:08:00", "2010/10/10 10:09:00", "2010/10/10 10:10:00")
dateNA <- as.POSIXct(dateNA, tz="UCT")
trNA <- c(111, 111, 111, 111, NA, 111, 111, 111, 111,111)
recNA <- c(11, NA, 33, 44, 55, 66, 77, 33, 44, 11)
latNA <- c(44.6358, 44.6362, 44.6373, 44.6368, 44.6371, 44.6375, 44.6380, NA, 44.6368, 44.6358)
longNA <- c(-63.5949, -63.5931, NA, -63.5890, -63.5882, -63.5873, -63.5885, -63.5912, -63.5890, -63.5949)
sampleVelNA <- data.frame(id=idNA, time=dateNA, transmitter=trNA, receiver=recNA, longitude=longNA, latitude=latNA)
velDataNA <- velTest(sampleVelNA, "sample", minVelValue = 2.5)


# Expected results
minDistShouldBe <- c(149.37496, 149.37496, 182.93891, 71.63089, 71.63089, 84.05377, 110.15029, 182.93891, 182.93891, 480.43047)
minTimeShouldBe <- c(436, 243, 169, 60, 60, 115, 121, 191, 191, 530)
minVelShouldBe <- c(0.3426031, 0.6147118, 1.0824788, 1.1938482, 1.1938482, 0.7309023, 0.9103330, 0.9577953, 0.9577953, 0.9064726) #May be rounding, 'tolerance' of average difference is used in expect_equal to account for this
validShouldBe <- c(1, 1, 2, 2, 2, 1, 1, 1, 1, 1)
minDistNAShouldBe <- c(480.4305, NA, NA, 155.5808, NA, 110.1503, 110.1503, NA, 139.3310, 480.4305)
minTimeNAShouldBe <- c(180, NA, NA, 120, NA, 60, 60, NA, 60, 60)
minVelNAShouldBe <- c(2.669058, NA, NA, 1.296507, NA, 1.835838, 1.835838, NA, 2.322183, 8.007175)
validNAShouldBe <- c(2, 3, 3, 1, 3, 1, 1, 3, 1, 2)

# Testing columns that result from velTest using testthat library
test_that("min_dist column of data without NA gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(velData$min_dist, minDistShouldBe)
})
test_that("min_time column of data without NA gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(velData$min_time, minTimeShouldBe)
})
test_that("min_vel column of data without NA gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(velData$min_vel, minVelShouldBe, tolerance = 3.96e-08) #To stop any error due to rounding
})
test_that("valid column of data without NA gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(velData$velValid, validShouldBe)
})
test_that("min_dist column of data with NA gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(velDataNA$min_dist, minDistNAShouldBe, tolerance = 2.43e-05) #To stop any error due to rounding
})
test_that("min_time column of data with NA gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(velDataNA$min_time, minTimeNAShouldBe)
})
test_that("min_vel column of data with NA gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(velDataNA$min_vel, minVelNAShouldBe, tolerance = 3.33e-07) #To stop any error due to rounding
})
test_that("valid column of data with NA gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(velDataNA$velValid, validNAShouldBe)
})

# Test that some columns do not change
test_that("id column of data without NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(velData$id, id)
})
test_that("time column of data without NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(velData$time, date)
})
test_that("transmitter column of data without NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(velData$transmitter, tr)
})
test_that("receiver column of data without NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(velData$receiver, rec)
})
test_that("longitude column of data without NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(velData$longitude, long)
})
test_that("latitude column of data without NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(velData$latitude, lat)
})
test_that("id column of data with NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(velDataNA$id, idNA)
})
test_that("time column of data with NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(velDataNA$time, dateNA)
})
test_that("transmitter column of data with NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(velDataNA$transmitter, trNA)
})
test_that("receiver column of data with NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(velDataNA$receiver, recNA)
})
test_that("longitude column of data with NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(velDataNA$longitude, longNA)
})
test_that("latitude column of data with NA does not change", {
  # Check if expected and actual results are the same
  expect_equal(velDataNA$latitude, latNA)
})