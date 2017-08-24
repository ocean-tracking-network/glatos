# To run:
# test_file("/Users/dinian/Desktop/glatos-git/R/testing/testEfficiencyTest.R")

# Sample data for efficiencyTestFunc.R (without NA)
id <- 1:10
d <- c("2010/10/10 10:52:07", "2010/10/10 10:59:23", "2010/10/10 11:03:26", "2010/10/10 11:06:15", "2010/10/10 11:07:15", "2010/10/10 11:09:10", "2010/10/10 11:11:11", "2010/10/10 11:16:16", "2010/10/10 11:19:27", "2010/10/10 11:28:17")
d <- as.POSIXct(d, tz="UCT")
tr <- c(111, 111, 111, 111, 111, 111, 111, 111, 111,111)
rec <- c(11, 22, 33, 44, 55, 66, 77, 33, 44, 11)
lat <- c(44.6358, 44.6362, 44.6373, 44.6368, 44.6371, 44.6375, 44.6380, 44.6373, 44.6368, 44.6358)
long <- c(-63.5949, -63.5931, -63.5912, -63.5890, -63.5882, -63.5873, -63.5885, -63.5912, -63.5890, -63.5949)
sampleEff <- data.frame(id=id,time=d, transmitter=tr, receiver=rec, longitude=long, latitude=lat)
effData <- efficiencyTest(sampleEff, type = "sample", minVelValue = 1, minDistValue = 150)

# Sample data for efficiencyTestFunc.R (with NA)
id <- 1:10
d <- c("2010/10/10 10:52:07", "2010/10/10 10:59:23", "2010/10/10 11:03:26", "2010/10/10 11:06:15", "2010/10/10 11:07:15", "2010/10/10 11:09:10", "2010/10/10 11:11:11", "2010/10/10 11:16:16", "2010/10/10 11:19:27", "2010/10/10 11:28:17")
d <- as.POSIXct(d, tz="UCT")
tr <- c(111, 111, 111, 111, NA, 111, 111, 111, 111,111)
rec <- c(11, NA, 33, 44, 55, 66, 77, 33, 44, 11)
lat <- c(44.6358, 44.6362, 44.6373, 44.6368, 44.6371, 44.6375, 44.6380, NA, 44.6368, 44.6358)
long <- c(-63.5949, -63.5931, NA, -63.5890, -63.5882, -63.5873, -63.5885, -63.5912, -63.5890, -63.5949)
sampleEffNA <- data.frame(id=id,time=d, transmitter=tr, receiver=rec, longitude=long, latitude=lat)
effDataNA <- efficiencyTest(sampleEffNA, type = "sample", minVelValue = 0.7, minDist=150)

# Expected results
minDistShouldBe <- c(149.37496, 149.37496, 182.93891, 71.63089, 71.63089, 84.05377, 110.15029, 182.93891, 182.93891, 480.43047)
minTimeShouldBe <- c(436, 243, 169, 60, 60, 115, 121, 191, 191, 530)
minVelShouldBe <- c(0.3426031, 0.6147118, 1.0824788, 1.1938482, 1.1938482, 0.7309023, 0.9103330, 0.9577953, 0.9577953, 0.9064726)
numIntValidShouldBe <- c(1, 2, 1, 1, 2, 2, 2, 1, 1, 1)
distValidShouldBe <- c(1, 1, 2, 1, 1, 1, 1, 2, 2, 2)
velValidShouldBe <- c(1, 1, 2, 2, 2, 1, 1, 1, 1, 1)
effValid1ShouldBe <- c(1, 2, 2, 2, 2, 2, 2, 2, 2, 2)
effValid2ShouldBe <- c(1, 1, 1, 1, 2, 1, 1, 1, 1, 1)
minDistNAShouldBe <- c(480.4305, NA, NA, 155.5808, NA, 110.1503, 110.1503, NA, 139.3310, 480.4305)
minTimeNAShouldBe <- c(848, NA, NA, 175, NA, 121, 121, NA, 496, 530)
minVelNAShouldBe <- c(0.5665454, NA, NA, 0.8890331, NA, 0.9103331, 0.9103331, NA, 0.2809093, 0.9064726)
numIntValidNAShouldBe <- c(1, 3, 1, 1, 3, 2, 2, 1, 1, 1)
distValidNAShouldBe <- c(2, 3, 3, 2, 3, 1, 1, 3, 1, 2)
velValidNAShouldBe <- c(1, 3, 3, 2, 3, 2, 2, 3, 1, 2)
effValid1NAShouldBe <- c(2, 3, 3, 2, 3, 2, 2, 3, 1, 2)
effValid2NAShouldBe <- c(1, 3, 3, 1, 3, 2, 2, 3, 1, 1)

# Test messages
#velMessageShouldBe <- "The filter identified 8 (53.33%) of 15 detections as invalid using the velocity test.\n"

# Testing columns that result from velTest using testthat library
test_that("min_dist column of data without NA gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(effData$min_dist, minDistShouldBe, tolerance = 3.43e-05) #To stop any error due to rounding
})
test_that("min_time column of data without NA gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(effData$min_time, minTimeShouldBe) #To stop any error due to rounding
})
test_that("min_vel column of data without NA gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(effData$min_vel, minVelShouldBe, tolerance = 3.33e-06) #To stop any error due to rounding
})
test_that("number detections and interval test validity column of data without NA gives expected result", {
  # Check that expected and actual results are the same
  expect_equal(effData$numIntervalValid, numIntValidShouldBe)
})
test_that("distance test validity column of data without NA gives expected result", {
  # Check that expected and actual results are the same
  expect_equal(effData$distValid, distValidShouldBe)
})
test_that("velocity test validity column of data without NA gives expected result", {
  # Check that expected and actual results are the same
  expect_equal(effData$velValid, velValidShouldBe)
})
test_that("efficiency test method 1 validity column of data without NA gives expected result", {
  # Check that expected and actual results are the same
  expect_equal(effData$effValid1, effValid1ShouldBe)
})
test_that("efficiency test method 2 validity column of data without NA gives expected result", {
  # Check that expected and actual results are the same
  expect_equal(effData$effValid2, effValid2ShouldBe)
})
test_that("min_dist column of data with NA gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(effDataNA$min_dist, minDistNAShouldBe, tolerance = 2.43e-05) #To stop any error due to rounding
})
test_that("min_time column of data with NA gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(effDataNA$min_time, minTimeNAShouldBe)
})
test_that("min_vel column of data with NA gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(effDataNA$min_vel, minVelNAShouldBe, tolerance = 4.29e-08) #To stop any error due to rounding
})
test_that("number detections and interval test validity column of data with NA gives expected result", {
  # Check that expected and actual results are the same
  expect_equal(effDataNA$numIntervalValid, numIntValidNAShouldBe)
})
test_that("distance test validity column of data with NA gives expected result", {
  # Check that expected and actual results are the same
  expect_equal(effDataNA$distValid, distValidNAShouldBe)
})
test_that("velocity test validity column of data with NA gives expected result", {
  # Check that expected and actual results are the same
  expect_equal(effDataNA$velValid, velValidNAShouldBe)
})
test_that("efficiency test method 1 validity column of data with NA gives expected result", {
  # Check that expected and actual results are the same
  expect_equal(effDataNA$effValid1, effValid1NAShouldBe)
})
test_that("efficiency test method 2 validity column of data with NA gives expected result", {
  # Check that expected and actual results are the same
  expect_equal(effDataNA$effValid2, effValid2NAShouldBe)
})

#test_that("velocity message gives expected result", {
#  expect_message(velTest(effData, "sample"), velMessageShouldBe)
#})