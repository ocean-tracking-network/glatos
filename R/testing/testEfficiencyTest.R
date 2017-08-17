# To run:
# test_file("/Users/dinian/Desktop/glatos-git/R/testing/testEfficiencyTest.R")

# Sample data for efficiencyTestFunc.R
id <- 1:15
d <- c("2010/10/11 11:00:00", "2010/10/11 11:01:00", "2010/10/11 11:02:00", "2010/10/11 11:03:00", "2010/10/11 11:04:00", "2010/10/11 11:05:00","2010/10/11 11:06:00", "2010/10/11 11:07:00", "2010/10/11 11:08:00", "2010/10/11 11:09:00", "2010/10/11 11:10:00", "2010/10/11 11:11:00","2010/10/11 11:12:00", "2010/10/11 11:13:00", "2010/10/11 11:14:00")
d <- as.POSIXct(d, tz="UCT")
tr <- c("A", "A", "A", "B", "A", "B", "B", "A", "A", "A", "A", "B", "B", "A", "A")
rec <- c("a", "a", "c", "a", "c", "b", "b", "b", "a", "a", "a", "c", "a", "c", "d")
long <- c(-63.62603, -63.64397, -63.62603, -63.61513, -63.58028, -63.59170, -63.61874, -63.61513, -63.58063, -63.56758, -63.58028, -63.56861, -63.58028, -63.59170, -63.58028)
lat <- c(44.66584, 44.67134, 44.66584, 44.65724, 44.63177, 44.63660, 44.64930, 44.65724, 44.64747, 44.64179, 44.63177, 44.62328, 44.63177, 44.63660, 44.63177)
sampleEff <- data.frame(id=id, time=d, transmitter=tr, receiver=rec, longitude=long, latitude=lat)
effData <- efficiencyTest(sampleEff, "sample", minVelValue = 20, minDistValue = 1300, shortIntSec = 3*60, longIntSec = 5*60)


# Expected results
minDistShouldBe <- c(1546.6335, 1546.6335, 1288.9375, 1288.9375, 1052.3652, 1052.3652,  928.9577,  928.9577, 1211.6445, 1211.6445, 1322.1264, 1322.1264, 1052.3652, 1052.3652, 1052.3652)
minTimeShouldBe <- rep(x=60, times=15)
minVelShouldBe <- c(25.77722, 25.77722, 21.48229, 21.48229, 17.53942, 17.53942, 15.48263, 15.48263, 20.19408, 20.19408, 22.03544, 22.03544, 17.53942, 17.53942, 17.53942)
numIntValidShouldBe <- c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0)
distValidShouldBe <- c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1)
velValidShouldBe <- c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1)
effValid1ShouldBe <- c(2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 2)
effValid2ShouldBe <- c(1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1)

# Testing columns that result from velTest using testthat library
test_that("min_dist column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(effData$min_dist, minDistShouldBe, tolerance = 3.43e-05) #To stop any error due to rounding
})
test_that("min_time column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(effData$min_time, minTimeShouldBe) #To stop any error due to rounding
})
test_that("min_vel column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(effData$min_vel, minVelShouldBe, tolerance = 3.33e-06) #To stop any error due to rounding
})
test_that("number detections and interval test validity column gives expected result", {
  # Check that expected and actual results are the same
  expect_equal(effData$numIntervalValid, numIntValidShouldBe)
})
test_that("distance test validity column gives expected result", {
  # Check that expected and actual results are the same
  expect_equal(effData$distValid, distValidShouldBe)
})
test_that("velocity test validity column gives expected result", {
  # Check that expected and actual results are the same
  expect_equal(effData$velValid, velValidShouldBe)
})
test_that("efficiency test method 1 validity column gives expected result", {
  # Check that expected and actual results are the same
  expect_equal(effData$effValid1, effValid1ShouldBe)
})
test_that("efficiency test method 2 validity column gives expected result", {
  # Check that expected and actual results are the same
  expect_equal(effData$effValid2, effValid2ShouldBe)
})