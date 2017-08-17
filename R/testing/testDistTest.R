# To run:
# test_file("/Users/dinian/Desktop/glatos-git/R/testing/testDistTest.R")

# Sample data for velTestFunc.R
id <- 1:10
d <- c("2010/10/10 10:52:07", "2010/10/10 10:59:23", "2010/10/10 11:03:26", "2010/10/10 11:06:15", "2010/10/10 11:07:15", "2010/10/10 11:09:10", "2010/10/10 11:11:11", "2010/10/10 11:16:16", "2010/10/10 11:19:27", "2010/10/10 11:28:17")
d <- as.POSIXct(d, tz="UCT")
tr <- c(111, 111, 111, 111, 111, 111, 111, 111, 111,111)
rec <- c(11, 22, 33, 44,55, 66, 77, 33, 44, 11)
lat <- c(44.6358, 44.6362, 44.6373, 44.6368, 44.6371, 44.6375, 44.6380, 44.6373, 44.6368, 44.6358)
long <- c(-63.5949, -63.5931, -63.5912, -63.5890, -63.5882, -63.5873, -63.5885, -63.5912, -63.5890, -63.5949)
sampleDist <- data.frame(id=id,time=d, transmitter=tr, receiver=rec, longitude=long, latitude=lat)
distData <- distTest(sampleDist, "sample", minDistValue = 150)

# Expected results
minDistShouldBe <- c(149.37496, 149.37496, 182.93891, 71.63089, 71.63089, 84.05377, 110.15029, 182.93891, 182.93891, 480.43047)
validShouldBe <- c(1, 1, 0, 1, 1, 1, 1, 0, 0, 0)

# Testing columns that result from velTest using testthat library
test_that("min_dist column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(distData$min_dist, minDistShouldBe)
})
test_that("valid column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(distData$distValid, validShouldBe)
})