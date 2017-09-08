# To run:
# test_file("/Users/dinian/Desktop/glatos-git/R/testing/testMovePath.R")

# Sample data for movePath.R
sX <- c(-63.8)
sY <- c(44.2)
eX <- c(-63.3)
eY <- c(44.7)
times <- c("2010/10/11 09:15:23", "2010/10/11 10:15:23", "2010/10/11 11:15:23", "2010/10/11 12:15:23", "2010/10/11 13:15:23", "2010/10/11 14:15:23")
times <- as.POSIXct(times, tz="UCT")
mpData <- movePath(sX, sY, eX, eY, times)

# Expected results
lonShouldBe <- c(-63.7, -63.6, -63.5, -63.4)
latShouldBe <- c(44.3, 44.4, 44.5, 44.6)
timestampShouldBe <- c("2010/10/11 10:15:23", "2010/10/11 11:15:23", "2010/10/11 12:15:23", "2010/10/11 13:15:23")
timestampShouldBe <- as.POSIXct(timestampShouldBe, tz="UCT")

# Testing columns that result from detectionEventFilter using testthat library
test_that("Longitude column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(mpData$lon, lonShouldBe)
})
test_that("Latitude column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(mpData$lat, latShouldBe)
})
test_that("Timestamp column gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(mpData$timestamp, timestampShouldBe)
})