# To run:
# test_file("/Users/dinian/Desktop/glatos-git/R/testing/testFalseDetectionFilter.R")

# Test falseDetectionFilter function
test_that("falseDetectionFilter works for calculating the column of 'min_lag' and checking if the entry is valid", {
  # Sample data for falseDetectionFilter.R
  id <- 1:10
  time <- c("2010/10/11 08:14:22", "2010/10/11 08:15:22", "2010/10/11 09:00:00", "2010/10/11 10:23:55", "2010/10/11 11:23:55", "2010/10/11 11:24:55", "2010/10/11 11:24:56", "2010/10/11 11:52:44", "2010/10/11 11:58:44", "2010/10/11 11:59:45")
  time <- as.POSIXct(time, tz="UCT")
  trans <- c(121, 151, 161, 151, 151, 161, 121, 151, 161, 121)
  rec <- c(4, 2, 3, 2, 2, 2, 4, 2, 2, 4)
  sampleFD <- data.frame(id=id, time=time, transmitter=trans, receiver=rec)
  fdData <- falseDetectionFilter(sampleFD, "sample", tf=3600)
  
  # Expected results
  minLagShouldBe <- c(11434, 7713, NA, 3600, 1729, 2029, 2089, 1729, 2029, 2089)
  validShouldBe <- c(0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
  
  # Check if expected and actual results are the same
  expect_equal(fdData$min_lag, minLagShouldBe)
  expect_equal(fdData$passedFilter, validShouldBe)
})