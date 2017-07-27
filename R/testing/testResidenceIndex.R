# To run:
# test_file("/Users/dinian/Desktop/glatos-git/R/testing/testResidenceIndex.R")

# Sample data for residence_index.R
id <- 1:10
sd <- c("2010/10/11 11:11:11", "2010/10/11 07:23:16", "2010/10/11 15:24:19", "2010/10/12 17:16:16", "2010/10/12 18:01:11", "2010/10/11 06:16:24", "2010/10/12 07:52:13", "2010/10/12 21:24:24", "2010/10/12 22:22:22", "2010/10/13 11:11:11")
ed <- c("2010/10/12 13:14:15", "2010/10/13 17:12:11", "2010/10/15 08:54:23", "2010/10/12 17:16:17", "2010/10/13 09:17:12", "2010/10/12 20:21:22", "2010/10/13 17:26:17", "2010/10/15 21:19:02", "2010/10/13 11:24:16", "2010/10/13 14:21:33")
sd <- as.POSIXct(sd, tz="UCT")
ed <- as.POSIXct(ed, tz="UCT")
sampleResInd <- data.frame(id=id, startdate=sd, enddate=ed)

#Actual results
totalDiffDays <- total_diff_days(sampleResInd, "sample")
totalDaysCount <- total_days_count(sampleResInd, "sample")
aggregateTotalWithOverlap <- aggregate_total_with_overlap(sampleResInd, "sample")
aggregateTotalNoOverlap <- aggregate_total_no_overlap(sampleResInd, "sample")
getDaysDiffDays <- get_days(sampleResInd, "sample", "timedelta")
getDaysCount <- get_days(sampleResInd, "sample", "daycount")
getDaysAggWithOverlap <- get_days(sampleResInd, "sample", "aggregate_with_overlap")
getDaysAggNoOverlap <- get_days(sampleResInd, "sample", "aggregate_no_overlap")

# Expected results
totalDiffDaysShouldBe <- 4.6268 #Expected result of get_days("timedelta") as well
totalDaysCountShouldBe <- 4 #Expected result of get_days("daycount") as well
aggregateTotalWithOverlapShouldBe <- 14.00002 #Expected result of get_days("aggregate_with_overlap") as well
aggregateTotalNoOverlapShouldBe <- 4.109711 #Expected result of get_days("aggregate_no_overlap") as well

# Test values that result from residence_index.R methods using testthat library
test_that("totalDiffDays gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(totalDiffDays, totalDiffDaysShouldBe, tolerance = 2.87e-05) #tolerance used to remove any rounding errors
})
test_that("totalDaysCount gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(totalDaysCount, totalDaysCountShouldBe)
})
test_that("aggregateTotalWithOverlap gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(aggregateTotalWithOverlap, aggregateTotalWithOverlapShouldBe, tolerance = 3.15e-06) #tolerance used to remove any rounding errors
})
test_that("aggregteTotalNoOverlap gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(aggregateTotalNoOverlap, aggregateTotalNoOverlapShouldBe, tolerance = 3.52e-07) #tolerance used to remove any rounding errors
})
test_that("getDaysDiffDays gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(getDaysDiffDays, totalDiffDaysShouldBe, tolerance = 2.87e-05) #tolerance used to remove any rounding errors
})
test)that("getDaysCount gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(getDaysCount, totalDaysCountShouldBe)
})
test_that("getDaysAggWithOverlap gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(getDaysAggWithOverlap, aggregateTotalWithOverlapShouldBe, tolerance = 3.15e-06) #tolerance used to remove any rounding errors
})
test_that("getDaysAggNoOverlap gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(getDaysAggNoOverlap, aggregateTotalNoOverlapShouldBe, tolerance = 3.52e-07) #tolerance used to remove any rounding errors
})