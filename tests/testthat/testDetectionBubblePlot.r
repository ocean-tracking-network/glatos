# To run:
# test_file("/Users/dinian/Desktop/glatos-git/R/testing/testDetectionBubblePlot.R")

# Sample data for detectionBubblePlot.R
# Detection data:
loc <- c("EP", "EP", "EP", "HFX034", "HFX035", "HFX034")
anId <- c(7, 6, 7, 7, 6, 7)
time <- c("2014/04/26 11:11:11", "2014/09/26 11:11:11", "2014/08/28 11:11:11", "2014/08/30 01:30:26", "2014/08/30 01:30:33", "2014/08/30 01:33:38")
time <- as.POSIXct(time, tz="UCT")
lat <- c(44.197, 44.231, 44.225, 44.287, 44.282, 44.287)
long <- c(-63.192, -63.160, -63.190, -63.320, -63.313, -63.320)
sampleDetDBP <- data.frame(location=loc, animal=anId, time=time, latitude=lat, longitude=long)
sampleDetDBP$location <- as.character(sampleDetDBP$location)

#Receiver data:
loc <- c("EP", "HFX034", "HFX035")
lat <- c(44.219, 44.287, 44.282)
long <- c(-63.209, -63.320, -63.313)
dt <- c("2013/07/31 11:11:11", "2014/07/01 06:47:59", "2014/07/01 02:47:29")
dt <- as.POSIXct(dt, tz="UCT")
rt <- c("2014/09/26 11:11:11", "2014/11/01 05:59:16", "2014/10/31 10:16:14")
rt <- as.POSIXct(rt, tz="UCT")
sampleRecDBP <- data.frame(location=loc, latitude=lat, longitude=long, deploy_time=dt, recover_time=rt)
sampleRecDBP$location <- as.character(sampleRecDBP$location)

dbpData <- detectionBubblePlot(sampleDetDBP, sampleRecDBP, "sample")
dbpNumFish <- dbpData[[1]]
dbpNumDet <- dbpData[[2]]

# Expected results
dbpNumFishLocShouldBe <- c("HFX034", "HFX035", "EP")
dbpNumFishSummaryShouldBe <- c(1, 1, 2)
dbpNumFishMeanLatShouldBe <- c(44.28700, 44.28200, 44.21767)
dbpNumFishMeanLonShouldBe <- c(-63.32000, -63.31300, -63.18067)
dbpNumDetLocShouldBe <- c("HFX035", "HFX034", "EP" )
dbpNumDetSummaryShouldBe <- c(1, 2, 3)
dbpNumDetMeanLatShouldBe <- c(44.28200, 44.28700, 44.21767)
dbpNumDetMeanLonShouldBe <- c(-63.31300, -63.32000, -63.18067)

# Test values that result from detectionBubblePlot using testthat library
test_that("location column in numFish table gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(dbpNumFish$location, dbpNumFishLocShouldBe)
})
test_that("Summary column in numFish table gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(dbpNumFish$Summary, dbpNumFishSummaryShouldBe)
})
test_that("meanLat column in numFish table gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(dbpNumFish$meanLat, dbpNumFishMeanLatShouldBe, tolerance = 3.33e-06) #tolerance used to remove any rounding errors
})
test_that("meanLon column in numFish table gives expected result", {
  expect_equal(dbpNumFish$meanLon, dbpNumFishMeanLonShouldBe, tolerance = 3.33e-06) #tolerance used to remove any rounding errors
})
test_that("location column in numDet table gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(dbpNumDet$location, dbpNumDetLocShouldBe)
})
test_that("Summary column in numDet table gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(dbpNumDet$Summary, dbpNumDetSummaryShouldBe)
})
test_that("meanLat column in numDet table gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(dbpNumDet$meanLat, dbpNumDetMeanLatShouldBe, tolerance = 3.33e-06) #tolerance used to remove any rounding errors
})
test_that("meanLon column in numDet table gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(dbpNumDet$meanLon, dbpNumDetMeanLonShouldBe, tolerance = 3.33e-06) #tolerance used to remove any rounding errors
})