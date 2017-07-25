# To run:
# test_file("/Users/dinian/Desktop/glatos-git/R/testing/testDetectionBubblePlot.R")

# Test detectionBubblePlot function
test_that("detectionBubblePlot works for calculating the columns specified", {
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

  # loc <- c("TTB", "OSC", "PRS", "OSC", "OSC", "TTB", "PRS", "PRS", "TTB", "OSC")
  # anId <- rep(x=7, times=10)
  # time <- c("2010/10/11 11:11:11", "2010/10/11 23:12:53", "2010/10/12 14:23:12", "2010/10/13 07:24:52", "2010/10/13 13:17:13", "2010/10/13 20:20:20", "2010/10/14 17:43:22", "2010/10/15 18:19:23", "2010/10/16 10:56:56", "2010/10/16 22:11:33")
  # time <- as.POSIXct(time, tz="UCT")
  # lat <- c(43.392, 43.387, 41.571, 41.574, 41.576, 43.610, 43.612, 41.635, 41.644, 44.145)
  # long <- c(-83.993, -83.987, -83.618, -83.607, -83.611, -83.887, -83.861, -83.531, -83.534, -83.466)
  # sampleDetDBP <- data.frame(location=loc, animal=anId, time=time, latitude=lat, longitude=long)
  # sampleDetDBP$location <- as.character(sampleDetDBP$location)

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
  
  # loc <- c("TTB", "OSC", "PRS", "OSC", "PRS", "TTB", "OSC", "PRS", "TTB", "TTB")
  # lat <- c(45.103, 45.097, 44.988, 45.047, 45.884, 44.862, 45.414, 44.498, 45.490, 45.493)
  # long <- c(-87.626, -87.604, -87.665, -87.745, -87.864, -87.968, -87.350, -88.023, -84.060, -84.081)
  # dt <- c("2010/10/11 10:11:11", "2010/10/11 22:12:53", "2010/10/12 13:23:12", "2010/10/11 22:11:53", "2010/10/12 13:22:12", "2010/10/11 10:10:11", "2010/10/11 22:11:53", "2010/10/12 13:21:12", "2010/10/11 10:09:11", "2010/10/11 10:08:11")
  # dt <- as.POSIXct(dt, tz="UCT")
  # rt <- c("2010/10/16 11:56:56", "2010/10/16 23:11:33", "2010/10/15 19:19:23", "2010/10/16 23:12:33", "2010/10/16 19:20:23", "2010/10/16 11:57:56", "2010/10/16 23:12:33", "2010/10/15 19:21:23", "2010/10/16 11:58:56", "2010/10/16 11:59:56")
  # rt <- as.POSIXct(rt, tz="UCT")
  # sampleRecDBP <- data.frame(location=loc, latitude=lat, longitude=long, deploy_time=dt, recover_time=rt)
  # sampleRecDBP$location <- as.character(sampleRecDBP$location)

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
  
  # Check if expected and actual results are the same
  expect_equal(dbpNumFish$location, dbpNumFishLocShouldBe)
  expect_equal(dbpNumFish$Summary, dbpNumFishSummaryShouldBe)
  expect_equal(dbpNumFish$meanLat, dbpNumFishMeanLatShouldBe, tolerance = 3.33e-06) #tolerance used to remove any rounding errors
  expect_equal(dbpNumFish$meanLon, dbpNumFishMeanLonShouldBe, tolerance = 3.33e-06) #tolerance used to remove any rounding errors
  expect_equal(dbpNumDet$location, dbpNumDetLocShouldBe)
  expect_equal(dbpNumDet$Summary, dbpNumDetSummaryShouldBe)
  expect_equal(dbpNumDet$meanLat, dbpNumDetMeanLatShouldBe, tolerance = 3.33e-06) #tolerance used to remove any rounding errors
  expect_equal(dbpNumDet$meanLon, dbpNumDetMeanLonShouldBe, tolerance = 3.33e-06) #tolerance used to remove any rounding errors
})