context("Check crw_in_polygon")

# non-spatial input
mypolygon <- data.frame(x = c(-50,-50, 50, 50), y = c(-50,50,50,-50))
set.seed(30)
path_dfin_spout <- crw_in_polygon(mypolygon, theta = c(0, 20), stepLen = 10, 
                         initPos=c(0,0), initHeading=0, nsteps=5,
                         sp_out = TRUE, show_progress = FALSE)
set.seed(30)
path_dfin_dfout <- crw_in_polygon(mypolygon, theta = c(0, 20), stepLen = 10, 
                         initPos=c(0,0), initHeading=0, nsteps=5, 
                         sp_out = FALSE, show_progress = FALSE)


# spatial input
data(greatLakesPoly)
set.seed(30)
path_spin_spout <- crw_in_polygon(greatLakesPoly, theta=c(0,25), stepLen=10000,
    initPos = c(-87.49017, 48.42314), initHeading=0, nsteps=5, sp_out = TRUE,
    cartesianCRS = 3175, show_progress = FALSE)
set.seed(30)
path_spin_dfout <- crw_in_polygon(greatLakesPoly, theta=c(0,25), stepLen=10000,
    initPos = c(-87.49017, 48.42314), initHeading=0, nsteps=5, sp_out = FALSE,
    cartesianCRS = 3175, show_progress = FALSE)

path_spin_spout_shouldBe <- 
  readRDS("../../inst/testdata/test-crw_in_polygon-path_spin_spout.rds")

  
path_spin_dfout_shouldBe <- 
structure(list(x = c(-87.49017, -87.5682764367357, -87.6624051520051, 
-87.7763030863065, -87.8344478577291, -87.9595460531474), y = c(48.42314, 
48.4965252233601, 48.5611619523508, 48.609942977839, 48.6911737155544, 
48.7263225805848)), class = "data.frame", row.names = c("1", 
"2", "3", "4", "5", "6"))

path_dfin_spout_shouldBe <- 
  readRDS("../../inst/testdata/test-crw_in_polygon-path_dfin_spout.rds")
  
path_dfin_dfout_shouldBe <- 
structure(list(x = c(0, -4.34765355285354, -9.75360353139628, 
-16.5935673247779, -19.6317805370012, -16.4086037689043), y = c(0, 
9.00543772308487, 17.4182674984633, 24.7131215275189, 34.24041182839, 
43.7067274538907)), class = "data.frame", row.names = c("1", 
"2", "3", "4", "5", "6"))


# Testing output matches desired format for each input
test_that("data.frame input, spatial output gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(path_dfin_spout, path_dfin_spout_shouldBe)
})
test_that("data.frame input, data.frame output gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(path_dfin_dfout, path_dfin_dfout_shouldBe)
})
test_that("spatial input, data.frame output gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(path_spin_dfout, path_spin_dfout_shouldBe)
})
test_that("spatial input, spatial output gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(path_spin_spout, path_spin_spout_shouldBe)
})
