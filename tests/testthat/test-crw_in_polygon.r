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
    show_progress = FALSE)
set.seed(30)
path_spin_dfout <- crw_in_polygon(greatLakesPoly, theta=c(0,25), stepLen=10000,
    initPos = c(-87.49017, 48.42314), initHeading=0, nsteps=5, sp_out = FALSE,
    show_progress = FALSE)

path_spin_spout_shouldBe <- 
new("SpatialPointsDataFrame", data = structure(list(), .Names = character(0), class = "data.frame", row.names = c(NA, 
6L)), coords.nrs = numeric(0), coords = structure(c(-87.49017, 
-87.5682764367357, -87.6624051520051, -87.7763030863065, -87.8344478577291, 
-87.9595460531474, 48.42314, 48.4965252233601, 48.5611619523508, 
48.609942977839, 48.6911737155544, 48.7263225805848), .Dim = c(6L, 
2L), .Dimnames = list(NULL, c("coords.x1", "coords.x2"))), bbox = structure(c(-87.9595460531474, 
48.42314, -87.49017, 48.7263225805848), .Dim = c(2L, 2L), .Dimnames = list(
c("coords.x1", "coords.x2"), c("min", "max"))), proj4string = new("CRS", 
projargs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))


path_spin_dfout_shouldBe <- 
structure(list(X = c(-87.49017, -87.5682764367357, -87.6624051520051, 
-87.7763030863065, -87.8344478577291, -87.9595460531474), Y = c(48.42314, 
48.4965252233601, 48.5611619523508, 48.609942977839, 48.6911737155544, 
48.7263225805848)), class = "data.frame", row.names = c("1", 
"2", "3", "4", "5", "6"))

path_dfin_spout_coords_shouldBe <- 
new("SpatialPointsDataFrame", data = structure(list(), .Names = character(0), class = "data.frame", row.names = c(NA, 
6L)), coords.nrs = numeric(0), coords = structure(c(0, -4.34765355285354, 
-9.75360353139628, -16.5935673247779, -19.6317805370012, -16.4086037689043, 
0, 9.00543772308487, 17.4182674984633, 24.7131215275189, 34.24041182839, 
43.7067274538907), .Dim = c(6L, 2L), .Dimnames = list(NULL, c("coords.x1", 
"coords.x2"))), bbox = structure(c(-19.6317805370012, 0, 0, 43.7067274538907
), .Dim = c(2L, 2L), .Dimnames = list(c("coords.x1", "coords.x2"
), c("min", "max"))), proj4string = new("CRS", projargs = "+proj=aea +lat_0=45.568977 +lon_0=-83.248627 +lat_1=42.122774 +lat_2=49.01518 +x_0=1000000 +y_0=1000000 +datum=NAD83 +units=m +no_defs"))

path_dfin_dfout_shouldBe <- 
structure(list(X = c(0, -4.34765355285354, -9.75360353139628, 
-16.5935673247779, -19.6317805370012, -16.4086037689043), Y = c(0, 
9.00543772308487, 17.4182674984633, 24.7131215275189, 34.24041182839, 
43.7067274538907)), class = "data.frame", row.names = c("1", 
"2", "3", "4", "5", "6"))


# Testing output matches desired format for each input
test_that("data.frame input, spatial output gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(sp::coordinates(path_dfin_spout), sp::coordinates(path_dfin_spout_coords_shouldBe))
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
  expect_equal(sp::coordinates(path_spin_spout), sp::coordinates(path_spin_spout_shouldBe))
})
