context("Check crw_in_polygon")

# non-spatial input
mypolygon <- data.frame(x = c(-50,-50, 50, 50), y = c(-50,50,50,-50))
set.seed(30)
path_dfin_spout <- crw_in_polygon(mypolygon, theta = c(0, 20), stepLen = 10, 
                         initPos=c(0,0), initHeading=0, nsteps=5)
set.seed(30)
path_dfin_dfout <- crw_in_polygon(mypolygon, theta = c(0, 20), stepLen = 10, 
                         initPos=c(0,0), initHeading=0, nsteps=5, sp_out = FALSE)


# spatial input
data(greatLakesPoly)
set.seed(30)
path_spin_spout <- crw_in_polygon(greatLakesPoly, theta=c(0,25), stepLen=10000,
    initPos = c(-87.49017, 48.42314), initHeading=0, nsteps=5)
set.seed(30)
path_spin_dfout <- crw_in_polygon(greatLakesPoly, theta=c(0,25), stepLen=10000,
    initPos = c(-87.49017, 48.42314), initHeading=0, nsteps=5, sp_out = FALSE)



# Expected results
path_dfin_spout_shouldBe <- 
  new("SpatialPoints", coords = structure(c(-1.16415321826935e-10, 
  -4.34765355300624, -9.75360353128053, -16.5935673250351, -19.6317805371946, 
  -16.4086037687957, 4.65661287307739e-09, 9.00543772650417, 17.4182675043121, 
  24.7131215310656, 34.2404118331615, 43.7067274576984), .Dim = c(6L, 
    2L), .Dimnames = list(NULL, c("x", "y"))), bbox = structure(c(-19.6317805371946, 
      4.65661287307739e-09, -1.16415321826935e-10, 43.7067274576984
    ), .Dim = c(2L, 2L), .Dimnames = list(c("x", "y"), c("min", "max"
    ))), proj4string = new("CRS", 
      projargs = "+init=epsg:3175 +proj=aea +lat_1=42.122774 +lat_2=49.01518 +lat_0=45.568977 +lon_0=-83.248627 +x_0=1000000 +y_0=1000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

path_dfin_dfout_shouldBe <- 
  structure(list(x = c(-1.16415321826935e-10, -4.34765355300624, 
    -9.75360353128053, -16.5935673250351, -19.6317805371946, -16.4086037687957
  ), y = c(4.65661287307739e-09, 9.00543772650417, 17.4182675043121, 
    24.7131215310656, 34.2404118331615, 43.7067274576984)), class = "data.frame", row.names = c(NA, 
      -6L))

path_spin_spout_shouldBe <- 
  new("SpatialPoints", coords = structure(c(-87.49017, -87.5682764367357, 
    -87.6624051520051, -87.7763030863065, -87.8344478577291, -87.9595460531474, 
    48.42314, 48.4965252233601, 48.5611619523508, 48.609942977839, 
    48.6911737155544, 48.7263225805848), .Dim = c(6L, 2L), .Dimnames = list(
      NULL, c("x", "y"))), bbox = structure(c(-87.9595460531474, 
        48.42314, -87.49017, 48.7263225805848), .Dim = c(2L, 2L), .Dimnames = list(
          c("x", "y"), c("min", "max"))), proj4string = new("CRS", 
            projargs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

path_spin_dfout_shouldBe <- 
  structure(list(x = c(-87.49017, -87.5682764367357, -87.6624051520051, 
    -87.7763030863065, -87.8344478577291, -87.9595460531474), y = c(48.42314, 
      48.4965252233601, 48.5611619523508, 48.609942977839, 48.6911737155544, 
      48.7263225805848)), class = "data.frame", row.names = c(NA, -6L
      ))

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
