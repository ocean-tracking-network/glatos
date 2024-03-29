context("Check make_transition")

#example 1 water polygon (Higgins Lake)
data(higgins_lake_polygon)
poly1 <- higgins_lake_polygon

trl1 <- make_transition(poly1, res = c(0.01, 0.01))

#raster::plot(trl1$rast)
#raster::plot(raster::raster(trl1$transition))

#higgins_lake_transition <- trl1
#saveRDS(higgins_lake_transition, file = "./inst/testdata/higgins_lake_transition.rds")


#example 2 land polygon (Flynn Island, Higgins Lake)
data(flynn_island_polygon)
poly2 <- flynn_island_polygon

trl2 <- make_transition(poly2, res = c(0.001, 0.001), 
                        all_touched = FALSE, 
                        invert = TRUE)

#raster::plot(trl2$rast)
#raster::plot(raster::raster(trl2$transition))

#flynn_island_transition <- trl2
#saveRDS(flynn_island_transition, file = "./inst/testdata/flynn_island_transition.rds")



# Expected results
# when called from devtools::test, working dir test
#  so need to handle that case vs package root
if(grepl("^glatos$", basename(getwd()))) testdata_dir <- normalizePath("./inst/testdata")
if(grepl("^testthat$", basename(getwd()))) testdata_dir <- normalizePath("../../inst/testdata")

trl1_trns_shouldBe <- readRDS(file.path(testdata_dir, 
                                        "higgins_lake_transition.rds"))

trl2_trns_shouldBe <- readRDS(file.path(testdata_dir, 
                                        "flynn_island_transition.rds"))

#raster::plot(trl2$rast)

# Drop names from rasters (to omit from comparisons)
trl1$rast@file@name <- NA_character_
trl2$rast@file@name <- NA_character_
trl1_trns_shouldBe$rast@file@name <- NA_character_
trl2_trns_shouldBe$rast@file@name <- NA_character_



# Testing water polygon transition matrix
test_that("Transition matrix for water polygon as expected", {
  # Check if expected and actual equal
  expect_equal(trl1$transition, trl1_trns_shouldBe$transition)
})

# Testing water polygon raster
test_that("Raster values for water polygon as expected", {
  # Check if expected and actual equal
  expect_equal(trl1$rast, trl1_trns_shouldBe$rast)
})


# Testing land polygon transition matrix
test_that("Transition matrix for land polygon as expected", {
  # Check if expected and actual equal
  expect_equal(trl2$transition, trl2_trns_shouldBe$transition)
})

# Testing land polygon raster
test_that("Raster values for land polygon as expected", {
  # Check if expected and actual equal
  expect_equal(trl2$rast, trl2_trns_shouldBe$rast)
})
