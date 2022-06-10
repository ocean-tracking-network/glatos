context("Check transmit_along_path")


# Spatial input
path_sf <- readRDS("./inst/testdata/test-crw_in_polygon-path_spin_spout.RDS")

# spatial output
set.seed(30)
tr_spin_spout <- transmit_along_path(path_sf, vel = 5.0,
                                     delayRng = c(600, 1800), burstDur=5.0)

# non-spatial output
set.seed(30)
tr_spin_dfout <- transmit_along_path(path_sf, vel = 5.0,
                                     delayRng = c(600, 1800), burstDur = 5.0, 
                                     sp_out = FALSE)


# Non-spatial input
path_df <- as.data.frame(sf::st_coordinates(path_sf))


# spatial output
set.seed(30)
tr_dfin_spout <- transmit_along_path(path_df, vel = 5.0, 
                                     delayRng = c(600, 1800),
                                     burstDur = 5.0, 
                                     colNames = list(x = "X", y = "Y"),
                                     pathCRS = 4326)

# non-spatial output
set.seed(30)
tr_dfin_dfout <- transmit_along_path(path_df, vel = 5.0, 
                                     delayRng = c(600, 1800),
                                     burstDur = 5.0,
                                     colNames = list(x = "X", y = "Y"),
                                     pathCRS = 4326, sp_out = FALSE)


# Expected results

tr_spin_spout_shouldBe <- readRDS("./inst/testdata/test-transmit_along_path-tr_spin_spout.rds")

tr_dfin_spout_shouldBe <- readRDS("./inst/testdata/test-transmit_along_path-tr_dfin_spout.rds")

tr_dfout_shouldBe <- data.frame(x = sf::st_coordinates(tr_spout_shouldBe)[,"X"],
                                y = sf::st_coordinates(tr_spout_shouldBe)[,"Y"],
                                time = tr_spout_shouldBe$time,
                                row.names = NULL)

# Testing output matches desired format for each input
test_that("data.frame input, spatial output gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(tr_dfin_spout, tr_dfin_spout_shouldBe)
})
test_that("data.frame input, data.frame output gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(tr_dfin_dfout, tr_dfout_shouldBe)
})
test_that("spatial input, data.frame output gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(tr_spin_dfout, tr_dfout_shouldBe)
})
test_that("spatial input, spatial output gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(tr_spin_spout, tr_spout_shouldBe)
})
