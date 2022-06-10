context("Check detect_transmissions")


# spatial transmission output
tr_sf <- readRDS("../../inst/testdata/test-transmit_along_path-tr_dfin_spout.rds")

# non spatial transmission output
tr_df <- data.frame(x = sf::st_coordinates(tr_sf)[,"X"],
                    y = sf::st_coordinates(tr_sf)[,"Y"],
                    time = tr_sf$time,
                    row.names = NULL)

# add receivers (non-spatial)
recs_df <- expand.grid(x = c(-87.75, -87.65),
                       y = c(48.5, 48.6))

recs_df$rec_id <- 1:nrow(recs_df)

# spatial receivers
recs_sf <- sf::st_as_sf(recs_df, coords = c("x", "y"), crs = 4326)


# Spatial input

# spatial detection output - 50% constant detection prob
set.seed(33)
dtc_spin_spout <- detect_transmissions(trnsLoc = tr_sf, 
                                       recLoc = recs_sf, 
                                       detRngFun = function(x) 0.5, 
                                       show_progress = FALSE)

# non-spatial detection output - 50% constant detection prob
set.seed(33)
dtc_spin_dfout <- detect_transmissions(trnsLoc = tr_sf, 
                                       recLoc = recs_sf, 
                                       detRngFun = function(x) 0.5,
                                       sp_out = FALSE, 
                                       show_progress = FALSE)

# Non-spatial input

# spatial detection output - 50% constant detection prob
set.seed(33)
dtc_dfin_spout <- detect_transmissions(trnsLoc = tr_df, 
                                       recLoc = recs_df, 
                                       detRngFun = function(x) 0.5, 
                                       inputCRS = 4326,
                                       show_progress = FALSE)

# non-spatial detection output - 50% constant detection prob
set.seed(33)
dtc_dfin_dfout <- detect_transmissions(trnsLoc = tr_df, 
                                       recLoc = recs_df, 
                                       detRngFun = function(x) 0.5,
                                       inputCRS = 4326,
                                       sp_out = FALSE, 
                                       show_progress = FALSE)

# Expected values
dtc_dfout_shouldBe <- readRDS("../../inst/testdata/test-detect_transmissions-dtc_dfout.rds")
dtc_spout_shouldBe <- readRDS("../../inst/testdata/test-detect_transmissions-dtc_spout.rds")


# Testing output matches desired format for each input
test_that("data.frame input, spatial output gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(dtc_dfin_spout, dtc_spout_shouldBe)
})
test_that("data.frame input, data.frame output gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(dtc_dfin_dfout, dtc_dfout_shouldBe)
})
test_that("spatial input, data.frame output gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(dtc_spin_dfout, dtc_dfout_shouldBe)
})
test_that("spatial input, spatial output gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(dtc_spin_spout, dtc_spout_shouldBe)
})
