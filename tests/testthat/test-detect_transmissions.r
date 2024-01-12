# spatial transmission output
tr_sf <- readRDS(
  test_path(
    'testdata',
    "transmit_along_path-tr_dfin_spout.rds"
  )
)

# non spatial transmission output
tr_df <- data.frame(
  x = sf::st_coordinates(tr_sf)[, "X"],
  y = sf::st_coordinates(tr_sf)[, "Y"],
  time = tr_sf$time,
  row.names = NULL
)

# add receivers (non-spatial)
recs_df <- expand.grid(
  x = c(-87.75, -87.65),
  y = c(48.5, 48.6)
)

recs_df$rec_id <- 1:nrow(recs_df)

# spatial receivers
recs_sf <- sf::st_as_sf(recs_df, coords = c("x", "y"), crs = 4326)


# Testing output matches desired format for each input

test_that("data.frame input, spatial output gives expected result", {
  # spatial detection output - 50% constant detection prob

  set.seed(33)

  # Check if expected and actual results are the same
  expect_snapshot(
    detect_transmissions(
      trnsLoc = tr_df,
      recLoc = recs_df,
      detRngFun = function(x) 0.5,
      inputCRS = sf::st_crs(tr_sf),
      show_progress = FALSE
    )
  )
})


test_that("data.frame input, data.frame output gives expected result", {
  # non-spatial detection output - 50% constant detection prob

  set.seed(33)

  # Check if expected and actual results are the same
  expect_snapshot(
    detect_transmissions(
      trnsLoc = tr_df,
      recLoc = recs_df,
      detRngFun = function(x) 0.5,
      inputCRS = 4326,
      sp_out = FALSE,
      show_progress = FALSE
    )
  )
})

test_that("spatial input, data.frame output gives expected result", {
  # non-spatial detection output - 50% constant detection prob

  set.seed(33)

  # Check if expected and actual results are the same
  expect_snapshot(
    detect_transmissions(
      trnsLoc = tr_sf,
      recLoc = recs_sf,
      detRngFun = function(x) 0.5,
      sp_out = FALSE,
      show_progress = FALSE)
  )
})



test_that("spatial input, spatial output gives expected result", {
  # spatial detection output - 50% constant detection prob

  set.seed(33)

  # Check if expected and actual results are the same
  expect_snapshot(
    detect_transmissions(
      trnsLoc = tr_sf,
      recLoc = recs_sf,
      detRngFun = function(x) 0.5,
      show_progress = FALSE
    )
  )
})

