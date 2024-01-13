# Testing output matches desired format for each input
test_that("data.frame input, sf output gives expected result", {
  # non-spatial input
  mypolygon <- data.frame(x = c(-50, -50, 50, 50), y = c(-50, 50, 50, -50))

  set.seed(30)

  expect_s3_class(
    dfin_sfout <- crw_in_polygon(
      mypolygon,
      theta = c(0, 20), stepLen = 10,
      initPos = c(0, 0), initHeading = 0, nsteps = 5,
      sp_out = TRUE,
      show_progress = FALSE
    ),
    'sf'
  )

  expect_equal(dim(dfin_sfout), c(6, 1))

  expect_snapshot(
    dfin_sfout
  )
})

test_that("data.frame input, data.frame output gives expected result", {
  # non-spatial input
  mypolygon <- data.frame(x = c(-50, -50, 50, 50), y = c(-50, 50, 50, -50))

  set.seed(30)

  expect_s3_class(
    dfin_dfout <- crw_in_polygon(
      mypolygon,
      theta = c(0, 20), stepLen = 10,
      initPos = c(0, 0), initHeading = 0, nsteps = 5,
      sp_out = FALSE,
      show_progress = FALSE
    ),
    'data.frame'
  )

  expect_equal(dim(dfin_dfout), c(6, 2))

  expect_snapshot(
    dfin_dfout
  )
})


test_that("SpatialPolygonsDataFrame input, data.frame output gives expected result", {
  set.seed(30)

  expect_s3_class(
    spin_dfout <- crw_in_polygon(
      greatLakesPoly,
      theta = c(0, 25), stepLen = 10000,
      initPos = c(-87.49017, 48.42314), initHeading = 0,
      nsteps = 5,
      sp_out = FALSE,
      cartesianCRS = 3175, show_progress = FALSE
    ),
    'data.frame'
  )

  expect_equal(dim(spin_dfout), c(6, 2))

  expect_snapshot(
    spin_dfout
  )
})


test_that("SpatialPolygonsDataFrame input, sf output gives expected result", {
  set.seed(30)

  expect_s3_class(
    spin_sfout <- crw_in_polygon(
      greatLakesPoly,
      theta = c(0, 25), stepLen = 10000,
      initPos = c(-87.49017, 48.42314), initHeading = 0,
      nsteps = 5,
      sp_out = TRUE,
      cartesianCRS = 3175, show_progress = FALSE
    ),
    'sf'
  )

  expect_equal(dim(spin_sfout), c(6, 1))

  expect_snapshot(
    spin_sfout
  )
})


test_that("sf input, data.frame output gives expected result", {
  set.seed(30)

  expect_s3_class(
    sfin_dfout <- crw_in_polygon(
      great_lakes_polygon,
      theta = c(0, 25), stepLen = 10000,
      initPos = c(-87.49017, 48.42314), initHeading = 0,
      nsteps = 5,
      sp_out = FALSE,
      cartesianCRS = 3175, show_progress = FALSE
    ),
    'data.frame'
  )

  expect_equal(dim(sfin_dfout), c(6, 2))

  expect_snapshot(
    sfin_dfout
  )
})


test_that("sf input, sf output gives expected result", {
  set.seed(30)

  expect_s3_class(
    sfin_sfout <- crw_in_polygon(
      great_lakes_polygon,
      theta = c(0, 25), stepLen = 10000,
      initPos = c(-87.49017, 48.42314), initHeading = 0,
      nsteps = 5,
      sp_out = TRUE,
      cartesianCRS = 3175, show_progress = FALSE
    ),
    'sf'
  )

  expect_equal(dim(sfin_sfout), c(6, 1))

  expect_snapshot(
    sfin_sfout
  )
})


##### TBD: TEST NON-EXPORTED FUNCTIONS ####
# Test non-exported query_worms_common function
test_that("internal function check_in_polygon", {
  skip("Test needs to be created.")
})

test_that("internal function check_cross_boundary", {
  skip("Test needs to be created.")
})
