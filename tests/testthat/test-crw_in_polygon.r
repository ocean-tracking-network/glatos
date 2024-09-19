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
    "sf"
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
    "data.frame"
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
    "data.frame"
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
    "sf"
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
    "data.frame"
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
    "sf"
  )

  expect_equal(dim(sfin_sfout), c(6, 1))

  expect_snapshot(
    sfin_sfout
  )
})


##### TEST NON-EXPORTED FUNCTIONS ####

test_that("internal function crosses_boundary gives expected result", {
  # crosses one boundary (inside poly to outside poly)
  expect_type(
    x <- crosses_boundary(
      matrix(c(0:6, rep(3, 7)), ncol = 2),
      sf::st_linestring(matrix(c(0, 0, 6, 0, 3, 6, 0, 0), ncol = 2, byrow = TRUE))
    ),
    "logical"
  )

  expect_equal(x, c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE))


  # crosses two boundaries (skips 'peninsula'; inside poly to inside poly)
  expect_type(
    x2 <- crosses_boundary(
      matrix(c(0, 1, 5, 6, rep(3, 4)), ncol = 2),
      sf::st_linestring(matrix(c(0, 0, 6, 0, 3, 6, 0, 0), ncol = 2, byrow = TRUE))
    ),
    "logical"
  )

  expect_equal(x2, c(FALSE, TRUE, FALSE))
})
