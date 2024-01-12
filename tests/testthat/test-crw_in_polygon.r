# Testing output matches desired format for each input
test_that("data.frame input, spatial output gives expected result", {
  # non-spatial input
  mypolygon <- data.frame(x = c(-50, -50, 50, 50), y = c(-50, 50, 50, -50))

  set.seed(30)
  expect_snapshot(
    crw_in_polygon(
      mypolygon,
      theta = c(0, 20), stepLen = 10,
      initPos = c(0, 0), initHeading = 0, nsteps = 5,
      sp_out = TRUE,
      show_progress = FALSE
    )
  )
})

test_that("data.frame input, data.frame output gives expected result", {
  # non-spatial input
  mypolygon <- data.frame(x = c(-50, -50, 50, 50), y = c(-50, 50, 50, -50))

  set.seed(30)
  expect_snapshot(
    crw_in_polygon(
      mypolygon,
      theta = c(0, 20), stepLen = 10,
      initPos = c(0, 0), initHeading = 0, nsteps = 5,
      sp_out = FALSE,
      show_progress = FALSE
    )
  )
})


test_that("spatial input, data.frame output gives expected result", {
  set.seed(30)
  expect_snapshot(
    crw_in_polygon(
      greatLakesPoly,
      theta = c(0, 25), stepLen = 10000,
      initPos = c(-87.49017, 48.42314), initHeading = 0,
      nsteps = 5,
      sp_out = FALSE,
      cartesianCRS = 3175, show_progress = FALSE
    )
  )
})


test_that("spatial input, spatial output gives expected result", {
  set.seed(30)

  expect_snapshot(
    crw_in_polygon(
      greatLakesPoly,
      theta = c(0, 25), stepLen = 10000,
      initPos = c(-87.49017, 48.42314), initHeading = 0,
      nsteps = 5,
      sp_out = TRUE,
      cartesianCRS = 3175, show_progress = FALSE
    )
  )
})


##### TBD: TEST NON-EXPORTED FUNCTIONS ####
# Test non-exported query_worms_common function
test_that('internal function check_in_polygon', {
  skip('Test needs to be created.')
})

test_that('internal function check_cross_boundary', {
  skip('Test needs to be created.')
})
