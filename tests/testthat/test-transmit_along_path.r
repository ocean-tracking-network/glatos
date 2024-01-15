# Testing output matches desired format for each input
test_that("data.frame input, spatial output gives expected result", {
  path_sp <- {
    set.seed(30)

    crw_in_polygon(
      greatLakesPoly,
      theta = c(0, 25), stepLen = 10000,
      initPos = c(-87.49017, 48.42314), initHeading = 0,
      nsteps = 5,
      sp_out = TRUE,
      cartesianCRS = 3175, show_progress = FALSE
    )
  }

  path_df <- as.data.frame(sf::st_coordinates(path_sp))


  tr_dfin_spout <- {
    set.seed(30)

    transmit_along_path(
      path_df,
      vel = 5.0,
      delayRng = c(600, 1800),
      burstDur = 5.0,
      colNames = list(x = "X", y = "Y"),
      pathCRS = sf::st_crs(path_sp)
    )
  }

  expect_s3_class(tr_dfin_spout, c('sf', 'data.frame'), exact = T)

  expect_equal(sf::st_crs(tr_dfin_spout)$proj4string,
               '+proj=longlat +datum=WGS84 +no_defs')

  expect_snapshot(
    tr_dfin_spout
  )
})




test_that("data.frame input, data.frame output gives expected result", {
  path_sp <- {
    set.seed(30)

    crw_in_polygon(
      greatLakesPoly,
      theta = c(0, 25), stepLen = 10000,
      initPos = c(-87.49017, 48.42314), initHeading = 0,
      nsteps = 5,
      sp_out = TRUE,
      cartesianCRS = 3175, show_progress = FALSE
    )
  }

  path_df <- as.data.frame(sf::st_coordinates(path_sp))

  tr_dfin_dfout <- {
    set.seed(30)

    transmit_along_path(
      path_df,
      vel = 5.0,
      delayRng = c(600, 1800),
      burstDur = 5.0,
      colNames = list(x = "X", y = "Y"),
      pathCRS = 4326,
      sp_out = FALSE
    )
  }

  expect_s3_class(tr_dfin_dfout, 'data.frame', exact = T)

  # Check if expected and actual results are the same
  expect_snapshot(
    tr_dfin_dfout
  )
})




test_that("spatial input, data.frame output gives expected result", {
  path_sp <- {
    set.seed(30)

    crw_in_polygon(
      greatLakesPoly,
      theta = c(0, 25), stepLen = 10000,
      initPos = c(-87.49017, 48.42314), initHeading = 0,
      nsteps = 5,
      sp_out = TRUE,
      cartesianCRS = 3175, show_progress = FALSE
    )
  }

  tr_spin_dfout <- {
    set.seed(30)

    transmit_along_path(
      path_sp,
      vel = 5.0,
      delayRng = c(600, 1800),
      burstDur = 5.0,
      sp_out = FALSE
    )
  }
  # Check if expected and actual results are the same
  expect_s3_class(tr_spin_dfout, 'data.frame', exact = T)

  expect_snapshot(
    tr_spin_dfout
  )
})




test_that("spatial input, spatial output gives expected result", {

  path_sp <- {
    set.seed(30)

    crw_in_polygon(
      greatLakesPoly,
      theta = c(0, 25), stepLen = 10000,
      initPos = c(-87.49017, 48.42314), initHeading = 0,
      nsteps = 5,
      sp_out = TRUE,
      cartesianCRS = 3175, show_progress = FALSE
    )
  }

  tr_spin_spout <- {
    set.seed(30)

    transmit_along_path(
      path_sp,
      vel = 5.0,
      delayRng = c(600, 1800),
      burstDur = 5.0
    )
  }


  expect_s3_class(tr_spin_spout, c('sf', 'data.frame'), exact = T)

  expect_equal(sf::st_crs(tr_spin_spout)$proj4string,
               '+proj=longlat +datum=WGS84 +no_defs')

  # Check if expected and actual results are the same
  expect_snapshot(
    tr_spin_spout
  )
})
