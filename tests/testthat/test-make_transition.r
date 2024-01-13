### make_transition3 tests TBD ####




### make_transition2 tests TBD ####




### make_transition
# Testing water polygon transition matrix
test_that("make_transition: Transition matrix for Higgins Lake water polygon as expected", {
  expect_warning(
    water <- make_transition(
      higgins_lake_polygon,
      res = c(0.01, 0.01)
    )$transition,
    "This function is deprecated and will be removed in the next version"
  )


  expect_s4_class(
    water,
    "TransitionLayer"
  )
  expect_s3_class(water, NA)

  expect_equal(dim(water), c(10, 12, 1))


  expect_s4_class(
    water@transitionMatrix,
    "dsCMatrix"
  )
  expect_length(water@transitionMatrix, 14400)


  expect_snapshot(
    water
  )
})

# Testing water polygon raster
test_that("make_transition: Raster values for Higgins Lake water polygon as expected", {
  expect_warning(
    water <- make_transition(
      higgins_lake_polygon,
      res = c(0.01, 0.01)
    )$rast,
    "This function is deprecated and will be removed in the next version"
  )

  expect_s4_class(
    water,
    "RasterLayer"
  )
  expect_s3_class(water, NA)

  expect_equal(dim(water), c(10, 12, 1))

  expect_snapshot(
    water
  )
})



# Testing land polygon transition matrix
test_that("make_transition: Transition matrix for Flynn Island land polygon as expected", {
  expect_warning(
    land <- make_transition(
      flynn_island_polygon,
      res = c(0.001, 0.001),
      all_touched = FALSE,
      invert = TRUE
    )$transition,
    "This function is deprecated and will be removed in the next version"
  )

  expect_s4_class(
    land,
    "TransitionLayer"
  )
  expect_s3_class(land, NA)

  expect_equal(dim(land), c(7, 9, 1))


  expect_s4_class(
    land@transitionMatrix,
    "dsCMatrix"
  )
  expect_length(land@transitionMatrix, 3969)


  expect_snapshot(
    land
  )
})

# Testing land polygon raster
test_that("make_transition: Raster values for Flynn Island polygon as expected", {
  expect_warning(
    land <- make_transition(
      flynn_island_polygon,
      res = c(0.001, 0.001),
      all_touched = FALSE,
      invert = TRUE
    )$rast,
    "This function is deprecated and will be removed in the next version"
  )

  expect_s4_class(
    land,
    "RasterLayer"
  )
  expect_s3_class(land, NA)

  expect_equal(dim(land), c(7, 9, 1))

  expect_snapshot(
    land
  )
})
