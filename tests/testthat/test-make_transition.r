### make_transition3 tests TBD ####




### make_transition2 tests TBD ####




### make_transition
# Testing water polygon transition matrix
test_that("make_transition: Deprecation message and transition matrix for Higgins Lake water polygon as expected", {
  expect_snapshot(
    make_transition(
      higgins_lake_polygon, res = c(0.01, 0.01)
    )$transition
  )
})

# Testing water polygon raster
test_that("make_transition: Deprecation message and raster values for Higgins Lake water polygon as expected", {
  # Check if expected and actual equal
  expect_snapshot(
    make_transition(
      higgins_lake_polygon, res = c(0.01, 0.01)
    )$rast
  )
})


# Testing land polygon transition matrix
test_that("make_transition: Deprecation message and transition matrix for Flynn Island land polygon as expected", {
  # Check if expected and actual equal
  expect_snapshot(
    make_transition(
      flynn_island_polygon,
      res = c(0.001, 0.001),
      all_touched = FALSE,
      invert = TRUE
    )$transition
  )
})

# Testing land polygon raster
test_that("make_transition: Deprecation message and raster values for Flynn Island polygon as expected", {
  # Check if expected and actual equal
  expect_snapshot(
    make_transition(
      flynn_island_polygon,
      res = c(0.001, 0.001),
      all_touched = FALSE,
      invert = TRUE
    )$rast
  )
})
