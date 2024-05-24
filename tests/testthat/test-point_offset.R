test_that("Returns named matrix", {
  expect_type(point_offset(lon = -83.0, lat = 44.0), 'double')
  expect_vector(point_offset(lon = -83.0, lat = 44.0))
  
  # No inputs
  expect_type(point_offset(), 'double')
  
  expect_identical(
    colnames(point_offset(lon = -83.0, lat = 44.0)),
    c('lon', 'lat')
  )
})




test_that("Converts feet to meters", {
  expect_equal(
    point_offset(lon = -83.0, lat = 44.0, offsetDist = 100,
                 offsetDir = 'NE', distUnit = 'ft'),
    matrix(c(-82.99973, 44.00019),
           ncol = 2,
           dimnames = list(NULL, c('lon', 'lat'))),
    tolerance = 1e-5
  )
  
  expect_false(
    identical(
      point_offset(lon = -83.0, lat = 44.0, offsetDist = 100,
                   offsetDir = 'NE', distUnit = 'm'),
      point_offset(lon = -83.0, lat = 44.0, offsetDist = 100,
                   offsetDir = 'NE', distUnit = 'ft')
    )
  )
})




test_that("Errors with wrong units", {
  expect_error(
    point_offset(lon = -83.0, lat = 44.0, offsetDist = 100,
                 offsetDir = 'NE', distUnit = 'km'),
    "Input attribute 'dirUnit' must be 'm' \\(meters\\) or 'ft' \\(feet\\)\\."
  )
})




test_that("No input returns NA", {
  # No distance
  expect_true(
    all(
      is.na(
        point_offset(lon = -83.0, lat = 44.0, offsetDir = 'NE')
      )
    )
  )
  
  # No direction
  expect_true(
    all(
      is.na(
        point_offset(lon = -83.0, lat = 44.0, offsetDist = 100)
      )
    )
  )
})
