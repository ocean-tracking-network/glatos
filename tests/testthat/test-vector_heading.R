# R/simutil-vector_heading.r

test_that("vector_heading works", {
  # generic cartesian input; one vector
  expect_equal(
    vector_heading(
      x = c(2, 4),
      y = c(2, 4)
    ),
    45L
  )

  # generic cartesian input; two vectors
  expect_equal(
    vector_heading(
      x = c(2, 4, 2),
      y = c(2, 4, 2)
    ),
    c(45L, 225L)
  )

  # use the x, y input method way
  expect_equal(
    vector_heading(
      x = c(-92.1005, -79.3832, -83.0458),
      y = c(46.7867, 43.6532, 42.3314),
      coord_sys = "longlat"
    ),
    c(104.67999972425, 244.995839411647)
  )

  # use the x-only input method

  path1 <- data.frame(
    city = c("Duluth", "Toronoto", "Detroit"),
    longitude = c(-92.1005, -79.3832, -83.0458),
    latitude = c(46.7867, 43.6532, 42.3314)
  )

  expect_equal(
    vector_heading(
      x = path1[, c("longitude", "latitude")],
      coord_sys = "longlat"
    ),
    c(104.67999972425, 244.995839411647)
  )
})
