# R/simutil-rotate_points.r

test_that("rotate_points works", {
  rp_shouldbe <- structure(
    list(
      x = c(-3, 2.19615242270663),
      y = c(0, -3)
    ),
    class = "data.frame",
    row.names = c(NA, -2L)
  )

  expect_equal(
    rotate_points(
      x = c(-3, 3),
      y = c(0, 0),
      theta = 30,
      focus = c(-3, 0)
    ),
    rp_shouldbe
  )
})
