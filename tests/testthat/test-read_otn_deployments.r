test_that("hfx_deployments gives expected result", {
  skip("Skipping until issue #202 is resolved.")

  dep_file <- system.file("extdata", "hfx_deployments.csv", package = "glatos")

  deps <- read_otn_deployments(dep_file, validate = FALSE)[1:5, ]

  # Check if expected and actual results are the same
  expect_equal(deps, hfx_deployments)
})
