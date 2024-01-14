test_that("hfx_deployments gives expected result", {
  dep_file <- system.file("extdata",
    "hfx_deployments.csv",
    package = "glatos"
  )

  deps <- read_otn_deployments(dep_file)


  # Check if expected and actual results are the same
  expect_equal(deps, hfx_deployments)
})
