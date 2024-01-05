context("Check read_otn_deployments")

# Actual result
# get path to example deployment file
dep_file <- system.file("extdata",
  "hfx_deployments.csv",
  package = "glatos"
)

deps <- read_otn_deployments(dep_file)

# Test using testthat library
test_that("hfx_deployments gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(deps, hfx_deployments)
})
