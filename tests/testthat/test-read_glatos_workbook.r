context("Check read_glatos_workbook")

# Actual result
#get path to example receiver location file
wb_file <- system.file("extdata", 
  "SMRSL_GLATOS_20140828.xlsm", package="glatos")
wb <- read_glatos_workbook(wb_file)

# Test using testthat library
test_that("metadata element gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(wb[["metadata"]][1:5], lamprey_workbook[["metadata"]][1:5])
})

test_that("receivers element gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(wb["receivers"], lamprey_workbook["receivers"])
})

test_that("animals gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(wb["animals"], lamprey_workbook["animals"])
})