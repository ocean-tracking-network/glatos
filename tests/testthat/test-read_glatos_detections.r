context("Check read_glatos_detections")

#check against internal data object 'receivers_2011' in R/sysdata.r

# Actual result
#get path to example detection file
det_file <- system.file("extdata", 
  "walleye_detections.csv",package="glatos")

det <- read_glatos_detections(det_file)

# Test using testthat library
test_that("read_glatos_detections gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(det, walleye_detections)
})
