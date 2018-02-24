context("Check read_otn_detections")

# Actual result
#get path to example detection file
bsd_file <- system.file("extdata",
  "blue_shark_detections.csv", package = "glatos")

bsd <- read_otn_detections(bsd_file)

# Test using testthat library
test_that("blue_shark_detections gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(bsd, blue_shark_detections)
})
