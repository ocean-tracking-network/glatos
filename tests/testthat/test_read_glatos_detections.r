context("Check read_glatos_detections")

# Expected result (example data)
data(walleye_detections)

# Actual result
#get path to example detection file
det_file <- system.file("extdata", 
  "walleye_detections.zip",package="glatos")
det_file <- unzip(det_file, "walleye_detections.csv")
det <- read_glatos_detections(det_file)
file.remove(det_file) #clean up

# Test using testthat library
test_that("read_glatos_detections gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(det, walleye_detections)
})
