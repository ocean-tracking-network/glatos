context("Check read_glatos_detections")

#check against internal data object 'receivers_2011' in R/sysdata.r

# Actual result
#get path to example detection file
wd_file <- system.file("extdata", 
  "walleye_detections.csv", package = "glatos")

wd <- read_glatos_detections(wd_file)

# Test using testthat library
test_that("walleye_detections gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(wd, walleye_detections)
})


#get path to example detection file
ld_file <- system.file("extdata", 
  "lamprey_detections.csv", package = "glatos")

ld <- read_glatos_detections(ld_file)

test_that("lamprey_detections gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(ld, lamprey_detections)
})
