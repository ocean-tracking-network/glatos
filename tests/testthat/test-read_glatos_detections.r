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


#test for mixed up column order
lamprey_detections2 <- ld[, c(2, 1, 3:27, 29, 28, 30)]

#write data frame with mixed up columns to temp file
temp_file <- tempfile()
write.csv(lamprey_detections2, temp_file, row.names = FALSE)

ld2 <- read_glatos_detections(temp_file)


test_that("lamprey_detections with mixed columns gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(ld2, lamprey_detections2)
})


