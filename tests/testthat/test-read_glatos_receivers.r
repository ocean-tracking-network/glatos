context("Check read_glatos_receivers")

#check against internal data object 'glatos_receivers' in R/sysdata.r

# Actual result
#get path to example receiver location file
rec_file <- system.file("extdata", 
  "sample_receivers.csv", package = "glatos")
rec <- read_glatos_receivers(rec_file)

# Test using testthat library
test_that("read_glatos_receivers gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(rec, sample_receivers)
})

#test for mixed up column order
sample_receivers2 <- rec[, c(1:8, 10, 9, 11:23)]

#write data frame with mixed up columns to temp file
temp_file <- tempfile()
write.csv(sample_receivers2, temp_file, row.names = FALSE)

rec2 <- read_glatos_receivers(temp_file)


test_that("read_glatos_receivers with mixed columns gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(rec2, sample_receivers2)
})

