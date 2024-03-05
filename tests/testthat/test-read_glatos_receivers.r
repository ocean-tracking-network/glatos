# check against internal data object 'sample_receivers' in R/sysdata.r

# Actual result
test_that("read_glatos_receivers gives expected result", {
  rec_file <- system.file(
    "extdata",
    "sample_receivers.csv",
    package = "glatos"
  )
  rec <- read_glatos_receivers(rec_file)

  # Check if expected and actual results are the same
  expect_equal(rec, sample_receivers)
})




# test for mixed up column order
test_that("read_glatos_receivers with mixed columns gives expected result", {
  rec_file <- system.file(
    "extdata",
    "sample_receivers.csv",
    package = "glatos"
  )
  rec <- read_glatos_receivers(rec_file)

  sample_receivers_muco <- rec[, c(1:8, 10, 9, 11:23)]

  # write data frame with mixed up columns to temp file
  temp_file <- tempfile()
  write.csv(sample_receivers_muco, temp_file, row.names = FALSE)

  recx_muco <- read_glatos_receivers(temp_file)

  # Check if expected and actual results are the same
  expect_equal(recx_muco, sample_receivers_muco)

  # Clean up
  unlink(
    temp_file
  )
})


# test for GLATOS receiver_locations file mod. Jan. 2023 (add code_map cols)
# Test using testthat library
test_that("read_glatos_receivers2 gives expected result", {
  # get path to example receiver location file
  rec2_file <- system.file(
    "extdata",
    "sample_receivers2.csv",
    package = "glatos"
  )
  rec2 <- read_glatos_receivers(rec2_file)

  # Check if expected and actual results are the same
  expect_equal(rec2, sample_receivers2)
})
