context("Check read_glatos_receivers")

#check against internal data object 'receivers_2011' in R/sysdata.r

# Actual result
#get path to example receiver location file
rec_file <- system.file("extdata", 
  "receiver_locations_2011.csv",package="glatos")
rec <- read_glatos_receivers(rec_file)

# Test using testthat library
test_that("read_glatos_receivers gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(rec, receivers_2011)
})
