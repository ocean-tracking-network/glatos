context("Check read_glatos_receiver_locations")

# Expected result (example data)
data(receiver_locations_2011)

# Actual result
#get path to example receiver location file
rec_file <- system.file("extdata", 
  "receiver_locations_2011.csv",package="glatos")
rec <- read_glatos_receiver_locations(rec_file)

# Test using testthat library
test_that("read_glatos_receiver_locations gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(rec, receiver_locations_2011)
})
