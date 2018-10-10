context("Check residence_index")

# Actual result
#get path to example detection file
bsd_file <- system.file("extdata",
                        "blue_shark_detections.csv", package = "glatos")

data <- read_otn_detections(bsd_file)

cdata <- glatos::detection_events(data, location_col = 'station')
rik_data <- glatos::residence_index(cdata, calculation_method = 'kessel')
rit_data <- glatos::residence_index(cdata, calculation_method = 'timedelta')
riawo_data <- glatos::residence_index(cdata, calculation_method = 'aggregate_with_overlap')
riano_data <- glatos::residence_index(cdata, calculation_method = 'aggregate_no_overlap')

# Test using testthat library
test_that("RI for Kessel method gives exepected result on blue sharks", {
  # Check if expected and actual results are the same
  expect_equal(rik_data, blueshark_ri_kessel_data)
})


# Test using testthat library
test_that("RI for timedelta method gives exepected result on blue sharks", {
  # Check if expected and actual results are the same
  expect_equal(rit_data, blueshark_ri_td_data)
})

# Test using testthat library
test_that("RI for Aggregate With Overlap method gives exepected result on blue sharks", {
  # Check if expected and actual results are the same
  expect_equal(riawo_data, blueshark_ri_awo_data)
})

# Test using testthat library
test_that("RI for Aggregate No Overlap method gives exepected result on blue sharks", {
  # Check if expected and actual results are the same
  expect_equal(riano_data, blueshark_ri_ano_data)
})