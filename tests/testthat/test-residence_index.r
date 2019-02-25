context("Check residence_index")

# Actual result
#get path to example detection file
bsd_file <- system.file("extdata",
                        "blue_shark_detections.csv", package = "glatos")

data <- read_otn_detections(bsd_file)

cdata <- glatos::detection_events(data, location_col = 'station')
rik_data <- glatos::residence_index(cdata, calculation_method = 'kessel', group_col = NULL)
rit_data <- glatos::residence_index(cdata, calculation_method = 'timedelta', group_col = NULL)
riawo_data <- glatos::residence_index(cdata, calculation_method = 'aggregate_with_overlap', group_col = NULL)
riano_data <- glatos::residence_index(cdata, calculation_method = 'aggregate_no_overlap', group_col = NULL)

#note that these are just checking RI values after sort for now since 
# structure of object returned by residence_index recently changed
# group_col = NULL was added to each call above for same reason

# Test using testthat library
test_that("RI for Kessel method gives exepected result on blue sharks", {
  # Check if expected and actual results are the same
  expect_equal(sort(rik_data$residency_index), sort(blueshark_ri_kessel_data$residency_index))
})


# Test using testthat library
test_that("RI for timedelta method gives exepected result on blue sharks", {
  # Check if expected and actual results are the same
  expect_equal(sort(rit_data$residency_index), sort(blueshark_ri_td_data$residency_index))
})

# Test using testthat library
test_that("RI for Aggregate With Overlap method gives exepected result on blue sharks", {
  # Check if expected and actual results are the same
  expect_equal(sort(riawo_data$residency_index), sort(blueshark_ri_awo_data$residency_index))
})

# Test using testthat library
test_that("RI for Aggregate No Overlap method gives exepected result on blue sharks", {
  # Check if expected and actual results are the same
  expect_equal(sort(riano_data$residency_index), sort(blueshark_ri_ano_data$residency_index))
})