context("Check convert_glatos_to_att")

# check against internal data object 'walleye_att' in R/sysdata.r

# Actual result
# get path to example detection file
wd_file <- system.file("extdata",
  "walleye_detections.csv",
  package = "glatos"
)

wald <- read_glatos_detections(wd_file)

# get path to example receiver file
rec_file <- system.file("extdata", "sample_receivers.csv",
  package = "glatos"
)
recd <- read_glatos_receivers(rec_file) # load receiver data

watt <- convert_glatos_to_att(wald, recd)



# Test using testthat library
test_that("walleye_att gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(watt, walleye_att)
})
