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


test_that("matches internal data: walleye_att", {
  expect_no_error(
    watt <- convert_glatos_to_att(wald, recd)
  )

  # Check if expected and actual results are the same
  expect_identical(watt, walleye_att)
})

test_that("matches type/class of internal data: walleye_att", {
  watt <- convert_glatos_to_att(wald, recd)

  expect_s3_class(watt, "ATT")
  expect_type(watt, "list")
})
