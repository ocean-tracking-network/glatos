context("Check read_vemco_tag_specs")

# Actual result
# get path to example tag spec file
tag_file <- system.file("extdata",
  "lamprey_tag_specs.xls",
  package = "glatos"
)
tag_specs <- read_vemco_tag_specs(tag_file, file_format = "vemco_xls")


# Test using testthat library
test_that("'specs' element gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(tag_specs["specs"], lamprey_tag_specs["specs"])
})

test_that("'schedule' element gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(tag_specs["schedule"], lamprey_tag_specs["schedule"])
})
