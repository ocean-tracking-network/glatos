test_that("returns a list", {
  # get path to example tag spec file
  tag_file <- system.file(
    "extdata",
    "lamprey_tag_specs.xls",
    package = "glatos"
  )

  tag_specs <- read_vemco_tag_specs(tag_file, file_format = "vemco_xls")

  expect_type(tag_specs, "list")
  expect_named(tag_specs, c("specs", "schedule"))
})


test_that("'specs' element gives expected result", {
  # get path to example tag spec file
  tag_file <- system.file(
    "extdata",
    "lamprey_tag_specs.xls",
    package = "glatos"
  )
  tag_specs <- read_vemco_tag_specs(tag_file, file_format = "vemco_xls")

  # Check if expected and actual results are the same
  expect_equal(tag_specs[["specs"]], lamprey_tag_specs[["specs"]])
  expect_s3_class(tag_specs[["specs"]], "data.frame")
})


test_that("'schedule' element gives expected result", {
  # get path to example tag spec file
  tag_file <- system.file(
    "extdata",
    "lamprey_tag_specs.xls",
    package = "glatos"
  )
  tag_specs <- read_vemco_tag_specs(tag_file, file_format = "vemco_xls")

  # Check if expected and actual results are the same
  expect_equal(tag_specs[["schedule"]], lamprey_tag_specs[["schedule"]])
  expect_s3_class(tag_specs[["schedule"]], "data.frame")
})
