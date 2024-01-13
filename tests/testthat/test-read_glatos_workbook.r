
test_that("metadata element gives expected result", {
  wb_file <- system.file("extdata",
    "walleye_workbook.xlsm",
    package = "glatos"
  )
  wb <- read_glatos_workbook(wb_file)

  expect_type(wb[["metadata"]], "list")

  # Check if expected and actual results are the same
  expect_equal(wb[["metadata"]][1:5], walleye_workbook[["metadata"]][1:5])
})

test_that("receivers element gives expected result", {
  wb_file <- system.file("extdata",
    "walleye_workbook.xlsm",
    package = "glatos"
  )
  wb <- read_glatos_workbook(wb_file)

  expect_s3_class(wb[["receivers"]], "glatos_receivers")
  expect_s3_class(wb[["receivers"]], "data.frame")

  # Check if expected and actual results are the same
  expect_equal(wb[["receivers"]], walleye_workbook[["receivers"]])
})

test_that("animals gives expected result", {
  wb_file <- system.file("extdata",
    "walleye_workbook.xlsm",
    package = "glatos"
  )
  wb <- read_glatos_workbook(wb_file)

  expect_s3_class(wb[["animals"]], "glatos_animals")
  expect_s3_class(wb[["animals"]], "data.frame")

  # Check if expected and actual results are the same
  expect_equal(wb[["animals"]], walleye_workbook[["animals"]])
})
