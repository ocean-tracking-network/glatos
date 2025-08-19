test_that("blue_shark_detections gives expected result", {
  # get path to example detection file
  bsd_file <- system.file(
    "extdata",
    "blue_shark_detections_old.csv",
    package = "glatos"
  )

  bsd <- read_otn_detections(bsd_file, format = "old")

  # Check if expected and actual results are the same
  expect_equal(bsd, blue_shark_detections_old)

  expect_s3_class(bsd, "glatos_detections")
  expect_s3_class(bsd, "data.frame")
})

test_that("handles zipped directory with one file", {
  bsd_file <- system.file(
    "extdata",
    "blue_shark_detections_old.csv",
    package = "glatos"
  )

  td <- file.path(tempdir(), "ziptest.zip")
  zip::zip(td, bsd_file, mode = "cherry-pick")

  bsd <- read_otn_detections(td, format = "old")

  # Check if expected and actual results are the same
  expect_equal(bsd, blue_shark_detections_old)

  expect_s3_class(bsd, "glatos_detections")
  expect_s3_class(bsd, "data.frame")

  unlink(td, recursive = TRUE)
})

test_that("handles zipped directory with multiple files", {
  bsd_file <- system.file(
    "extdata",
    "blue_shark_detections_old.csv",
    package = "glatos"
  )

  td <- file.path(tempdir(), "ziptest.zip")
  temp_txt <- file.path(tempdir(), "data_description.txt")
  file.create(temp_txt)
  zip::zip(
    td,
    c(bsd_file, temp_txt),
    mode = "cherry-pick"
  )

  bsd <- read_otn_detections(td, format = "old")

  # Check if expected and actual results are the same
  expect_equal(bsd, blue_shark_detections_old)

  expect_s3_class(bsd, "glatos_detections")
  expect_s3_class(bsd, "data.frame")

  unlink(td, recursive = TRUE)
  unlink(temp_txt)
})
