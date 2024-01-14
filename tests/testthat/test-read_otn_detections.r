test_that("blue_shark_detections gives expected result", {
  # get path to example detection file
  bsd_file <- system.file(
    "extdata",
    "blue_shark_detections.csv",
    package = "glatos"
  )

  bsd <- read_otn_detections(bsd_file)

  # Check if expected and actual results are the same
  expect_equal(bsd, blue_shark_detections)

  expect_s3_class(bsd, 'glatos_detections')
  expect_s3_class(bsd, 'data.frame')
})
