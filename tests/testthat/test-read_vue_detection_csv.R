# R/load-read_vue_detection_csv.r

test_that("read_vue_detection_csv works", {
  # VR2W file
  vue_csv_file <- system.file("extdata", "VR2W_109924_20110718_1.csv",
    package = "glatos"
  )

  # read all record types
  expect_snapshot(read_vue_detection_csv(vue_csv_file))
})


test_that("bad inputs are caught", {
  # Input file not found
  not_a_file <- tempfile()
  expect_warning(
    read_vue_detection_csv(not_a_file),
    "File not found:"
  )


  # VUE CSV is missing columns
  vue_csv_file <- system.file("extdata", "VR2W_109924_20110718_1.csv",
    package = "glatos"
  )

  vue <- read_vue_detection_csv(vue_csv_file)

  temp_file <- tempfile()

  write.csv(vue[1:5, -2], temp_file, row.names = FALSE)

  expect_error(
    read_vue_detection_csv(temp_file),
    "Input file does not appear to be in VUE Export format"
  )
})
