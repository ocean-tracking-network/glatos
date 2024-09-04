# R/load-read_vdat_csv.r

skip_on_cran()
skip_on_ci()

test_that("read_vdat_csv works", {
  # VR2W file
  vrl_file <- system.file("extdata", "detection_files_raw",
    "VR2W_109924_20110718_1.vrl",
    package = "glatos"
  )

  temp_dir <- tempdir()

  csv_file <- suppressMessages(vdat_convert(vrl_file,
    out_dir = temp_dir,
    show_progress = FALSE
  ))

  # read all record types
  expect_snapshot(vdat <- read_vdat_csv(csv_file))


  # read only DET record type
  expect_equal(
    read_vdat_csv(csv_file, record_types = c("DET"))$DET,
    vdat$DET
  )

  # clean up
  on.exit(unlink(csv_file))
})


test_that("bad inputs are caught", {
  # Input file not found
  not_a_file <- tempfile()
  expect_warning(
    read_vdat_csv(not_a_file),
    "File not found:"
  )


  # VUE export format
  vue_csv_file <- system.file("extdata", "VR2W_109924_20110718_1.csv",
    package = "glatos"
  )

  expect_error(
    read_vdat_csv(vue_csv_file),
    "Input file appears to be in VUE Export format"
  )


  # Specified record_type not found in VDAT CSV
  temp_file <- tempfile()

  vdat2 <- c(
    "VEMCO DATA LOG,2.0.0,vdat-9.3.0-20240207-74ad8e-release",
    "RECORD TYPE,FIELD,FIELD,FIELD,FIELD,FIELD,FIELD,FIELD",
    "UNRECOGNIZED_DESC,Device Time (UTC),Time",
    "UNRECOGNIZED,2020-01-01 01:01:01,2020-01-01 01:01:01"
  )

  writeLines(vdat2, temp_file)

  expect_error(
    read_vdat_csv(temp_file, record_types = "DET"),
    "The following input 'record_types' were not found in CSV file"
  )


  # clean up
  on.exit(unlink(temp_file))
})
