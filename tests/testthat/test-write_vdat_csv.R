# R/load-write_vdat_csv.r

skip_on_ci()
skip_on_cran()

test_that("write_vdat_csv works", {
  # VR2W file
  vrl_file <- system.file(
    "extdata",
    "detection_files_raw",
    "VR2W_109924_20110718_1.vrl",
    package = "glatos"
  )

  temp_dir <- tempdir()

  csv_file <- suppressMessages(vdat_convert(
    vrl_file,
    out_dir = temp_dir,
    show_progress = FALSE
  ))

  # read all record types
  vdat <- read_vdat_csv(csv_file)

  # write to disk; default args
  temp_file <- tempfile(fileext = ".csv")

  expect_type(
    out_name <- write_vdat_csv(vdat, out_file = temp_file),
    "character"
  )

  # read in the new file
  vdat2 <- read_vdat_csv(temp_file)

  # compare to vdat created by fathom vdat.exe
  expect_equal(vdat, vdat2, ignore_attr = TRUE)

  # compare attributes
  expect_equal(
    attributes(vdat)[c("names", "class", "fathom_csv_version")],
    attributes(vdat2)[c("names", "class", "fathom_csv_version")]
  )

  # check unique attribute
  expect_equal(
    attributes(vdat2)[["source"]],
    paste0("glatos-", packageVersion("glatos"))
  )

  # out_file is dir
  expect_equal(
    write_vdat_csv(vdat, out_file = temp_dir),
    csv_file
  )

  # specify output record_types
  expect_equal(
    write_vdat_csv(
      vdat,
      record_types = c("DET", "EVENT_OFFLOAD", "DATA_SOURCE_FILE"),
      out_file = temp_dir
    ),
    csv_file
  )

  # "split" output format
  # write to multiple files (fathom split option)
  split_dir <- paste0(csv_file, "-fathom-split")
  expect_equal(
    write_vdat_csv(
      vdat,
      out_file = temp_dir,
      output_format = "csv.fathom.split"
    ),
    split_dir
  )

  # "split" output format
  # out_file is dir that does not exist
  new_dir <- file.path(temp_dir, "newdir")
  expect_type(
    write_vdat_csv(
      vdat,
      out_file = new_dir,
      output_format = "csv.fathom.split"
    ),
    "character"
  )

  # clean up
  on.exit(unlink(
    c(
      csv_file,
      temp_file,
      new_dir,
      split_dir
    ),
    recursive = TRUE
  ))

  # tests to skip
  skip("skip writing to wd")

  expect_type(write_vdat_csv(vdat), "character")
})


test_that("bad inputs are caught", {
  # VR2W file
  vrl_file <- system.file(
    "extdata",
    "detection_files_raw",
    "VR2W_109924_20110718_1.vrl",
    package = "glatos"
  )

  temp_dir <- tempdir()

  csv_file <- suppressMessages(vdat_convert(
    vrl_file,
    out_dir = temp_dir,
    show_progress = FALSE
  ))

  # read all record types
  vdat <- read_vdat_csv(csv_file)

  # Input not of class "vdat_list"
  temp_file <- tempfile(fileext = ".csv")
  expect_error(
    write_vdat_csv(unclass(vdat), out_file = temp_file),
    "Input 'vdat' must have class 'vdat_list'"
  )

  # out_file is dir but vdat missing DATA_SOURCE_FILE record
  expect_error(
    write_vdat_csv(
      vdat[c("DET", "EVENT_OFFLOAD")],
      out_file = file.path(temp_dir, "newdir"),
      output_format = "csv.fathom.split"
    ),
    "Input 'out_file' must include file name if 'vdat' does not contain"
  )

  on.exit(unlink(csv_file))
})


# Check format_POSIXt

test_that("format_POSIXt() works", {
  # POSIXct input
  t1 <- as.POSIXct(
    c(
      "2011-03-08 23:59:58",
      "2011-03-08 23:59:58.828867"
    ),
    tz = "UTC"
  )

  expect_equal(
    format_POSIXt(t1, digits = 5, drop0trailing = FALSE),
    c("2011-03-08 23:59:58.00000", "2011-03-08 23:59:58.82887")
  )

  expect_equal(
    format_POSIXt(t1, digits = 3, drop0trailing = TRUE),
    c("2011-03-08 23:59:58", "2011-03-08 23:59:58.829")
  )

  expect_equal(
    format_POSIXt(t1, digits = 0),
    c("2011-03-08 23:59:58", "2011-03-08 23:59:59")
  )

  # POSIXlt input
  t2 <- as.POSIXlt(t1)

  expect_equal(
    format_POSIXt(t2, digits = 5, drop0trailing = FALSE),
    c("2011-03-08 23:59:58.00000", "2011-03-08 23:59:58.82887")
  )

  expect_equal(
    format_POSIXt(t2, digits = 3, drop0trailing = TRUE),
    c("2011-03-08 23:59:58", "2011-03-08 23:59:58.829")
  )

  expect_equal(
    format_POSIXt(t2, digits = 0),
    c("2011-03-08 23:59:58", "2011-03-08 23:59:59")
  )

  # catch non-posix input

  expect_error(
    format_POSIXt(as.character(t1)),
    "class of input 'x' must be 'POSIXct' or 'POSIXlt'"
  )
})


# vdat_list subset method
test_that("vdat_list subset method works", {
  # VR2W file
  vrl_file <- system.file(
    "extdata",
    "detection_files_raw",
    "VR2W_109924_20110718_1.vrl",
    package = "glatos"
  )

  temp_dir <- tempdir()

  csv_file <- suppressMessages(vdat_convert(
    vrl_file,
    out_dir = temp_dir,
    overwrite = TRUE,
    show_progress = FALSE
  ))

  # read all record types
  vdat <- read_vdat_csv(csv_file)

  # subset by name
  record_types_names <- c("DET", "EVENT_INIT", "EVENT_OFFLOAD")

  vdat2 <- vdat[record_types_names]

  expect_s3_class(vdat2, "vdat_list")
  expect_equal(names(vdat2), record_types_names)

  # subset by number
  vdat3 <- vdat[match(record_types_names, names(vdat))]

  expect_s3_class(vdat3, "vdat_list")
  expect_equal(names(vdat3), record_types_names)

  # clean up
  on.exit(unlink(csv_file))
})
