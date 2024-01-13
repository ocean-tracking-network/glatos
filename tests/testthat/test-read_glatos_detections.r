# check against internal data object 'receivers_2011' in R/sysdata.r


test_that("walleye_detections gives expected result", {
  # get path to example detection file
  wd_file <- system.file(
    "extdata",
    "walleye_detections.csv",
    package = "glatos"
  )

  wd <- read_glatos_detections(wd_file)


  # Check if expected and actual results are the same
  expect_equal(wd, walleye_detections)
})




test_that("lamprey_detections gives expected result", {
  ld_file <- system.file(
    "extdata",
    "lamprey_detections.csv",
    package = "glatos"
  )

  ld <- read_glatos_detections(ld_file)

  # Check if expected and actual results are the same
  expect_equal(ld, lamprey_detections)
})




test_that("lamprey_detections with mixed columns gives expected result", {
  ld_file <- system.file(
    "extdata",
    "lamprey_detections.csv",
    package = "glatos"
  )

  ld <- read_glatos_detections(ld_file)

  ld_mixed_col <- ld[, c(2, 1, 3:27, 29, 28, 30)]

  # write data frame with mixed up columns to temp file
  temp_file <- tempfile()
  write.csv(ld_mixed_col, temp_file, row.names = FALSE)

  # Check if expected and actual results are the same
  expect_equal(
    read_glatos_detections(temp_file),
    ld_mixed_col
  )

  # Clean up
  unlink(
    temp_file
  )
})




# test for some missing animal_id but not all
test_that("lamprey_detections with some missing animal_id expected result", {
  # write data frame with some missing animal_id
  temp_file <- tempfile()
  ld_file <- system.file(
    "extdata",
    "lamprey_detections.csv",
    package = "glatos"
  )

  ld_missing <- read_glatos_detections(ld_file)

  # make two animal_id missing
  ld_missing$animal_id[ld_missing$animal_id %in%
    c("A69-1601-1363", "A69-9002-7189")] <- NA
  write.csv(ld_missing, temp_file, row.names = FALSE)

  expect_warning(
    ld_missing_read_glatos <- read_glatos_detections(temp_file),
    "Some or all values of required column 'animal_id' were missing so they were created from 'transmitter_codespace' and 'transmitter_id'\\."
  )

  # Check if expected and actual results are the same
  expect_equal(
    ld_missing_read_glatos,
    read_glatos_detections(ld_file)
  )

  # Clean up
  unlink(
    temp_file
  )
})
