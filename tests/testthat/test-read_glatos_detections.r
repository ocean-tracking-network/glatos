context("Check read_glatos_detections")

# check against internal data object 'receivers_2011' in R/sysdata.r

# Actual result
# get path to example detection file
wd_file <- system.file("extdata",
  "walleye_detections.csv",
  package = "glatos"
)

wd <- read_glatos_detections(wd_file)

# Test using testthat library
test_that("walleye_detections gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(wd, walleye_detections)
})


# get path to example detection file
ld_file <- system.file("extdata",
  "lamprey_detections.csv",
  package = "glatos"
)

ld <- read_glatos_detections(ld_file)

test_that("lamprey_detections gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(ld, lamprey_detections)
})


# test for mixed up column order
lamprey_detections2 <- ld[, c(2, 1, 3:27, 29, 28, 30)]

# write data frame with mixed up columns to temp file
temp_file <- tempfile()
write.csv(lamprey_detections2, temp_file, row.names = FALSE)

ld2 <- read_glatos_detections(temp_file)

test_that("lamprey_detections with mixed columns gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(ld2, lamprey_detections2)
})


# test for some missing animal_id but not all

# write data frame with some missing animal_id
temp_file3 <- tempfile()
lamprey_detections3 <- ld
# make two animal_id missing
lamprey_detections3$animal_id[lamprey_detections3$animal_id %in%
  c("A69-1601-1363", "A69-9002-7189")] <- NA
write.csv(lamprey_detections3, temp_file3, row.names = FALSE)

ld3 <- suppressWarnings(read_glatos_detections(temp_file3))

test_that("lamprey_detections with some missing anima_id expected result", {
  # Check if expected and actual results are the same
  expect_equal(ld3, ld)
})


# test that warning is correct when animal_id is missing
ld3_w <- tryCatch(read_glatos_detections(temp_file3),
  warning = function(w) {
    return(w$message)
  }
)

w_should_be <- paste(
  "Some or all values of required column 'animal_id' were",
  "missing so they were created from 'transmitter_codespace' and",
  "'transmitter_id'.)"
)

test_that("lamprey_detections with some missing anima_id expected result", {
  # Check if expected and actual results are the same
  expect_equal(ld3_w, w_should_be)
})
