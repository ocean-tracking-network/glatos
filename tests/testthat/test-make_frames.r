context("Check make_frames")

# make example records
pos1 <- structure(list(animal_id = c("153", "153", "153", "153"), 
  bin_timestamp = structure(c(1335704727, 
  1335704727, 1335791127, 1335877527), class = c("POSIXct", "POSIXt"
  ), tzone = "UTC"), latitude = c(43.60963, 43.61235, 43.6157011174478, 
    43.6404351467905), longitude = c(-83.88658, -83.8608, -83.858826537583, 
      -83.8442607462938), record_type = c("detection", "detection", 
        "interpolated", "interpolated")), row.names = 4:7, class = "data.frame")

# make preview image 
temp_dir <- tempdir()
make_frames(pos1, out_dir=temp_dir, preview = TRUE)

# Actual file sizes
img_file <- file.path(temp_dir, "1.png")
img_size <- file.info(img_file)$size

# Expected file sizes
size_should_be <- 30919

# Clean up
unlink(list.files(temp_dir, full.names = TRUE, recursive = TRUE,
                  include.dirs = TRUE), recursive = TRUE)

# Testing file size results
test_that("making preview image expected result", {
  # Check if expected and actual file sizes
  expect_equal(img_size, size_should_be)
})

