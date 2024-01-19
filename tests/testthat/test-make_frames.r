context("Check make_frames")

# make example records
pos1 <- structure(list(animal_id = c("153", "153", "153", "153"), 
  bin_timestamp = structure(c(1335704727, 
  1335704727, 1335791127, 1335877527), class = c("POSIXct", "POSIXt"
  ), tzone = "UTC"), latitude = c(43.60963, 43.61235, 43.6157011174478, 
    43.6404351467905), longitude = c(-83.88658, -83.8608, -83.858826537583, 
      -83.8442607462938), record_type = c("detection", "detection", 
        "interpolated", "interpolated")), row.names = 4:7, class = "data.frame")

# Check output with default background_ylim, background_xlim, and bg_map
# make a preview image
temp_dir <- tempdir()
make_frames(pos1, out_dir=temp_dir, preview = TRUE)

# Expected file size
size_should_be1 <- 30919

# Actual file sizes
img_file1 <- file.path(temp_dir, "1.png")
img_size1 <- file.info(img_file1)$size

# Clean up
unlink(list.files(temp_dir, full.names = TRUE, recursive = TRUE,
                  include.dirs = TRUE), recursive = TRUE)

# Check output with default background_ylim, background_xlim, but not bg_map
# make a preview image
data(great_lakes_polygon)

temp_dir <- tempdir()
make_frames(pos1, out_dir=temp_dir, preview = TRUE, 
            bg_map = great_lakes_polygon)

# Expected file size
size_should_be2 <- 23832

# Actual file sizes
img_file2 <- file.path(temp_dir, "1.png")
img_size2 <- file.info(img_file2)$size

# Clean up
unlink(list.files(temp_dir, full.names = TRUE, recursive = TRUE,
                  include.dirs = TRUE), recursive = TRUE)

# Check output with specified background_ylim, background_xlim, and bg_map
# make a preview image
data(great_lakes_polygon)

temp_dir <- tempdir()
make_frames(pos1, 
            out_dir = temp_dir, 
            background_ylim = c(42, 47),
            background_xlim = c(-90, -78),
            preview = TRUE, 
            bg_map = great_lakes_polygon)

# Expected file size
size_should_be3 <- 32220

# Actual file sizes
img_file3 <- file.path(temp_dir, "1.png")
img_size3 <- file.info(img_file3)$size

# Clean up
unlink(list.files(temp_dir, full.names = TRUE, recursive = TRUE,
                  include.dirs = TRUE), recursive = TRUE)


# Testing file size results
test_that("Expected result when background lims and map not supplied", {
  # Check if expected and actual file sizes
  expect_equal(img_size1, size_should_be1)
})

test_that("Expected result when map but not background lims supplied", {
  # Check if expected and actual file sizes
  expect_equal(img_size2, size_should_be2)
})

test_that("Expected result when map and background lims supplied", {
  # Check if expected and actual file sizes
  expect_equal(img_size3, size_should_be3)
})
