context("Check adjust_playback_time")

# load example frames 
frames <- system.file("extdata", "frames", package = "glatos")

# make video animation 
temp_dir <- tempdir()
temp_file <- tempfile(fileext = ".mp4")
make_video(dir = frames, pattern = "%02d.png", output_dir = temp_dir, 
           output = basename(temp_file))

# slow video down by a factor of 10 
temp_file_1 <- tempfile(fileext = ".mp4")
adjust_playback_time(scale_factor = 10, input = temp_file, 
                     output_dir = temp_dir, output = basename(temp_file_1))

# speed up by a factor of 10
temp_file_2 <- tempfile(fileext = ".mp4")
adjust_playback_time(scale_factor = 0.1, input = temp_file, 
  output_dir = temp_dir, output = basename(temp_file_2))

# call from input path and name containing spaces and parentheses
temp_dir_3 <- file.path(temp_dir, "path with ( spaces)")
dir.create(temp_dir_3)
temp_file_3 <- tempfile(pattern = "fname with ( special)", tmpdir = temp_dir_3, 
                       fileext = ".mp4")
adjust_playback_time(scale_factor = 0.1, input = temp_file, 
  output_dir = temp_dir_3, output = basename(temp_file_3))

# Actual file sizes
vid_size <- file.info(c(temp_file_1, temp_file_2, temp_file_3))$size
# round to MB
vid_size <- round(vid_size * 0.001)

# Expected file sizes
size_should_be <- round(c(166532, 48196, 48196) * 0.001)

# Clean up
unlink(list.files(temp_dir, full.names = TRUE, recursive = TRUE,
                  include.dirs = TRUE), recursive = TRUE)

# Testing file size results
test_that("slow down gives expected result", {
  # Check if expected and actual file sizes
  expect_equal(vid_size[1], size_should_be[1])
})

test_that("speed up gives expected result", {
  # Check if expected and actual file sizes
  expect_equal(vid_size[2], size_should_be[2])
})

test_that("input/output with space/parenth gives expected result", {
  # Check if expected and actual file sizes
  expect_equal(vid_size[3], size_should_be[3])
})
