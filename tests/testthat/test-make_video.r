context("Check make_video")

# load example frames 
frames <- system.file("extdata", "frames", package = "glatos")

# make video animation 
temp_dir <- tempdir()
temp_file_1 <- tempfile(fileext = ".mp4")
make_video(dir = frames, pattern = "%02d.png", output_dir = temp_dir, 
           output = basename(temp_file_1))

# call from input path and name containing spaces and parentheses
temp_dir_2 <- file.path(temp_dir, "path with ( spaces)", "frames")
dir.create(temp_dir_2, recursive = TRUE)
file.copy(list.files(frames, full.names = TRUE), 
          file.path(temp_dir_2, list.files(frames)))
temp_file_2 <- tempfile(pattern = "fname with ( special)", tmpdir = temp_dir_2, 
  fileext = ".mp4")
make_video(dir = frames, pattern = "%02d.png", output_dir = temp_dir_2, 
  output = basename(temp_file_2))

# Actual file sizes
vid_size <- file.info(c(temp_file_1, temp_file_2))$size

# Expected file sizes
size_should_be <- c(68872, 68872)

# Clean up
unlink(list.files(temp_dir, full.names = TRUE, recursive = TRUE,
                  include.dirs = TRUE), recursive = TRUE)

# Testing file size results
test_that("making video expected result", {
  # Check if expected and actual file sizes
  expect_equal(vid_size[1], size_should_be[1])
})

test_that("input/output with space/parenth gives expected result", {
  # Check if expected and actual file sizes
  expect_equal(vid_size[2], size_should_be[2])
})
