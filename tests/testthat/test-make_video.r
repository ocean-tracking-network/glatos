context("Check make_video")

# load example frames
frames <- system.file("extdata", "frames", package = "glatos")

# make video animation


test_that("makes videos with expected size", {
  temp_dir <- tempdir()
  temp_file_1 <- tempfile(fileext = ".mp4")
  make_video(
    input_dir = frames,
    input_ext = ".png",
    output = temp_file_1
  )

  # Check if expected and actual file sizes
  expect_equal(
    file.size(temp_file_1),
    72024,
    tolerance = 0.005
  )
})

# call from input path and name containing spaces and parentheses
temp_dir_2 <- file.path(temp_dir, "path with ( spaces)", "frames")
dir.create(temp_dir_2, recursive = TRUE)
file.copy(
  list.files(frames, full.names = TRUE),
  file.path(temp_dir_2, list.files(frames))
)
temp_file_2 <- tempfile(
  pattern = "fname with ( special)",
  tmpdir = temp_dir_2,
  fileext = ".mp4"
)
make_video(
  input_dir = frames,
  input_ext = ".png",
  output = temp_file_2
)

# Actual file sizes
vid_size <- file.info(c(temp_file_1, temp_file_2))$size
# round to nearest MB
vid_size <- round(vid_size * 0.001)

# Expected file sizes
size_should_be <- round(c(72024, 72024) * 0.001)


# Clean up
unlink(list.files(temp_dir,
  full.names = TRUE, recursive = TRUE,
  include.dirs = TRUE
), recursive = TRUE)

# Testing file size results
test_that("making video expected result", {
  # Check if expected and actual file sizes
  expect_equal(vid_size[1], size_should_be[1])
})

test_that("input/output with space/parenth gives expected result", {
  # Check if expected and actual file sizes
  expect_equal(vid_size[2], size_should_be[2])
})
