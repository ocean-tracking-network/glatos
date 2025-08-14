# load example frames
frames <- system.file("extdata", "frames", package = "glatos")

# Create temporary directory
temp_dir <- tempdir()

test_that("makes videos with expected size and returns file name", {
  temp_file_1 <- tempfile(fileext = ".mp4")

  # make video animation
  output <- make_video(
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

  # Returns file name
  expect_equal(
    basename(output),
    basename(temp_file_1)
  )

  # Returns directory
  expect_equal(
    dirname(output),
    dirname(temp_file_1)
  )
})


test_that("input/output with space/parenth gives expected result", {
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

  output <- make_video(
    input_dir = frames,
    input_ext = ".png",
    output = temp_file_2
  )

  # Check if expected and actual file sizes
  expect_equal(
    file.size(temp_file_2),
    72024,
    tolerance = 0.005
  )

  # Returns file name
  expect_equal(
    basename(output),
    basename(temp_file_2)
  )

  # Returns directory
  expect_equal(
    dirname(output),
    dirname(temp_file_2)
  )
})

# Clean up
unlink(
  list.files(
    temp_dir,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = TRUE
  ),
  recursive = TRUE
)
