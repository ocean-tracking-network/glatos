# load example frames
frames <- system.file("extdata", "frames", package = "glatos")

# make video animation
temp_dir <- tempdir()

temp_file <- make_video(
  input_dir = frames,
  intput_ext = ".png",
  output = tempfile(fileext = ".mp4")
)

# Testing file size results
# slow video down by a factor of 10
test_that("slow down gives expected result", {
  temp_file_1 <- tempfile(fileext = ".mp4")

  # check no error
  expect_no_error(
    slow_down <- adjust_playback_time(
      scale_factor = 10,
      input = temp_file,
      output_dir = temp_dir,
      output = basename(temp_file_1)
    )
  )

  # Check if expected and actual file sizes
  expect_equal(
    file.size(slow_down),
    93158,
    tolerance = 0.005 # 0.5% difference
  )
})


# speed up by a factor of 10
test_that("speed up gives expected result", {
  temp_file_2 <- tempfile(fileext = ".mp4")

  expect_no_error(
    speed_up <- adjust_playback_time(
      scale_factor = 0.1,
      input = temp_file,
      output_dir = temp_dir,
      output = basename(temp_file_2)
    )
  )

  # Check if expected and actual file sizes
  expect_equal(
    file.size(speed_up),
    56817,
    tolerance = 0.005 # 0.5% difference
  )
})

# call from input path and name containing spaces and parentheses
test_that("input/output with space/parenth gives expected result", {
  temp_dir_3 <- file.path(temp_dir, "path with ( spaces)")
  dir.create(temp_dir_3)

  temp_file_3 <- tempfile(
    pattern = "fname with ( special)",
    tmpdir = temp_dir_3,
    fileext = ".mp4"
  )

  expect_no_error(
    path_spaces <- adjust_playback_time(
      scale_factor = 0.1,
      input = temp_file,
      output_dir = temp_dir_3,
      output = basename(temp_file_3)
    )
  )

  # Check if expected and actual file sizes
  expect_equal(
    file.size(path_spaces),
    56817,
    tolerance = 0.005
  )
})


# Clean up
unlink(
  list.files(temp_dir,
             full.names = TRUE,
             recursive = TRUE,
             include.dirs = TRUE
  ),
  recursive = TRUE
)

