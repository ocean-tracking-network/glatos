pos <- interpolate_path(
  walleye_detections[walleye_detections$animal_id == 153, ][
    1:125,
  ]
)[3:6, ]

temp_dir <- tempdir()

# Testing file size results
test_that("making preview image expected result", {
  # make preview image
  expect_message(
    make_frames(pos, out_dir = temp_dir, preview = TRUE),
    "Preview frames written to"
  )

  # Check if expected and actual file sizes
  expect_equal(
    file.size(
      file.path(temp_dir, "1.png")
    ),
    30799
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
