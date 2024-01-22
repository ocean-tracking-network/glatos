# make example records
pos <- interpolate_path(
  walleye_detections[walleye_detections$animal_id == 153, ][
    1:125,
  ],
  start_time = as.POSIXct("2012-04-29 13:05:27")
)[1:4, ]


test_that("Expected result when background lims and map not supplied", {
  # Check output with default background_ylim, background_xlim, and bg_map
  temp_dir <- tempdir()

  expect_message(
    make_frames(pos, out_dir = temp_dir, preview = TRUE),
    "Preview frames written to"
  )

  # Check if expected and actual file sizes
  expect_equal(
    file.size(
      file.path(temp_dir, "1.png")
    ),
    30919,
    tolerance = 0.01
  )

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
})

test_that("Expected result when map but not background lims supplied", {
  temp_dir <- tempdir()

  expect_message(
    make_frames(
      pos,
      out_dir = temp_dir,
      preview = TRUE,
      bg_map = great_lakes_polygon
    ),
    "Preview frames written to"
  )

  # Check if expected and actual file sizes
  expect_equal(
    file.size(
      file.path(temp_dir, "1.png")
    ),
    24300,
    tolerance = 0.01
  )

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
})

test_that("Expected result when map and background lims supplied", {
  temp_dir <- tempdir()

  expect_message(
    make_frames(pos,
      out_dir = temp_dir,
      background_ylim = c(42, 47),
      background_xlim = c(-90, -78),
      preview = TRUE,
      bg_map = great_lakes_polygon
    ),
    "Preview frames written to"
  )

  # Check if expected and actual file sizes
  expect_equal(
    file.size(
      file.path(temp_dir, "1.png")
    ),
    32220,
    tolerance = 0.01
  )

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
})

test_that("Expected result when map is spatVector, bg lims not supplied", {
  sv_poly <- terra::vect(great_lakes_polygon)

  temp_dir <- tempdir()
  expect_message(
    expect_message(
      make_frames(pos,
        out_dir = temp_dir,
        preview = TRUE,
        bg_map = sv_poly
      ),
      "Converted terra object to sf"
    ),
    "Preview frames written to"
  )


  expect_equal(
    file.size(
      file.path(temp_dir, "1.png")
    ),
    24000,
    tolerance = 0.02
  )


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
})
