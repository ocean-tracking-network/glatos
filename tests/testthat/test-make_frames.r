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
  
  # Check that file exists
  expect_true(
    file.exists(
      file.path(temp_dir, "1.png")
    )
  )

  # Check file size greater than 30900 bytes
  expect_gt(
    file.size(
      file.path(temp_dir, "1.png")
    ),
    30900
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

  # Check that file exists
  expect_true(
    file.exists(
      file.path(temp_dir, "1.png")
    )
  )
  
  # Check if file size greater than 24000 bytes
  expect_gt(
    file.size(
      file.path(temp_dir, "1.png")
    ),
    24000
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

  # Check that file exists
  expect_true(
    file.exists(
      file.path(temp_dir, "1.png")
    )
  )
  
  # Check if file size greater than 32000 bytes
  expect_gt(
    file.size(
      file.path(temp_dir, "1.png")
    ),
    32000
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

  # Check that file exists
  expect_true(
    file.exists(
      file.path(temp_dir, "1.png")
    )
  )
  
  # Check if file size greater than 24000 bytes
  expect_gt(
    file.size(
      file.path(temp_dir, "1.png")
    ),
    24000
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
