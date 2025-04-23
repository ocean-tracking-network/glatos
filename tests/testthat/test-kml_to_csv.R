test_that("errors with kmz", {
  expect_error(
    kml_to_csv("some_kmz_file.kmz"),
    "kmz are not supported"
  )
})

test_that("creates csv", {
  temp_dir <- file.path(tempdir(), "test-kml_to_csv")
  dir.create(temp_dir)
  result_of_copy <- file.copy(
    system.file("extdata", "example_polygons.kml", package = "glatos"),
    temp_dir
  )

  out_loc <- kml_to_csv(file.path(temp_dir, "example_polygons.kml"))

  expect_equal(
    out_loc,
    file.path(temp_dir, "example_polygons.csv")
  )

  expect_true(
    file.exists(file.path(temp_dir, "example_polygons.csv"))
  )

  expect_snapshot(
    read.csv(
      file.path(temp_dir, "example_polygons.csv")
    )
  )

  unlink(temp_dir, recursive = T)
})
