# Testing output gives expected values
test_that("full vps data set gives expected result", {
  phm_shouldBe <- phm_values_known()

  # Data returned invisibly
  expect_invisible(
    phm_full_input <- position_heat_map(
      lamprey_tracks,
      x_limits = c(-84.14, -84.12),
      y_limits = c(46.45, 46.47),
      resolution = 100
    )
  )


  # Check if expected and actual results are the same
  expect_equal(phm_full_input$values, phm_shouldBe)


  # Check classes and names
  expect_named(
    phm_full_input,
    c("values", "utm_zone", "bbox_UTM", "bbox_LL", "function_call")
  )
  expect_type(phm_full_input, "list")
  expect_type(phm_full_input$utm_zone, "character")
  expect_s3_class(phm_full_input$bbox_UTM, "data.frame")
  expect_s3_class(phm_full_input$bbox_LL, "data.frame")
  expect_type(phm_full_input$function_call, "language")
})



test_that("data frame with min required columns gives expected result", {
  phm_shouldBe <- phm_values_known()

  expect_invisible(
    phm_reduced_input <- position_heat_map(
      lamprey_tracks[, c(
        "DETECTEDID",
        "DATETIME",
        "LAT",
        "LON"
      )],
      x_limits = c(-84.14, -84.12),
      y_limits = c(46.45, 46.47),
      resolution = 100
    )
  )

  # Check if expected and actual results are the same
  expect_equal(phm_reduced_input$values, phm_shouldBe)

  # Check classes and names
  expect_named(
    phm_reduced_input,
    c("values", "utm_zone", "bbox_UTM", "bbox_LL", "function_call")
  )
  expect_type(phm_reduced_input, "list")
  expect_type(phm_reduced_input$utm_zone, "character")
  expect_s3_class(phm_reduced_input$bbox_UTM, "data.frame")
  expect_s3_class(phm_reduced_input$bbox_LL, "data.frame")
  expect_type(phm_reduced_input$function_call, "language")
})



test_that("data.table input gives expected result", {
  phm_shouldBe <- phm_values_known()

  expect_invisible(
    phm_dt_input <- position_heat_map(
      data.table::as.data.table(lamprey_tracks),
      x_limits = c(-84.14, -84.12),
      y_limits = c(46.45, 46.47),
      resolution = 100
    )
  )

  # Check if expected and actual results are the same
  expect_equal(phm_dt_input$values, phm_shouldBe)

  # Check classes and names
  expect_named(
    phm_dt_input,
    c("values", "utm_zone", "bbox_UTM", "bbox_LL", "function_call")
  )
  expect_type(phm_dt_input, "list")
  expect_type(phm_dt_input$utm_zone, "character")
  expect_s3_class(phm_dt_input$bbox_UTM, "data.frame")
  expect_s3_class(phm_dt_input$bbox_LL, "data.frame")
  expect_type(phm_dt_input$function_call, "language")
})



test_that("png output default name gives expected result", {
  phm_shouldBe <- phm_values_known()
  temp_dir <- tempdir()

  # Test png out
  expect_message(
    phm_png_out <- position_heat_map(
      lamprey_tracks,
      x_limits = c(-84.14, -84.12),
      y_limits = c(46.45, 46.47),
      resolution = 100,
      output = "png",
      folder = temp_dir
    ),
    "Output file is located in"
  )

  expect_invisible(
    phm_png_out <- position_heat_map(
      lamprey_tracks,
      x_limits = c(-84.14, -84.12),
      y_limits = c(46.45, 46.47),
      resolution = 100,
      output = "png",
      folder = temp_dir
    )
  )

  # Check if png file produced
  expect_true(
    file.exists(
      file.path(
        temp_dir,
        "fish_absolute.png"
      )
    )
  )

  expect_equal(phm_png_out$values, phm_shouldBe)

  # Clean up
  unlink(
    file.path(
      temp_dir,
      "fish_absolute.png"
    )
  )
})




test_that("png output custom name gives expected result", {
  phm_shouldBe <- phm_values_known()
  temp_dir <- tempdir()

  expect_message(
    phm_png_out_named <- position_heat_map(
      lamprey_tracks,
      x_limits = c(-84.14, -84.12),
      y_limits = c(46.45, 46.47),
      resolution = 100,
      output = "png",
      folder = temp_dir,
      out_file = "mymap"
    ),
    "Output file is located in"
  )

  expect_invisible(
    phm_png_out_named <- position_heat_map(
      lamprey_tracks,
      x_limits = c(-84.14, -84.12),
      y_limits = c(46.45, 46.47),
      resolution = 100,
      output = "png",
      folder = temp_dir,
      out_file = "mymap"
    )
  )

  # Check if png file produced
  expect_true(
    file.exists(
      file.path(
        temp_dir,
        "mymap.png"
      )
    )
  )

  expect_equal(phm_png_out_named$values, phm_shouldBe)

  # Clean up
  unlink(
    file.path(
      temp_dir,
      "mymap.png"
    )
  )
})




test_that("kmz output default name gives expected result", {
  phm_shouldBe <- phm_values_known()
  temp_dir <- tempdir()

  expect_message(
    phm_kmz_out <- position_heat_map(
      lamprey_tracks,
      x_limits = c(-84.14, -84.12),
      y_limits = c(46.45, 46.47),
      resolution = 100,
      output = "kmz",
      folder = temp_dir
    ),
    "Output file is located in"
  )

  expect_invisible(
    phm_kmz_out <- position_heat_map(
      lamprey_tracks,
      x_limits = c(-84.14, -84.12),
      y_limits = c(46.45, 46.47),
      resolution = 100,
      output = "kmz",
      folder = temp_dir
    )
  )

  # Check if kmz file produced
  expect_true(
    file.exists(
      file.path(
        temp_dir,
        "fish_absolute.kmz"
      )
    )
  )


  expect_equal(phm_kmz_out$values, phm_shouldBe)

  # Clean up
  unlink(
    file.path(
      temp_dir,
      "fish_absolute.kmz"
    )
  )
})





test_that("kmz output custom name gives expected result", {
  phm_shouldBe <- phm_values_known()
  temp_dir <- tempdir()

  expect_message(
    phm_kmz_out_named <- position_heat_map(
      lamprey_tracks,
      x_limits = c(-84.14, -84.12),
      y_limits = c(46.45, 46.47),
      resolution = 100,
      output = "kmz",
      folder = temp_dir,
      out_file = "mymap"
    ),
    "Output file is located in"
  )

  expect_invisible(
    phm_kmz_out_named <- position_heat_map(
      lamprey_tracks,
      x_limits = c(-84.14, -84.12),
      y_limits = c(46.45, 46.47),
      resolution = 100,
      output = "kmz",
      folder = temp_dir,
      out_file = "mymap"
    )
  )

  # Check if kmz file produced
  expect_true(
    file.exists(
      file.path(
        temp_dir,
        "mymap.kmz"
      )
    )
  )

  expect_equal(phm_kmz_out_named$values, phm_shouldBe)

  # Clean up
  unlink(
    file.path(
      temp_dir,
      "mymap.kmz"
    )
  )
})





##### TBD: TEST NON-EXPORTED FUNCTIONS ####
# Test non-exported query_worms_common function
test_that("internal function lonlat_to_utm", {
  skip("Test needs to be created.")
})

# Test non-exported query_worms_common function
test_that("internal function utm_to_lonlat", {
  skip("Test needs to be created.")
})
