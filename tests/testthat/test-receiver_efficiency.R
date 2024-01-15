test_that("REI gives expected results for the halifax line", {
  det_file <- system.file(
    "extdata",
    "hfx_detections.csv",
    package = "glatos"
  )

  dep_file <- system.file(
    "extdata",
    "hfx_deployments.csv",
    package = "glatos"
  )

  hfx_detections <- read_otn_detections(det_file)
  hfx_deployments <- read_otn_deployments(dep_file)

  hfx_rei <- REI(hfx_detections, hfx_deployments)



  hfx_efficiency_index <- hfx_receiver_efficiency_index()

  # Check if expected and actual results are the same
  expect_equal(hfx_rei, hfx_efficiency_index)
})




test_that("REI gives expected results for the GLATOS samples", {
  det_file <- system.file(
    "extdata",
    "walleye_detections.csv",
    package = "glatos"
  )

  dep_file <- system.file(
    "extdata",
    "sample_receivers.csv",
    package = "glatos"
  )


  glatos_dets <- read_glatos_detections(det_file)
  glatos_deps <- read_glatos_receivers(dep_file)


  glatos_rei <- REI(glatos_dets, glatos_deps)


  glatos_efficiency_index <- glatos_receiver_efficiency_index()

  # Check if expected and actual results are the same
  expect_equal(glatos_rei, glatos_efficiency_index)

  expect_s3_class(glatos_rei, "data.frame")
  expect_identical(dim(glatos_rei), c(82L, 4L))
  expect_named(glatos_rei, c("station", "latitude", "longitude", "rei"))
})
