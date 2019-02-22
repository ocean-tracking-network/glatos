context("Check receiver_efficiency")

# Actual result
#get path to example detection and deployment files
det_file <- system.file("extdata", "hfx_detections.csv",
                        package = "glatos")

dep_file <- system.file("extdata", "hfx_deployments.csv",
                        package = "glatos")

hfx_detections <- glatos::read_otn_detections(det_file)
hfx_deployments <- glatos::read_otn_deployments(dep_file)

hfx_rei <- glatos::REI(hfx_detections,hfx_deployments)



det_file <- system.file("extdata", "walleye_detections.csv",
                        package = "glatos")

dep_file <- system.file("extdata", "sample_receivers.csv",
                        package = "glatos")


glatos_dets <- glatos::read_glatos_detections(det_file)
glatos_deps <- glatos::read_glatos_receivers(dep_file)


glatos_rei <- glatos::REI(glatos_dets, glatos_deps)

# Test using testthat library
test_that("REI gives expected results for the halifax line", {
  # Check if expected and actual results are the same
  expect_equal(hfx_rei, hfx_receiver_efficiency_index)
})

# Test using testthat library
test_that("REI gives expected results for the GLATOS samples", {
  # Check if expected and actual results are the same
  expect_equal(glatos_rei, glatos_receiver_efficiency_index)
})