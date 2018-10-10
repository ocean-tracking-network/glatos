context("Check receiver_efficiency")

# Actual result
#get path to example detection and deployment files
det_file <- system.file("extdata", "hfx_detections.csv",
                        package = "glatos")

dep_file <- system.file("extdata", "hfx_deployments.csv",
                        package = "glatos")

hfx_deployments <- glatos::read_otn_deployments(dep_file)
dets <- glatos::read_otn_detections(det_file)

hfx_rei <- glatos::REI(dets,hfx_deployments)

# Test using testthat library
test_that("REI gives expected results for the halifax line", {
  # Check if expected and actual results are the same
  expect_equal(hfx_rei, hfx_receiver_efficiency_index)
})