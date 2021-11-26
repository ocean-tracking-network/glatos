context("Check convert_otn_to_att")

dets_path <- system.file("extdata", "blue_shark_detections.csv", 
                         package = "glatos")
deploy_path <- system.file("extdata", "hfx_deploy_simplified.xlsx",
                           package = "glatos")
tag_path <- system.file("extdata", "otn_nsbs_tag_metadata.xls",
                        package = "glatos")

dets <- read_otn_detections(dets_path)
tags <- prepare_tag_sheet(tag_path, 5, 2)
deploy <- prepare_deploy_sheet(deploy_path)

bs_att <- convert_otn_to_att(dets, tags, deploymentSheet = deploy)

test_that("blue_shark_att gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(bs_att, blue_shark_att)
})