# Make internal data object blue_shark_att for testing
# (example data object)

dets_path <- system.file(
  "extdata",
  "blue_shark_detections_old.csv",
  package = "glatos"
)
deploy_path <- system.file(
  "extdata",
  "hfx_deploy_simplified.xlsx",
  package = "glatos"
)
tag_path <- system.file(
  "extdata",
  "otn_nsbs_tag_metadata.xls",
  package = "glatos"
)

dets <- read_otn_detections(dets_path, format="old")
tags <- prepare_tag_sheet(tag_path, 5, 2)
deploy <- prepare_deploy_sheet(deploy_path, 1)

blue_shark_att <- convert_otn_to_att(dets, tags, deploymentSheet = deploy)


#----------------------------------------------------
# add to sysdata.rda
rda_file <- file.path("R/sysdata.rda")
add_internal_data(blue_shark_att, rda_file)
