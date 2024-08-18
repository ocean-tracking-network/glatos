# Make internal data object blue_shark_att for testing
# (example data object)

dets_path <- system.file("extdata", "blue_shark_detections.csv",
  package = "glatos"
)
deploy_path <- system.file("extdata", "hfx_deploy_simplified.xlsx",
  package = "glatos"
)
tag_path <- system.file("extdata", "otn_nsbs_tag_metadata.xls",
  package = "glatos"
)

dets <- glatos::read_otn_detections(dets_path)
tags <- glatos::prepare_tag_sheet(tag_path, 5, 2)
deploy <- glatos::prepare_deploy_sheet(deploy_path, 1)

blue_shark_att <- glatos::convert_otn_to_att(dets, tags, 
                                             deploymentSheet = deploy)


#----------------------------------------------------
# add to sysdata.rda
rda_file <- file.path("R/sysdata.rda")
glatos:::add_internal_data(blue_shark_att, rda_file)
