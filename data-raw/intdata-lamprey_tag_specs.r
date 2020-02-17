#Make internal data object lamprey_tag_specs for testing
# (internal data object)

#get path to example detections file
tag_file <- system.file("extdata", 
  "lamprey_tag_specs.xls", package = "glatos")
lamprey_tag_specs <- read_vemco_tag_specs(tag_file, file_format = "vemco_xls")

#----------------------------------------------------

#add to sysdata.rda
rda_file <- file.path("R/sysdata.rda")
glatos:::add_internal_data(lamprey_tag_specs, rda_file)

#for exported ('public') data
#devtools::use_data(lamprey_tag_specs, pkg = "..", overwrite = TRUE)

