#Make internal data object walleye_detections for testing
# (example data object)

#get path to example detections file
dtc_file <- system.file("extdata", 
  "walleye_detections.csv", package = "glatos")

#create new object
walleye_detections <- read_glatos_detections(dtc_file)

#----------------------------------------------------

#add to sysdata.rda
rda_file <- file.path(".","R/sysdata.rda")
glatos:::add_internal_data(walleye_detections, rda_file)

#for exported ('public') data
#devtools::use_data(walleye_detections, pkg = "..", overwrite = TRUE)
