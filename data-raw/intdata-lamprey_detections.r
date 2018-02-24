#Make internal data object lamprey_detections for testing
# (example data object)

#get path to example detections file
dtc_file <- system.file("extdata", 
  "lamprey_detections.csv", package = "glatos")

#create new object
lamprey_detections <- read_glatos_detections(dtc_file)

#----------------------------------------------------

#add to sysdata.rda
rda_file <- file.path("..","R/sysdata.rda")
glatos:::add_internal_data(lamprey_detections, rda_file)

#for exported ('public') data
#devtools::use_data(lamprey_detections, pkg = "..", overwrite = TRUE)
