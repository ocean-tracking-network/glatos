#Make walleye_detections.rda 
# (example data object)

#get path to example detections file
dtc_file <- system.file("extdata", 
  "walleye_detections.zip", package = "glatos")
dtc_file <- unzip(dtc_file, "walleye_detections.csv")
walleye_detections <- read_glatos_detections(dtc_file)

#----------------------------------------------------

#devtools::use_data(walleye_detections, pkg = "..", overwrite = TRUE)
