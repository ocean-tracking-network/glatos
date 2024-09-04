# Make internal data object blue_shark_detections for testing
# (example data object)

# get path to example detections file
dtc_file <- system.file("extdata",
  "blue_shark_detections.csv",
  package = "glatos"
)

# create new object
blue_shark_detections <- read_otn_detections(dtc_file)

#----------------------------------------------------

# add to sysdata.rda
rda_file <- file.path("R/sysdata.rda")
add_internal_data(blue_shark_detections, rda_file)
