# Make internal data object blue_shark_detections for testing
# (example data object)

# get path to example detections file
dtc_file <- system.file(
  "extdata",
  "blue_shark_detections_old.csv",
  package = "glatos"
)

# create new object
blue_shark_detections_old <- read_otn_detections(dtc_file, "old")

#----------------------------------------------------

# add to sysdata.rda
rda_file <- file.path("R/sysdata.rda")
add_internal_data(blue_shark_detections_old, rda_file)
