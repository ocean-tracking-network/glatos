# Make internal data object detection_efficiency for testing and examples
# (example data object)

# get path to example detection efficency
sample_detection_efficiency <- system.file("extdata",
  "sample_detection_efficiency.csv",
  package = "glatos"
)

#----------------------------------------------------

# add to sysdata.rda
rda_file <- file.path("R/sysdata.rda")


glatos:::add_internal_data(sample_detection_efficiency, rda_file)

# for exported ('public') data
# devtools::use_data(walleye_workbook, pkg = "..", overwrite = TRUE)

# devtools::use_data(sample_detection_efficiency, pkg = "..", overwrite = TRUE)
