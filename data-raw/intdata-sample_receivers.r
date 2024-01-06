# Make glatos_receivers.rda
# (internal data object)

# get path to example receiver_locations file
rec_file <- system.file("extdata",
  "sample_receivers.csv",
  package = "glatos"
)

sample_receivers <- read_glatos_receivers(rec_file)

#----------------------------------------------------

# add to sysdata.rda
rda_file <- file.path("R/sysdata.rda")
glatos:::add_internal_data(sample_receivers, rda_file)

# for exported ('public') data
# devtools::use_data(glatos_receivers, pkg = "..", overwrite = TRUE)
