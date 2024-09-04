# Make glatos_receivers2.rda
# (internal data object)

# get path to example receiver_locations file
rec2_file <- system.file("extdata",
  "sample_receivers2.csv",
  package = "glatos"
)

sample_receivers2 <- read_glatos_receivers(rec2_file)

#----------------------------------------------------

# add to sysdata.rda
rda_file <- file.path("R/sysdata.rda")
add_internal_data(sample_receivers2, rda_file)
