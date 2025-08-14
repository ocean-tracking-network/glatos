# Make hfx_deployments in rda
# (internal data object)

# get path to example receiver_locations file
rec_file <- system.file("extdata", "hfx_deployments.csv", package = "glatos")

hfx_deployments <- read_otn_deployments(rec_file)

#----------------------------------------------------

# add to sysdata.rda
rda_file <- file.path("R/sysdata.rda")
add_internal_data(hfx_deployments, rda_file)
