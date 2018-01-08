#Make receiver_locations_2011.rda 
# (example data object)

#get path to example receiver_locations file
rec_file <- system.file("extdata", 
  "receiver_locations_2011.csv", package = "glatos")
receiver_locations_2011 <- read_glatos_receiver_locations(rec_file)

#----------------------------------------------------

#devtools::use_data(receiver_locations_2011, overwrite = TRUE)
