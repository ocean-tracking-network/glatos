#Make receiver_locations_2011.rda 
# (example data object)

#get path to example receiver_locations file
rec_file <- system.file("extdata", 
  "receiver_locations_2011.csv", package = "glatos")
receivers_2011 <- read_glatos_receiver_locations(rec_file)

#----------------------------------------------------

#add to internal data objects
isd <- file.path("..","R/sysdata.rda") 
append.Rda <- function(x, file) {
  old.objects <- load(file, new.env())
  new.objects <- deparse(substitute(x))
  old.objects <- setdiff(old.objects, new.objects)
  save(list = c(old.objects, new.objects), file = file)
}
append.Rda(receivers_2011, isd)
