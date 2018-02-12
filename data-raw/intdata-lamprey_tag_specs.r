#Make internal data object lamprey_tag_specs for testing
# (internal data object)

#get path to example detections file
tag_file <- system.file("extdata", 
  "lamprey_tag_specs.xls", package = "glatos")
lamprey_tag_specs <- read_vemco_tag_specs(tag_file, file_format = "vemco_xls")

#----------------------------------------------------

#add to internal data objects
isd <- file.path("..","R/sysdata.rda") 
append.Rda <- function(x, file) {
  load(file)
  old.objects <- load(file, new.env())
  new.objects <- deparse(substitute(x))
  save(list = c(old.objects, new.objects), file = file)
  }
append.Rda(lamprey_tag_specs, isd)


