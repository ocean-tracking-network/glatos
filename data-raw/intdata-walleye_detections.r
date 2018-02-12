#Make internal data object walleye_detections for testing
# (example data object)

#get path to example detections file
dtc_file <- system.file("extdata", 
  "walleye_detections.zip", package = "glatos")
dtc_file <- unzip(dtc_file, "walleye_detections.csv")
walleye_detections <- read_glatos_detections(dtc_file)
file.remove(dtc_file) #clean up

#----------------------------------------------------

#add to internal data objects
isd <- file.path("..","R/sysdata.rda") 
append.Rda <- function(x, file) {
  old.objects <- load(file, new.env())
  save(list = c(old.objects, deparse(substitute(x))), file = file)
  }
append.Rda(walleye_detections, isd)


