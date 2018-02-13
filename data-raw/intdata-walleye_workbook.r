#Make internal data object lamprey_workbook for testing
# (example data object)

#get path to example workbook file
wb_file <- system.file("extdata", 
  "SMRSL_GLATOS_20140828.xlsm", package = "glatos")
lamprey_workbook <- read_glatos_workbook(wb_file)

#----------------------------------------------------

#add to internal data objects
isd <- file.path("..","R/sysdata.rda") 
append.Rda <- function(x, file) {
  old.objects <- load(file, new.env())
  new.objects <- deparse(substitute(x))
  old.objects <- setdiff(old.objects, new.objects)
  save(list = c(old.objects, new.objects), file = file)
}
append.Rda(lamprey_workbook, isd)
