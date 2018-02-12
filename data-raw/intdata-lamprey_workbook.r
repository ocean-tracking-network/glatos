#Make internal data object lamprey_workbook for testing
# (example data object)

#get path to example workbook file
wb_file <- system.file("extdata", 
  "SMRSL_GLATOS_20140828.xlsm", package = "glatos")
lamprey_workbook <- read_glatos_workbook(wb_file)

#----------------------------------------------------

#devtools::use_data(lamprey_workbook, pkg = "..", internal = TRUE)
