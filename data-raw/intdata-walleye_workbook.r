#Make internal data object lamprey_workbook for testing
# (example data object)

#get path to example workbook file
wb_file <- system.file("extdata", 
  "walleye_workbook.xlsm", package = "glatos")
walleye_workbook <- read_glatos_workbook(wb_file)

#----------------------------------------------------

#add to sysdata.rda
rda_file <- file.path(".","R/sysdata.rda")
glatos:::add_internal_data(walleye_workbook, rda_file)

#for exported ('public') data
#devtools::use_data(walleye_workbook, pkg = "..", overwrite = TRUE)

