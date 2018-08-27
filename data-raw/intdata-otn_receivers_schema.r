#Make glatos_receivers_schema 
# (internal data object for read_glatos_receivers)
#Specify column names and data types for each receiver locations file version


#Make list element for each version
#pre-allocate table-level structure within each version
glatos_receivers_schema <- list(
  "v1.0" = NA
)


#----------------------------------------------------
#Version 1.0

glatos_receivers_schema$v1.0 <- read.table(text = "
  name                  type              mapping
  
  ",
  header = TRUE,
  stringsAsFactors = FALSE)

#\Version 1.0
#----------------------------------------------------

#add to sysdata.rda
rda_file <- file.path("..","R/sysdata.rda")
glatos:::add_internal_data(otn_receivers_schema, rda_file)

#for exported ('public') data
#devtools::use_data(glatos_receivers_schema, pkg = "..", overwrite = TRUE)

