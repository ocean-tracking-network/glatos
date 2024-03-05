# Make glatos_receivers_schema
# (internal data object for read_glatos_receivers)
# Specify column names and data types for each receiver locations file version


# Make list element for each version
# pre-allocate table-level structure within each version
glatos_receivers_schema <- list(
  "v1.0" = NA,
  "v1.1" = NA
)


#----------------------------------------------------
# Version 1.0

glatos_receivers_schema$v1.0 <- read.table(
  text = "
  name                  type
  station               character
  glatos_array          character
  station_no            character
  consecutive_deploy_no integer
  intend_lat            numeric
  intend_long           numeric
  deploy_lat            numeric
  deploy_long           numeric
  recover_lat           numeric
  recover_long          numeric
  deploy_date_time      POSIXct
  recover_date_time     POSIXct
  bottom_depth          numeric
  riser_length          numeric
  instrument_depth      numeric
  ins_model_no          character
  glatos_ins_frequency  integer
  ins_serial_no         character
  deployed_by           character
  comments              character
  glatos_seasonal       character
  glatos_project        character
  glatos_vps            character
  ",
  header = TRUE,
  stringsAsFactors = FALSE
)

# \Version 1.0
#----------------------------------------------------

#----------------------------------------------------
# Version 1.1

glatos_receivers_schema$v1.1 <- read.table(
  text = "
  name                  type
  station               character
  glatos_array          character
  station_no            character
  consecutive_deploy_no integer
  intend_lat            numeric
  intend_long           numeric
  deploy_lat            numeric
  deploy_long           numeric
  recover_lat           numeric
  recover_long          numeric
  deploy_date_time      POSIXct
  recover_date_time     POSIXct
  bottom_depth          numeric
  riser_length          numeric
  instrument_depth      numeric
  ins_model_no          character
  glatos_ins_frequency  integer
  ins_serial_no         character
  code_map              character
  code_map_comment      character
  deployed_by           character
  comments              character
  glatos_seasonal       character
  glatos_project        character
  glatos_vps            character
  ",
  header = TRUE,
  stringsAsFactors = FALSE
)

# \Version 1.1
#----------------------------------------------------


# add to sysdata.rda
rda_file <- file.path("R/sysdata.rda")
glatos:::add_internal_data(glatos_receivers_schema, rda_file)

# for exported ('public') data
# devtools::use_data(glatos_receivers_schema, pkg = "..", overwrite = TRUE)
