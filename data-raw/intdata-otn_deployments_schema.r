#Make glatos_otn_schema 
# (internal data object for read_otn_deployments)
#Specify column names and data types for each receiver locations file version

# name                  type          mapping
# FID              character          FID
# collectioncode   character          glatos_project
# seriescode       character          seriescode
# ocean            character          ocean
# station_name     character          station
# station_type     character          station_type
# stn_lat            numeric          deploy_lat
# stn_long           numeric          deploy_long
# stationstatus    character          stationstatus
# locality         character          locality
# model            character          ins_model_no
# instrumenttype   character          instrumenttype
# deploy_date        POSIXct          deploy_date_time
# last_download      POSIXct          last_download
# recovery_date      POSIXct          receover_date_time
# downloads          numeric          downloads
# off_set            numeric          off_set

#----------------------------------------------------
#Version 1.0

otn_deployments_schema <- read.table(text = "
  name                  type          mapping
  collectioncode   character          glatos_project
  station_name     character          station
  stn_lat            numeric          deploy_lat
  stn_long           numeric          deploy_long
  model            character          ins_model_no
  deploy_date        POSIXct          deploy_date_time
  recovery_date      POSIXct          receover_date_time
  ",
  header = TRUE,
  stringsAsFactors = FALSE)

#\Version 1.0
#----------------------------------------------------

#add to sysdata.rda
rda_file <- file.path("..","R/sysdata.rda")
glatos:::add_internal_data(otn_deployments_schema, rda_file)

#for exported ('public') data
#devtools::use_data(otn_deployments_schema, pkg = "..", overwrite = TRUE)

