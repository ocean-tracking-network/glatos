# Make glatos_otn_schema
# (internal data object for read_otn_deployments)
# Specify column names and data types for each receiver locations file version


#----------------------------------------------------
# Version 1.0
# This must match the format of the stations_receivers layer of the OTN GeoServer
# pulled via URLs like the below:
# https://geoserver.oceantrack.org/geoserver/otn/wfs?request=getfeature&service=wfs&typename=otn:stations_receivers&outputFormat=CSV

otn_deployments_schema <- read.table(
  text = "
  name                  type          mapping
  collectioncode   character          glatos_project
  station_name     character          station
  stn_lat            numeric          deploy_lat
  stn_long           numeric          deploy_long
  model            character          ins_model_no
  deploy_date        POSIXct          deploy_date_time
  recovery_date      POSIXct          recover_date_time
  last_download      POSIXct          last_download
  ",
  header = TRUE,
  stringsAsFactors = FALSE
)

# \Version 1.0
#----------------------------------------------------

# add to sysdata.rda
rda_file <- file.path("R/sysdata.rda")
add_internal_data(otn_deployments_schema, rda_file)
