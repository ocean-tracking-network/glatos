# Make otn_detection_schema (internal data object for read_otn_detections)
# Specify column names and data types for each detection file version

#----------------------------------------------------

# OTN 2025

otn_detection_schema <- read.table(
  text = "
                                   name           type                 mapping
                                   catalogNumber      character               animal_id
                                   commonName         character           common_name_e
                                   receiver           character             receiver_sn
                                   station            character                 station
                                   tagName            character          transmitter_id
                                   codeSpace          character   transmitter_codespace
                                   dateCollectedUTC     POSIXct detection_timestamp_utc
                                   decimalLongitude     numeric             deploy_long
                                   decimalLatitude      numeric              deploy_lat
                                   ",
  header = TRUE,
  stringsAsFactors = FALSE
)

otn_detection_schema_min_columns <- c(
  "catalogNumber",
  "station",
  "dateCollectedUTC",
  "decimalLongitude",
  "decimalLatitude"
)








# OTN pre-2025

otn_detection_schema_old <- read.table(
  text = "
                                   name           type                 mapping
                                   catalognumber      character               animal_id
                                   commonname         character           common_name_e
                                   receiver_group     character            glatos_array
                                   receiver           character             receiver_sn
                                   station            character                 station
                                   tagname            character          transmitter_id
                                   codespace          character   transmitter_codespace
                                   datecollected        POSIXct detection_timestamp_utc
                                   longitude            numeric             deploy_long
                                   latitude             numeric              deploy_lat
                                   ",
  header = TRUE,
  stringsAsFactors = FALSE
)

# for non-matched detection extracts from OTN
otn_detection_schema_old_min_columns <- c(
  "catalognumber",
  "station",
  "datecollected",
  "longitude",
  "latitude"
)
# \OTN
#----------------------------------------------------

# add to sysdata.rda
rda_file <- file.path("R/sysdata.rda")
add_internal_data(otn_detection_schema, rda_file)
add_internal_data(otn_detection_schema_min_columns, rda_file)
