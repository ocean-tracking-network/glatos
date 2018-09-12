
#Make otn_detection_schema (internal data object for read_otn_detections)
#Specify column names and data types for each detection file version




#----------------------------------------------------
# OTN

otn_detection_schema <- read.table(text = "
                                   name           type                 mapping
                                   catalognumber      character               animal_id
                                   commonname         character           common_name_e
                                   receiver_group     character            glatos_array
                                   station            character                 station
                                   tagname            character          transmitter_id
                                   datecollected        POSIXct detection_timestamp_utc
                                   longitude            numeric             deploy_long
                                   latitude             numeric              deploy_lat
                                   ",
                                   header = TRUE,
                                   stringsAsFactors = FALSE)

#\OTN
#----------------------------------------------------


#add to sysdata.rda
rda_file <- file.path("..","R/sysdata.rda")
glatos:::add_internal_data(otn_detection_schema, rda_file)