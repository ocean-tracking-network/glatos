
#Make glatos_detection_schema (internal data object for read_glatos_detections)
#Specify column names and data types for each detection file version




#----------------------------------------------------
# OTN

otn_detection_schema <- read.table(text = "
         name           type                 mapping
collectioncode     character          collectioncode
catalognumber      character               animal_id
scientificname     character          scientificname
commonname         character              commonname
datelastmodified        Date        datelastmodified
detectedby         character              detectedby
receiver_group     character             receiver_sn
station            character            glatos_array
receiver           character                receiver
bottom_depth         numeric            bottom_depth
receiver_depth       numeric          receiver_depth
tagname            character          transmitter_id
codespace          character   transmitter_codespace
sensorname         character              sensorname
sensorraw            numeric               sensorraw
sensortype         character              sensortype
sensorvalue          numeric             sensorvalue
sensorunit         character              sensorunit
datecollected        POSIXct detection_timestamp_utc
timezone           character                timezone
longitude            numeric             deploy_long
latitude             numeric              deploy_lat
st_setsrid_4326    character         st_setsrid_4326
yearcollected        integer           yearcollected
monthcollected       integer          monthcollected
daycollected         integer            daycollected
julianday            integer               julianday
timeofday            numeric               timeofday
datereleasedtagger      Date      datereleasedtagger
datereleasedpublic      Date      datereleasedpublic
local_area         character              local_area
notes              character                   notes
citation           character                citation
unqdetecid         character              unqdetecid
",
header = TRUE,
stringsAsFactors = FALSE)

#\OTN
#----------------------------------------------------


#add to sysdata.rda
rda_file <- file.path("..","R/sysdata.rda")
glatos:::add_internal_data(otn_detection_schema, rda_file)
