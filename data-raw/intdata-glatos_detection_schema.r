#Make glatos_detection_schema (internal data object for read_glatos_detections)
#Specify column names and data types for each detection file version


#Make list element for each version
#pre-allocate table-level structure within each version
glatos_detection_schema <- list(
  "v1.3" = NA
)


#----------------------------------------------------
#Version 1.3

glatos_detection_schema$v1.3 <- read.table(text = "
  name                       type
  animal_id                  character
  detection_timestamp_utc    POSIXct
  glatos_array               character
  station_no                 character
  transmitter_codespace      character
  transmitter_id             character
  sensor_value               numeric
  sensor_unit                character
  deploy_lat                 numeric
  deploy_long                numeric
  receiver_sn                character
  tag_type                   character
  tag_model                  character
  tag_serial_number          character
  common_name_e              character
  capture_location           character
  length                     numeric
  weight                     numeric
  sex                        character
  release_group              character
  release_location           character
  release_latitude           numeric
  release_longitude          numeric
  utc_release_date_time      POSIXct
  glatos_project_transmitter character
  glatos_project_receiver    character
  glatos_tag_recovered       character
  glatos_caught_date         Date
  station                    character
  min_lag                    numeric
  ",
  header = TRUE,
  stringsAsFactors = FALSE)

#\Version 1.3
#----------------------------------------------------


#add to internal data objects
isd <- file.path("..","R/sysdata.rda") 
append.Rda <- function(x, file) {
  old.objects <- load(file, new.env())
  new.objects <- deparse(substitute(x))
  old.objects <- setdiff(old.objects, new.objects)
  save(list = c(old.objects, new.objects), file = file)
}
append.Rda(glatos_detection_schema, isd)
