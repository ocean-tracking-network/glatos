#make detection_spec file; an internal data object for read_glatos_detections
#Define spec for each detection file version
detection_specs <- list(
  "v1.3" = NA
)
detection_specs$v1.3 <- read.table(text = "
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
  receiver_sn                numeric
  tag_type                   character
  tag_model                  character
  tag_serial_number          numeric
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


