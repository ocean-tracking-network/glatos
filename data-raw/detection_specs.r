#make detection_spec file; an internal data object for read_glatos_detections
#Define spec for each detection file version
detection_specs <- list(
  "v1.3" = NA
)
detection_specs$v1.3 <- read.table(text = "
  name                       type
  animal_id                  text
  detection_timestamp_utc    timestamp
  glatos_array               text
  station_no                 text
  transmitter_codespace      text
  transmitter_id             text
  sensor_value               numeric
  sensor_unit                text
  deploy_lat                 numeric
  deploy_long                numeric
  receiver_sn                numeric
  tag_type                   text
  tag_model                  text
  tag_serial_number          numeric
  common_name_e              text
  capture_location           text
  length                     numeric
  weight                     numeric
  sex                        text
  release_group              text
  release_location           text
  release_latitude           numeric
  release_longitude          numeric
  utc_release_date_time      timestamp
  glatos_project_transmitter text
  glatos_project_receiver    text
  glatos_tag_recovered       text
  glatos_caught_date         date
  station                    text
  min_lag                    numeric
  ",
  header = TRUE,
  stringsAsFactors = FALSE)


