#make workbook_spec file; an internal data object for read_glatos_workbook


#Define spec for each workbook version
workbook_specs <- list(
  "v1.3" = list(
    project = NA,
    locations = NA, 
    proposed = NA,
    deployment = NA, 
    recovery = NA, 
    tagging = NA)
)

workbook_specs$v1.3$project <- NA

workbook_specs$v1.3$locations <- read.table(text = "
  name                 type
  glatos_array         text
  location_description text
  water_body           text
  glatos_region        text",
  header = TRUE,
  stringsAsFactors = FALSE)

workbook_specs$v1.3$proposed <- read.table(text = "
  name                 type
  otn_region           text            
  glatos_array         text
  otn_array            text
  station_no           text
  bottom_depth         numeric
  proposed_lat         numeric
  proposed_long        numeric
  proposed_start_date  date
  proposed_end_date    date
  glatos_ins_frequency numeric
  glatos_funded        text
  glatos_seasonal      text
  glatos_project       text 
  glatos_vps           text",
  header = TRUE,
  stringsAsFactors = FALSE)

workbook_specs$v1.3$deployment <- read.table(text = "
  name                      type
  glatos_array              text
  otn_array                 text
  station_no                text
  consecutive_deploy_no     numeric
  mooring_drop_dead_date    date
  intend_lat                numeric
  intend_long               numeric
  otn_mission_id            text
  deploy_date_time          timestamp
  glatos_deploy_date_time   timestamp
  glatos_timezone           text
  deploy_lat                numeric
  deploy_long               numeric
  bottom_depth              numeric
  riser_length              numeric
  instrument_depth          numeric
  checwlk_complete_time     text
  status_in                 text
  ins_model_no              text
  glatos_ins_frequency      text
  ins_serial_no             text
  rcv_modem_address         text
  sync_date_time            timestamp
  memory_erased_at_deploy   text
  rcv_battery_install_date  date
  rcv_expected_battery_life text
  rcv_voltage_at_deploy     text
  rcv_tilt_after_deploy     text
  deployed_by               text
  comments                  text
  glatos_seasonal           text
  glatos_project            text
  glatos_vps                text",    
  header = TRUE,
  stringsAsFactors = FALSE)

workbook_specs$v1.3$recovery <- read.table(text= "
  name                      type
  glatos_array              text
  otn_array                 text
  station_no                text
  consecutive_deploy_no     numeric
  otn_mission_id            text
  ar_confirm                text 
  data_downloaded           text
  ins_model_number          text
  ins_serial_number         text
  recovered                 text
  recover_date_time         timestamp
  glatos_recover_date_time  timestamp
  glatos_timezone           text
  recover_lat               numeric
  recover_long              numeric
  glatos_project            text
  glatos_vps                text",
  header = TRUE,
  stringsAsFactors = FALSE)

workbook_specs$v1.3$tagging <- read.table(text="
  name                                      type
  animal_id                                 text
  tag_type                                  text
  tag_manufacturer                          text
  tag_model                                 text
  tag_serial_number                         text        
  tag_id_code                               text
  tag_code_space                            text
  tag_implant_type                          text
  tag_activation_date                       date
  est_tag_life                              numeric
  tagger                                    text
  tag_owner_pi                              text 
  tag_owner_organization                    text 
  common_name_e                             text
  scientific_name                           text
  capture_location                          text 
  capture_latitude                          numeric
  capture_longitude                         numeric
  wild_or_hatchery                          text
  stock                                     text
  length                                    numeric
  weight                                    numeric
  length_type                               text 
  age                                       text
  sex                                       text
  dna_sample_taken                          text
  treatment_type                            text
  release_group                             text
  release_location                          text
  release_latitude                          numeric
  release_longitude                         numeric
  utc_release_date_time                     timestamp
  glatos_release_date_time                  timestamp
  glatos_timezone                           text
  capture_depth                             numeric
  temperature_change                        numeric
  holding_temperature                       numeric
  surgery_location                          text
  date_of_surgery                           date
  surgery_latitude                          numeric
  surgery_longitude                         numeric
  sedative                                  text
  sedative_concentration                    text
  anaesthetic                               text
  buffer                                    text
  anaesthetic_concentration                 text
  buffer_concentration_in_anaesthetic       text 
  anesthetic_concentration_in_recirculation text
  buffer_concentration_in_recirculation     text
  dissolved_oxygen                          text
  comments                                  text",
  header = TRUE,
  stringsAsFactors = FALSE)

