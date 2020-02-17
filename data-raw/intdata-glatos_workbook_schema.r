#Make glatos_workbook_schema (internal data object for read_glatos_workbook)
#Specify column names and data types for each sheet in each workbook version


#Make list element for each workbook version
#pre-allocate sheet-level structure within each version
glatos_workbook_schema <- list(
  "v1.3" = list(
    project = NA,
    locations = NA, 
    proposed = NA,
    deployment = NA, 
    recovery = NA, 
    tagging = NA)
)


#----------------------------------------------------
#Version 1.3

glatos_workbook_schema$v1.3$project <- NA

glatos_workbook_schema$v1.3$locations <- read.table(text = "
  name                 type       args
  glatos_array         character  NA
  location_description character  NA
  water_body           character  NA
  glatos_region        character  NA",
  header = TRUE,
  stringsAsFactors = FALSE)

glatos_workbook_schema$v1.3$proposed <- read.table(text = "
  name                 type       args
  otn_region           character  NA           
  glatos_array         character  NA 
  otn_array            character  NA 
  station_no           character  NA 
  bottom_depth         numeric    NA 
  proposed_lat         numeric    NA 
  proposed_long        numeric    NA 
  proposed_start_date  Date       NA 
  proposed_end_date    Date       NA 
  glatos_ins_frequency numeric    NA 
  glatos_funded        character  NA 
  glatos_seasonal      character  NA 
  glatos_project       character  NA 
  glatos_vps           character  NA",
  header = TRUE,
  stringsAsFactors = FALSE)

glatos_workbook_schema$v1.3$deployment <- read.table(text = "
  name                      type       args
  glatos_array              character  NA
  otn_array                 character  NA
  station_no                character  NA
  consecutive_deploy_no     numeric    NA
  mooring_drop_dead_date    Date       NA
  intend_lat                character  NA
  intend_long               character  NA
  otn_mission_id            character  NA
  deploy_date_time          POSIXct    \'tz = \"UTC\"\'
  glatos_deploy_date_time   POSIXct    \'tz = REFCOL(glatos_timezone)\'
  glatos_timezone           character  NA
  deploy_lat                numeric    NA
  deploy_long               numeric    NA
  bottom_depth              numeric    NA
  riser_length              numeric    NA
  instrument_depth          numeric    NA
  checwlk_complete_time     character  NA
  status_in                 character  NA
  ins_model_no              character  NA
  glatos_ins_frequency      character  NA
  ins_serial_no             character  NA
  rcv_modem_address         character  NA
  sync_date_time            POSIXct    \'tz = \"UTC\"\'
  memory_erased_at_deploy   character  NA
  rcv_battery_install_date  Date       NA
  rcv_expected_battery_life character  NA
  rcv_voltage_at_deploy     character  NA
  rcv_tilt_after_deploy     character  NA
  deployed_by               character  NA
  comments                  character  NA
  glatos_seasonal           character  NA
  glatos_project            character  NA
  glatos_vps                character  NA",    
  header = TRUE,
  stringsAsFactors = FALSE)

glatos_workbook_schema$v1.3$recovery <- read.table(text= "
  name                      type       args
  glatos_array              character  NA
  otn_array                 character  NA
  station_no                character  NA
  consecutive_deploy_no     numeric    NA
  otn_mission_id            character  NA
  ar_confirm                character  NA
  data_downloaded           character  NA
  ins_model_number          character  NA
  ins_serial_number         character  NA
  recovered                 character  NA
  recover_date_time         POSIXct    \'tz = \"UTC\"\'
  glatos_recover_date_time  POSIXct    \'tz = REFCOL(glatos_timezone)\'
  glatos_timezone           character  NA
  recover_lat               numeric    NA
  recover_long              numeric    NA
  glatos_project            character  NA
  glatos_vps                character  NA",
  header = TRUE,
  stringsAsFactors = FALSE)

glatos_workbook_schema$v1.3$tagging <- read.table(text="
  name                                      type       args
  animal_id                                 character  NA
  tag_type                                  character  NA
  tag_manufacturer                          character  NA
  tag_model                                 character  NA
  tag_serial_number                         character  NA    
  tag_id_code                               character  NA
  tag_code_space                            character  NA
  tag_implant_type                          character  NA
  tag_activation_date                       Date       NA
  est_tag_life                              character  NA
  tagger                                    character  NA
  tag_owner_pi                              character  NA
  tag_owner_organization                    character  NA
  common_name_e                             character  NA
  scientific_name                           character  NA
  capture_location                          character  NA
  capture_latitude                          numeric    NA
  capture_longitude                         numeric    NA
  wild_or_hatchery                          character  NA
  stock                                     character  NA
  length                                    numeric    NA
  weight                                    numeric    NA
  length_type                               character  NA
  age                                       character  NA
  sex                                       character  NA
  dna_sample_taken                          character  NA
  treatment_type                            character  NA
  release_group                             character  NA
  release_location                          character  NA
  release_latitude                          numeric    NA
  release_longitude                         numeric    NA
  utc_release_date_time                     POSIXct    \'tz=UTC\'
  glatos_release_date_time                  POSIXct    \'tz=REFCOL(glatos_timezone)\'
  glatos_timezone                           character  NA
  capture_depth                             numeric    NA
  temperature_change                        numeric    NA
  holding_temperature                       numeric    NA
  surgery_location                          character  NA
  date_of_surgery                           Date       NA
  surgery_latitude                          numeric    NA
  surgery_longitude                         numeric    NA
  sedative                                  character  NA
  sedative_concentration                    character  NA
  anaesthetic                               character  NA
  buffer                                    character  NA
  anaesthetic_concentration                 character  NA
  buffer_concentration_in_anaesthetic       character  NA
  anesthetic_concentration_in_recirculation character  NA
  buffer_concentration_in_recirculation     character  NA
  dissolved_oxygen                          character  NA
  comments                                  character  NA
  glatos_project	                          character  NA
  glatos_external_tag_id1                   character  NA
  glatos_external_tag_id2                   character  NA
  glatos_tag_recovered                      character  NA
  glatos_caught_date	                      Date       NA
  glatos_reward                             character  NA
",
  header = TRUE,
  stringsAsFactors = FALSE)

#\Version 1.3
#----------------------------------------------------

#add to sysdata.rda
rda_file <- file.path("R/sysdata.rda")
glatos:::add_internal_data(glatos_workbook_schema, rda_file)

#for exported ('public') data
#devtools::use_data(glatos_workbook_schema, pkg = "..", overwrite = TRUE)

