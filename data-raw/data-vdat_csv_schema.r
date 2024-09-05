# Make vdat_csv_schema (data object for read_vdat_csv)
# Specify column names and data types for each known vdat csv version


# Make list element for each vdat csv schema version
# pre-allocate sheet-level structure within each version
vdat_csv_schema <- list(
  "v1.0.0" = list(
    "ATTITUDE" = NA,
    "BATTERY" = NA,
    "CFG_CHANNEL" = NA,
    "CFG_STATION" = NA,
    "CFG_STUDY" = NA,
    "CFG_TRANSMITTER" = NA,
    "CLOCK_REF" = NA,
    "CLOCK_SET" = NA,
    "DATA_ERROR" = NA,
    "DATA_SOURCE_FILE" = NA,
    "DEPTH" = NA,
    "DEPTH_STATS" = NA,
    "DET" = NA,
    "DET_FILTER" = NA,
    "DIAG" = NA, # added 2019-09-10
    "DIAG_FAST" = NA, # added 2019-09-10
    "DIAG_HR2" = NA, # added 2019-09-10
    "DIAG_VR2W" = NA,
    "DIAG_VR2W_INTERIM" = NA,
    "DIAG_VR2AR" = NA,
    "DIAG_VR2AR_INTERIM" = NA,
    "DIAG_VR2TX" = NA,
    "DIAG_VR2TX_INTERIM" = NA,
    "DIAG_VR4" = NA, # added 2019-09-10
    "EVENT" = NA,
    "EVENT_FAULT" = NA,
    "EVENT_INIT" = NA,
    "EVENT_OFFLOAD" = NA,
    "HEALTH_HR2" = NA, # added 2019-09-10
    "HEALTH_VR2W" = NA,
    "HEALTH_VR2AR" = NA,
    "HEALTH_VR2TX" = NA,
    "HEALTH_VR4" = NA, # added 2019-09-10
    "NOISE_STATS_VR2AR" = NA,
    "NOISE_STATS_VR2TX" = NA,
    "TEMP" = NA,
    "TEMP_STATS" = NA
  )
)

vdat_csv_schema$v1.0.0$ATTITUDE <- read.table(
  text = '
  name                   type
  "ATTITUDE_DESC"        character
  "Device Time (UTC)"    POSIXct
  "Time"                 POSIXct
  "Time Offset (h)"      numeric
  "Time Correction (s)"  numeric
  "Model"                character
  "Serial Number"        character
  "Tilt (deg)"           numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$BATTERY <- read.table(
  text = '
  name                     type
  "BATTERY_DESC"           character
  "Device Time (UTC)"      POSIXct
  "Time"                   POSIXct
  "Time Offset (h)"        numeric
  "Time Correction (s)"    numeric
  "Model"                  character
  "Serial Number"          character
  "Battery Position"       character
  "Battery Type"           character
  "Battery Serial Number"  character
  "Battery Voltage (V)"    numeric
  "Battery Remaining (%)"  numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$CFG_CHANNEL <- read.table(
  text = '
  name                     type
  "CFG_CHANNEL_DESC"       character
  "Device Time (UTC)"      POSIXct
  "Time"                   POSIXct
  "Time Offset (h)"        numeric
  "Time Correction (s)"    numeric
  "Model"                  character
  "Serial Number"          character
  "Index"                  numeric
  "Frequency (kHz)"        numeric
  "Decoder"                character
  "PPM Map ID"             character
  "HR Coding ID"           character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)


vdat_csv_schema$v1.0.0$CFG_STATION <- read.table(
  text = '
  name                     type
 "CFG_STATION_DESC"        character
 "Device Time (UTC)"       POSIXct
 "Time"                    POSIXct
 "Time Offset (h)"         numeric
 "Time Correction (s)"      numeric
 "Model"                   character
 "Serial Number"           character
 "Station Name"            character
 "Latitude (deg)"          numeric
 "Longitude (deg)"         numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$CFG_STUDY <- read.table(
  text = '
  name                     type
  "CFG_STUDY_DESC"         character
  "Device Time (UTC)"      POSIXct
  "Time"                   POSIXct
  "Time Offset (h)"        numeric
  "Time Correction (s)"    numeric
  "Model"                  character
  "Serial Number"          character
  "Description"            character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$CFG_TRANSMITTER <- read.table(
  text = '
  name                     type
  "CFG_TRANSMITTER_DESC"   character
  "Device Time (UTC)"      POSIXct
  "Time"                   POSIXct
  "Time Offset (h)"        numeric
  "Time Correction (s)"    numeric
  "Model"                  character
  "Serial Number"          character
  "Transmission Type"      character
  "Full ID"                character
  "ID"                     character
  "Power Level"            character
  "Min Delay (s)"          numeric
  "Max Delay (s)"          numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$CLOCK_REF <- read.table(
  text = '
  name                      type
  "CLOCK_REF_DESC"          character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "External Time (UTC)"     POSIXct
  "External Difference (s)" numeric
  "Source"                  character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$CLOCK_SET <- read.table(
  text = '
  name                      type
  "CLOCK_SET_DESC"          character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Prior Device Time (UTC)" POSIXct
  "Prior Difference (s)"    numeric
  "Source"                  character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$DATA_ERROR <- read.table(
  text = '
  name                      type
  "DATA_ERROR_DESC"         character
  "Type"                    character
  "Error"                   character
  "Page"                    character
  "Offset"                  character
  "Description"             character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$DATA_SOURCE_FILE <- read.table(
  text = '
  name                      type
  "DATA_SOURCE_FILE_DESC"   character
  "File Name"               character
  "UUID"                    character
  "Type"                    character
  "Size"                    character
  "State"                   character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$DEPTH <- read.table(
  text = '
  name                      type
  "DEPTH_DESC"              character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Depth (m)"               numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$DEPTH_STATS <- read.table(
  text = '
  name                      type
  "DEPTH_STATS_DESC"        character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Depth Min (m)"           numeric
  "Depth Max (m)"           numeric
  "Depth Mean (m)"          numeric
  "Sample Count"            numeric
  "Accumulation Period (s)" numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$DET <- read.table(
  text = '
  name                      type
  "DET_DESC"                character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Detection Type"          character
  "Full ID"                 character
  "ID"                      character
  "Signal Strength (dB)"    numeric
  "Noise (dB)"              numeric
  "Quality Score"           numeric
  "Decoder"                 character
  "Sensor Value"            numeric
  "Sensor Unit"             character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$DET_FILTER <- read.table(
  text = '
  name                      type
  "DET_FILTER_DESC"         character
  "Filter Name"             character
  "Details"                 character
  "Rejected Detections"     numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$DIAG <- read.table(
  text = '
  name                          type
  "DIAG_DESC"                   character
  "Device Time (UTC)"           POSIXct
  "Time"                        POSIXct
  "Time Offset (h)"             numeric
  "Time Correction (s)"         numeric
  "Model"                       character
  "Serial Number"               character
  "Ambient Temperature (deg C)" numeric
  "Noise Mean (mV)"             numeric
  "Tilt (deg)"                  numeric
  "Depth (m)"                   numeric
  "PPM Pings"                   numeric
  "PPM Detections"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$DIAG_FAST <- read.table(
  text = '
  name                          type
  "DIAG_FAST_DESC"                   character
  "Device Time (UTC)"           POSIXct
  "Time"                        POSIXct
  "Time Offset (h)"             numeric
  "Time Correction (s)"         numeric
  "Model"                       character
  "Serial Number"               character
  "Ambient Temperature (deg C)" numeric
  "Noise (mV)"                  numeric
  "Tilt (deg)"                  numeric
  "Depth (m)"                   numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$DIAG_HR2 <- read.table(
  text = '
  name                          type
  "DIAG_HR2_DESC"               character
  "Device Time (UTC)"           POSIXct
  "Time"                        POSIXct
  "Time Offset (h)"             numeric
  "Time Correction (s)"         numeric
  "Model"                       character
  "Serial Number"               character
  "HR Noise (dB)"               numeric
  "HR Accepted Detections"      numeric
  "HR Rejected Detections"      numeric
  "PPM Pings (D1)"              numeric
  "PPM Detections (D1)"         numeric
  "PPM Pings (D2)"              numeric
  "PPM Detections (D2)"         numeric
  "PPM Noise (dB)"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$DIAG_VR2AR <- read.table(
  text = '
  name                      type
  "DIAG_VR2AR_DESC"         character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Pings"                   numeric
  "Detections"              numeric
  "Noise (mV)"              numeric
  "Tilt (deg)"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$DIAG_VR2AR_INTERIM <- read.table(
  text = '
  name                      type
  "DIAG_VR2AR_INTERIM_DESC" character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Pings"                   numeric
  "Detections"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$DIAG_VR2TX <- read.table(
  text = '
  name                      type
  "DIAG_VR2TX_DESC"         character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Pings"                   numeric
  "Detections"              numeric
  "Noise (mV)"              numeric
  "Tilt (deg)"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$DIAG_VR2TX_INTERIM <- read.table(
  text = '
  name                      type
  "DIAG_VR2TX_INTERIM_DESC" character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Pings"                   numeric
  "Detections"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$DIAG_VR4 <- read.table(
  text = '
  name                      type
  "DIAG_VR4_DESC"           character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Pings (69 kHz)"          numeric
  "Detections (69 kHz)"     numeric
  "Pings (180 kHz)"         numeric
  "Detections (180 kHz)"    numeric
  "Tilt (deg)"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$DIAG_VR2W <- read.table(
  text = '
  name                      type
  "DIAG_VR2W_DESC"          character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Pings"                   numeric
  "Detections"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$DIAG_VR2W_INTERIM <- read.table(
  text = '
  name                      type
  "DIAG_VR2W_INTERIM_DESC"          character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Pings"                   numeric
  "Detections"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$EVENT <- read.table(
  text = '
  name                      type
  "EVENT_DESC"              character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Event Type"              character
  "Event Details"           character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$EVENT_FAULT <- read.table(
  text = '
  name                      type
  "EVENT_FAULT_DESC"        character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Fault Importance"        character
  "Fault Code"              character
  "Fault Description"       character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$EVENT_INIT <- read.table(
  text = '
  name                      type
  "EVENT_INIT_DESC"         character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Prior Device Time (UTC)" POSIXct
  "External Time Zone"      character
  "Firmware Version"        character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$EVENT_OFFLOAD <- read.table(
  text = '
  name                            type
  "EVENT_OFFLOAD_DESC"            character
  "Device Time (UTC)"             POSIXct
  "Time"                          POSIXct
  "Time Offset (h)"               numeric
  "Time Correction (s)"           numeric
  "Model"                         character
  "Serial Number"                 character
  "External Time (UTC)"           POSIXct
  "External Time Zone"            character
  "HR Total Accepted Detections"  numeric
  "PPM Total Accepted Detections" numeric
  "Memory Remaining (%)"          numeric
  "Battery Remaining (%)"         numeric
  "Original File"                 character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$HEALTH_HR2 <- read.table(
  text = '
  name                            type
  "HEALTH_HR2_DESC"               character
  "Device Time (UTC)"             POSIXct
  "Time"                          POSIXct
  "Time Offset (h)"               numeric
  "Time Correction (s)"           numeric
  "Model"                         character
  "Serial Number"                 character
  "Line Voltage (V)"              numeric
  "Memory Remaining (%)"          numeric
  "RTC Time"                      POSIXct
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$HEALTH_VR2AR <- read.table(
  text = '
  name                            type
  "HEALTH_VR2AR_DESC"             character
  "Device Time (UTC)"             POSIXct
  "Time"                          POSIXct
  "Time Offset (h)"               numeric
  "Time Correction (s)"           numeric
  "Model"                         character
  "Serial Number"                 character
  "Memory Remaining (%)"          numeric
  "RTC Time"                      POSIXct
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$HEALTH_VR2TX <- read.table(
  text = '
  name                            type
  "HEALTH_VR2TX_DESC"             character
  "Device Time (UTC)"             POSIXct
  "Time"                          POSIXct
  "Time Offset (h)"               numeric
  "Time Correction (s)"           numeric
  "Model"                         character
  "Serial Number"                 character
  "Memory Remaining (%)"          numeric
  "RTC Time"                      POSIXct
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$HEALTH_VR4 <- read.table(
  text = '
  name                            type
  "HEALTH_VR4_DESC"               character
  "Device Time (UTC)"             POSIXct
  "Time"                          POSIXct
  "Time Offset (h)"               numeric
  "Time Correction (s)"           numeric
  "Model"                         character
  "Serial Number"                 character
  "Memory Remaining (%)"          numeric
  "Relative Humidity (%)"         numeric
  "RTC Time"                      POSIXct
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$HEALTH_VR2W <- read.table(
  text = '
  name                            type
  "HEALTH_VR2W_DESC"              character
  "Device Time (UTC)"             POSIXct
  "Time"                          POSIXct
  "Time Offset (h)"               numeric
  "Time Correction (s)"           numeric
  "Model"                         character
  "Serial Number"                 character
  "Memory Remaining (%)"          numeric
  "RTC Time"                      POSIXct
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$NOISE_STATS_VR2AR <- read.table(
  text = '
  name                      type
  "NOISE_STATS_VR2AR_DESC"  character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Noise Min (mV)"          numeric
  "Noise Max (mV)"          numeric
  "Noise Mean (mV)"         numeric
  "Sample Count"            numeric
  "Accumulation Period (s)" numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$NOISE_STATS_VR2TX <- read.table(
  text = '
  name                      type
  "NOISE_STATS_VR2TX_DESC"  character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Noise Min (mV)"          numeric
  "Noise Max (mV)"          numeric
  "Noise Mean (mV)"         numeric
  "Sample Count"            numeric
  "Accumulation Period (s)" numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$TEMP <- read.table(
  text = '
  name                      type
  "TEMP_DESC"               character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Ambient (deg C)"         numeric
  "Internal (deg C)"        numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v1.0.0$TEMP_STATS <- read.table(
  text = '
  name                      type
  "TEMP_STATS_DESC"         character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Ambient Min (deg C)"     numeric
  "Ambient Max (deg C)"     numeric
  "Ambient Mean (deg C)"    numeric
  "Sample Count"            numeric
  "Accumulation Period (s)" numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)


vdat_csv_schema$v2.0.0 <- list(
  "ATTITUDE" = NA,
  "BATTERY" = NA,
  "CFG_CHANNEL" = NA,
  "CFG_RECEIVER_HR3" = NA, # new v2.0.0
  "CFG_STATION" = NA,
  "CFG_STUDY" = NA,
  "CFG_TRANSMITTER" = NA,
  "CLOCK_REF" = NA,
  "CLOCK_SET" = NA,
  "DATA_ERROR" = NA,
  "DATA_SOURCE_FILE" = NA,
  "DEPTH" = NA,
  "DEPTH_STATS" = NA,
  "DET" = NA,
  "DET_HTI" = NA, # new v2.0.0
  "DET_FILTER" = NA,
  "DET_SENS" = NA, # new v2.0.0
  "DIAG" = NA,
  "DIAG_FAST" = NA,
  "DIAG_HR2" = NA,
  "DIAG_HR3" = NA, # new v2.0.0
  "DIAG_VR2W" = NA,
  "DIAG_VR2W_INTERIM" = NA,
  "DIAG_VR2AR" = NA,
  "DIAG_VR2AR_INTERIM" = NA,
  "DIAG_VR2TX" = NA,
  "DIAG_VR2TX_INTERIM" = NA,
  "DIAG_VR4" = NA,
  "EVENT" = NA,
  "EVENT_FAULT" = NA,
  "EVENT_INIT" = NA,
  "EVENT_OFFLOAD" = NA,
  "HEALTH_HR2" = NA,
  "HEALTH_HR3" = NA, # new v2.0.0
  "HEALTH_VR2AR" = NA,
  "HEALTH_VR2TX" = NA,
  "HEALTH_VR2W" = NA,
  "HEALTH_VR4" = NA,
  "HEALTH_NEXTRAK" = NA, #new vdat-10.6.0-20240716-1903df-release
  "NOISE_STATS_VR2AR" = NA,
  "NOISE_STATS_VR2TX" = NA,
  "PING" = NA, # new v2.0.0
  "TEMP" = NA,
  "TEMP_STATS" = NA
  #"XPND_EVENT" = NA # new v2.0.0; but absent vdat-10.6.0
)

vdat_csv_schema$v2.0.0$ATTITUDE <- read.table(
  text = '
  name                   type
  "ATTITUDE_DESC"        character
  "Device Time (UTC)"    POSIXct
  "Time"                 POSIXct
  "Time Offset (h)"      numeric
  "Time Correction (s)"  numeric
  "Model"                character
  "Serial Number"        character
  "Tilt (deg)"           numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$BATTERY <- read.table(
  text = '
  name                     type
  "BATTERY_DESC"           character
  "Device Time (UTC)"      POSIXct
  "Time"                   POSIXct
  "Time Offset (h)"        numeric
  "Time Correction (s)"    numeric
  "Model"                  character
  "Serial Number"          character
  "Battery Position"       character
  "Battery Type"           character
  "Battery Serial Number"  character
  "Battery Voltage (V)"    numeric
  "Battery Remaining (%)"  numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$CFG_CHANNEL <- read.table(
  text = '
  name                     type
  "CFG_CHANNEL_DESC"       character
  "Device Time (UTC)"      POSIXct
  "Time"                   POSIXct
  "Time Offset (h)"        numeric
  "Time Correction (s)"    numeric
  "Model"                  character
  "Serial Number"          character
  "Channel"                character
  "Detection Type"         character
  "Frequency (kHz)"        numeric
  "Blanking (ms)"          numeric
  "Map ID"                 character
  "Coding ID"              character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)


vdat_csv_schema$v2.0.0$CFG_RECEIVER_HR3 <- read.table(
  text = '
  name                                  type
 "CFG_RECEIVER_HR3_DESC"                character
 "Device Time (UTC)"                    POSIXct
 "Time"                                 POSIXct
 "Time Offset (h)"                      numeric
 "Time Correction (s)"                  numeric
 "Model"                                character
 "Serial Number"                        character
 "Mode"                                 character
 "HR Bitscore Threshold"                numeric
 "HR Detection Combining Timeout (s)"   numeric
 "HTI Threshold Factor"                numeric
 "HTI Threshold Minimum"                numeric
 "HTI Filter"                           character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$CFG_STATION <- read.table(
  text = '
  name                     type
 "CFG_STATION_DESC"        character
 "Device Time (UTC)"       POSIXct
 "Time"                    POSIXct
 "Time Offset (h)"         numeric
 "Time Correction (s)"      numeric
 "Model"                   character
 "Serial Number"           character
 "Station Name"            character
 "Latitude (deg)"          numeric
 "Longitude (deg)"         numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$CFG_STUDY <- read.table(
  text = '
  name                     type
  "CFG_STUDY_DESC"         character
  "Device Time (UTC)"      POSIXct
  "Time"                   POSIXct
  "Time Offset (h)"        numeric
  "Time Correction (s)"    numeric
  "Model"                  character
  "Serial Number"          character
  "Description"            character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$CFG_TRANSMITTER <- read.table(
  text = '
  name                     type
  "CFG_TRANSMITTER_DESC"   character
  "Device Time (UTC)"      POSIXct
  "Time"                   POSIXct
  "Time Offset (h)"        numeric
  "Time Correction (s)"    numeric
  "Model"                  character
  "Serial Number"          character
  "Transmission Type"      character
  "Full ID"                character
  "ID"                     character
  "Power Level"            character
  "Min Delay (s)"          numeric
  "Max Delay (s)"          numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$CLOCK_REF <- read.table(
  text = '
  name                      type
  "CLOCK_REF_DESC"          character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "External Time (UTC)"     POSIXct
  "External Difference (s)" numeric
  "Source"                  character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$CLOCK_SET <- read.table(
  text = '
  name                      type
  "CLOCK_SET_DESC"          character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Prior Device Time (UTC)" POSIXct
  "Prior Difference (s)"    numeric
  "Source"                  character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$DATA_ERROR <- read.table(
  text = '
  name                      type
  "DATA_ERROR_DESC"         character
  "Type"                    character
  "Error"                   character
  "Page"                    character
  "Offset"                  character
  "Description"             character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$DATA_SOURCE_FILE <- read.table(
  text = '
  name                      type
  "DATA_SOURCE_FILE_DESC"   character
  "File Name"               character
  "UUID"                    character
  "Type"                    character
  "Size"                    character
  "State"                   character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$DEPTH <- read.table(
  text = '
  name                      type
  "DEPTH_DESC"              character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Depth (m)"               numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$DEPTH_STATS <- read.table(
  text = '
  name                      type
  "DEPTH_STATS_DESC"        character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Depth Min (m)"           numeric
  "Depth Max (m)"           numeric
  "Depth Mean (m)"          numeric
  "Sample Count"            numeric
  "Accumulation Period (s)" numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$DET <- read.table(
  text = '
  name                      type
  "DET_DESC"                character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Channel"                 character
  "Detection Type"          character
  "Full ID"                 character
  "ID"                      character
  "Raw Data"                character
  "Transmitter Serial"      character
  "Signal Strength (dB)"    numeric
  "Noise (dB)"              numeric
  "Gain (dB)"               numeric
  "Quality Score"           numeric
  "Station Name"            character
  "Latitude"                numeric
  "Longitude"               numeric
  "GPS HDOP"                numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$DET_HTI <- read.table(
  text = '
  name                      type
  "DET_HTI_DESC"            character
  "Device Time (UTC)"       POSIXct
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Peak Amplitude"          numeric
  "Noise"                   numeric
  "Threshold"               numeric
  "PW -3dB (ms)"            numeric
  "PW -6dB (ms)"            numeric
  "PW -12dB (ms)"           numeric
  "Frequency Offset (Hz)"   numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$DET_FILTER <- read.table(
  text = '
  name                      type
  "DET_FILTER_DESC"         character
  "Filter Name"             character
  "Details"                 character
  "Rejected Detections"     numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$DET_SENS <- read.table(
  text = '
  name                      type
  "DET_SENS_DESC"           character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Channel"                 character
  "Detection Type"          character
  "Full ID"                 character
  "ID"                      character
  "Raw Data"                character
  "Transmitter Serial"      character
  "Signal Strength (dB)"    numeric
  "Noise (dB)"              numeric
  "Gain (dB)"               numeric
  "Quality Score"           numeric
  "Station Name"            character
  "Latitude"                numeric
  "Longitude"               numeric
  "GPS HDOP"                numeric
  "Transmitter Type"        character
  "Sensor Function"         character
  "Sensor Value"            numeric
  "Sensor Unit"             character
  "Sensor Precision"        character
  "Definition Source"       character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)


vdat_csv_schema$v2.0.0$DIAG <- read.table(
  text = '
  name                          type
  "DIAG_DESC"                   character
  "Device Time (UTC)"           POSIXct
  "Time"                        POSIXct
  "Time Offset (h)"             numeric
  "Time Correction (s)"         numeric
  "Model"                       character
  "Serial Number"               character
  "Ambient Temperature (deg C)" numeric
  "Noise Mean (mV)"             numeric
  "Tilt (deg)"                  numeric
  "Depth (m)"                   numeric
  "PPM Pings"                   numeric
  "PPM Detections"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$DIAG_FAST <- read.table(
  text = '
  name                          type
  "DIAG_FAST_DESC"              character
  "Device Time (UTC)"           POSIXct
  "Time"                        POSIXct
  "Time Offset (h)"             numeric
  "Time Correction (s)"         numeric
  "Model"                       character
  "Serial Number"               character
  "Ambient Temperature (deg C)" numeric
  "Noise (mV)"                  numeric
  "Tilt (deg)"                  numeric
  "Depth (m)"                   numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$DIAG_HR2 <- read.table(
  text = '
  name                          type
  "DIAG_HR2_DESC"               character
  "Device Time (UTC)"           POSIXct
  "Time"                        POSIXct
  "Time Offset (h)"             numeric
  "Time Correction (s)"         numeric
  "Model"                       character
  "Serial Number"               character
  "Tilt (deg)"                  numeric
  "HR Noise (dB)"               numeric
  "HR Accepted Detections"      numeric
  "HR Rejected Detections"      numeric
  "PPM Pings (D1)"              numeric
  "PPM Detections (D1)"         numeric
  "PPM Pings (D2)"              numeric
  "PPM Detections (D2)"         numeric
  "PPM Noise (dB)"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$DIAG_HR3 <- read.table(
  text = '
  name                          type
  "DIAG_HR3_DESC"               character
  "Device Time (UTC)"           POSIXct
  "Time"                        POSIXct
  "Time Offset (h)"             numeric
  "Time Correction (s)"         numeric
  "Model"                       character
  "Serial Number"               character
  "Tilt (deg)"                  numeric
  "HR Noise (dB)"               numeric
  "HR Accepted Detections"      numeric
  "HR Rejected Detections"      numeric
  "PPM Pings (D1)"              numeric
  "PPM Detections (D1)"         numeric
  "PPM Pings (D2)"              numeric
  "PPM Detections (D2)"         numeric
  "PPM Noise (dB)"              numeric
  "HTI Detections"              numeric
  "HTI Noise (dB)"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$DIAG_VR2W <- read.table(
  text = '
  name                      type
  "DIAG_VR2W_DESC"          character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Pings"                   numeric
  "Detections"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$DIAG_VR2W_INTERIM <- read.table(
  text = '
  name                      type
  "DIAG_VR2W_INTERIM_DESC"  character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Pings"                   numeric
  "Detections"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)


vdat_csv_schema$v2.0.0$DIAG_VR2AR <- read.table(
  text = '
  name                      type
  "DIAG_VR2AR_DESC"         character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Pings"                   numeric
  "Detections"              numeric
  "Noise (mV)"              numeric
  "Tilt (deg)"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$DIAG_VR2AR_INTERIM <- read.table(
  text = '
  name                      type
  "DIAG_VR2AR_INTERIM_DESC" character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Pings"                   numeric
  "Detections"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$DIAG_VR2TX <- read.table(
  text = '
  name                      type
  "DIAG_VR2TX_DESC"         character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Pings"                   numeric
  "Detections"              numeric
  "Noise (mV)"              numeric
  "Tilt (deg)"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$DIAG_VR2TX_INTERIM <- read.table(
  text = '
  name                      type
  "DIAG_VR2TX_INTERIM_DESC" character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Pings"                   numeric
  "Detections"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$DIAG_VR4 <- read.table(
  text = '
  name                      type
  "DIAG_VR4_DESC"           character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Pings (69 kHz)"          numeric
  "Detections (69 kHz)"     numeric
  "Pings (180 kHz)"         numeric
  "Detections (180 kHz)"    numeric
  "Tilt (deg)"              numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)


vdat_csv_schema$v2.0.0$EVENT <- read.table(
  text = '
  name                      type
  "EVENT_DESC"              character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Type"                    character
  "Subtype"                 character
  "Description"             character
  "Values"                  character
  "Latitude"                numeric
  "Longitude"               numeric
  "GPS HDOP"                numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$EVENT_FAULT <- read.table(
  text = '
  name                      type
  "EVENT_FAULT_DESC"        character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Fault Importance"        character
  "Fault Code"              character
  "Fault Description"       character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$EVENT_INIT <- read.table(
  text = '
  name                      type
  "EVENT_INIT_DESC"         character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Prior Device Time (UTC)" POSIXct
  "External Time Zone"      character
  "Firmware Version"        character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$EVENT_OFFLOAD <- read.table(
  text = '
  name                            type
  "EVENT_OFFLOAD_DESC"            character
  "Device Time (UTC)"             POSIXct
  "Time"                          POSIXct
  "Time Offset (h)"               numeric
  "Time Correction (s)"           numeric
  "Model"                         character
  "Serial Number"                 character
  "External Time (UTC)"           POSIXct
  "External Time Zone"            character
  "HR Total Accepted Detections"  numeric
  "PPM Total Accepted Detections" numeric
  "Memory Remaining (%)"          numeric
  "Battery Remaining (%)"         numeric
  "Original File"                 character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$HEALTH_HR2 <- read.table(
  text = '
  name                            type
  "HEALTH_HR2_DESC"               character
  "Device Time (UTC)"             POSIXct
  "Time"                          POSIXct
  "Time Offset (h)"               numeric
  "Time Correction (s)"           numeric
  "Model"                         character
  "Serial Number"                 character
  "Line Voltage (V)"              numeric
  "Memory Remaining (%)"          numeric
  "RTC Time"                      POSIXct
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$HEALTH_HR3 <- read.table(
  text = '
  name                            type
  "HEALTH_HR3_DESC"               character
  "Device Time (UTC)"             POSIXct
  "Time"                          POSIXct
  "Time Offset (h)"               numeric
  "Time Correction (s)"           numeric
  "Model"                         character
  "Serial Number"                 character
  "Line Voltage (V)"              numeric
  "Memory Remaining (%)"          numeric
  "RTC Time"                      POSIXct
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$HEALTH_VR2AR <- read.table(
  text = '
  name                            type
  "HEALTH_VR2AR_DESC"             character
  "Device Time (UTC)"             POSIXct
  "Time"                          POSIXct
  "Time Offset (h)"               numeric
  "Time Correction (s)"           numeric
  "Model"                         character
  "Serial Number"                 character
  "Memory Remaining (%)"          numeric
  "RTC Time"                      POSIXct
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$HEALTH_VR2TX <- read.table(
  text = '
  name                            type
  "HEALTH_VR2TX_DESC"             character
  "Device Time (UTC)"             POSIXct
  "Time"                          POSIXct
  "Time Offset (h)"               numeric
  "Time Correction (s)"           numeric
  "Model"                         character
  "Serial Number"                 character
  "Memory Remaining (%)"          numeric
  "RTC Time"                      POSIXct
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$HEALTH_VR2W <- read.table(
  text = '
  name                            type
  "HEALTH_VR2W_DESC"              character
  "Device Time (UTC)"             POSIXct
  "Time"                          POSIXct
  "Time Offset (h)"               numeric
  "Time Correction (s)"           numeric
  "Model"                         character
  "Serial Number"                 character
  "Memory Remaining (%)"          numeric
  "RTC Time"                      POSIXct
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$HEALTH_VR4 <- read.table(
  text = '
  name                            type
  "HEALTH_VR4_DESC"               character
  "Device Time (UTC)"             POSIXct
  "Time"                          POSIXct
  "Time Offset (h)"               numeric
  "Time Correction (s)"           numeric
  "Model"                         character
  "Serial Number"                 character
  "Memory Remaining (%)"          numeric
  "Relative Humidity (%)"         numeric
  "RTC Time"                      POSIXct
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$HEALTH_NEXTRAK <- read.table(
  text = '
  name                            type
  "HEALTH_NEXTRAK_DESC"           character
  "Device Time (UTC)"             POSIXct
  "Time"                          POSIXct
  "Time Offset (h)"               numeric
  "Time Correction (s)"           numeric
  "Model"                         character
  "Serial Number"                 character
  "Memory Remaining (%)"          numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$NOISE_STATS_VR2AR <- read.table(
  text = '
  name                      type
  "NOISE_STATS_VR2AR_DESC"  character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Noise Min (mV)"          numeric
  "Noise Max (mV)"          numeric
  "Noise Mean (mV)"         numeric
  "Sample Count"            numeric
  "Accumulation Period (s)" numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$NOISE_STATS_VR2TX <- read.table(
  text = '
  name                      type
  "NOISE_STATS_VR2TX_DESC"  character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Noise Min (mV)"          numeric
  "Noise Max (mV)"          numeric
  "Noise Mean (mV)"         numeric
  "Sample Count"            numeric
  "Accumulation Period (s)" numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$PING <- read.table(
  text = '
  name                      type
  "PING_DESC"               character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Channel"                 character
  "Frequency (kHz)"         numeric
  "Blanking (ms)"           numeric
  "Signal Strength (dB)"    numeric
  "Noise (dB)"              numeric
  "Gain (dB)"               numeric
  "Latitude"                numeric
  "Longitude"               numeric
  "GPS HDOP"                numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$TEMP <- read.table(
  text = '
  name                      type
  "TEMP_DESC"               character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Ambient (deg C)"         numeric
  "Internal (deg C)"        numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

vdat_csv_schema$v2.0.0$TEMP_STATS <- read.table(
  text = '
  name                      type
  "TEMP_STATS_DESC"         character
  "Device Time (UTC)"       POSIXct
  "Time"                    POSIXct
  "Time Offset (h)"         numeric
  "Time Correction (s)"     numeric
  "Model"                   character
  "Serial Number"           character
  "Ambient Min (deg C)"     numeric
  "Ambient Max (deg C)"     numeric
  "Ambient Mean (deg C)"    numeric
  "Sample Count"            numeric
  "Accumulation Period (s)" numeric
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

# XPND_EVENT was present in vdat.exe v. 9.3 but not 10.6
# vdat_csv_schema$v2.0.0$XPND_EVENT <- read.table(
#   text = '
#   name                      type
#   "XPND_EVENT_DESC"         character
#   "Device Time (UTC)"       POSIXct
#   "Time"                    POSIXct
#   "Time Offset (h)"         numeric
#   "Time Correction (s)"     numeric
#   "Model"                   character
#   "Serial Number"           character
#   "Type"                    character
#   "Subtype"                 character
#   "Description"             character
#   "Values"                  character
#   "Responder Model"         character
#   "Responder Serial Number" character
#   "Responder Range (m)"     numeric
#   "Transmit Power (dB)"     numeric
#   "Receive Signal (dB)"     numeric
#   "Receive Gain (dB)"       numeric
#   "Latitude"                numeric
#   "Longitude"               numeric
#   "GPS HDOP"                numeric
#   ',
#   header = TRUE,
#   stringsAsFactors = FALSE,
#   check.names = FALSE,
#   comment.char = ""
# )

# usethis::use_data(vdat_csv_schema, overwrite = TRUE)
