## ----setup, include = FALSE--------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

#set 'str' options to desired output format
str_opts <- getOption("str") #get list of options
str_opts$strict.width = "wrap"
str_opts$vec.len = 1
options(str = str_opts)

#set 'width'
options(width = 85)


## ----results = "hide", warning = FALSE, message = FALSE----------------------------
#install.packages("data.table")
library(data.table)

#install.packages("lubridate")
library(lubridate)

## ----results = "hide", warning = FALSE, message = FALSE----------------------------
dx <- data.frame(a = 1 , b = 2)
setDT(dx) #convert to data.table

## ----echo=TRUE---------------------------------------------------------------------
# Set path to walleye_detections.csv example dataset
wal_det_file <- system.file("extdata", "walleye_detections.csv", 
                            package = "glatos")

## ----message = FALSE---------------------------------------------------------------
# Attach glatos package
library(glatos)

# Read in the walleye_detections.csv file using `read_glatos_detections`
walleye_detections <- read_glatos_detections(wal_det_file)

## ----echo=TRUE---------------------------------------------------------------------
# View the structure and data from first row
str(walleye_detections)

## ----echo=TRUE---------------------------------------------------------------------
# Set path to blue_shark_detections.csv example dataset
shrk_det_file <- system.file("extdata", "blue_shark_detections.csv",
                             package = "glatos")

# Read in the blue_shark_detections.csv file using `read_otn_detections`
blue_shark_detections <- read_otn_detections(shrk_det_file)


# View the structure of blue_shark_detections
str(blue_shark_detections)

## ---- echo=TRUE--------------------------------------------------------------------
#get path to example CSV file included with glatos package
csv_file <- system.file("extdata", "VR2W_109924_20110718_1.csv",
                        package = "glatos")

## ----------------------------------------------------------------------------------
dtc <- read.csv(csv_file, as.is = TRUE, check.names = FALSE, 
                fileEncoding = "UTF-8-BOM")


## ----eval = FALSE------------------------------------------------------------------
#  #read data from csv file using data.table::fread
#  dtc <- fread(csv_file)

## ----------------------------------------------------------------------------------
#change column name
names(dtc)[match("Date and Time (UTC)", names(dtc))] <- "detection_timestamp_utc"

## ----eval = FALSE------------------------------------------------------------------
#  #use data.table::setnames to change column names via old and new names
#  setnames(dtc, "Date and Time (UTC)", "detection_timestamp_utc")

## ----------------------------------------------------------------------------------

dtc$detection_timestamp_utc <- as.POSIXct(dtc$detection_timestamp_utc,
                                          tz = "UTC")

#take a peek
str(dtc$detection_timestamp_utc)

#first few records
head(dtc$detection_timestamp_utc, 3)

## ----eval = FALSE------------------------------------------------------------------
#  #use ':=' to format timestamps
#  dtc[ , detection_timestamp_utc := as.POSIXct(detection_timestamp_utc,
#                                               tz = "UTC")]

## ----eval = FALSE------------------------------------------------------------------
#  #lubridate::fast_strptime is the fastest way we know to ceorce strings to POSIX
#  dtc[ , detection_timestamp_utc :=
#           lubridate::fast_strptime(detection_timestamp_utc,
#                                    format = "%Y-%m-%d %H:%M:%OS",
#                                    tz = "UTC",
#                                    lt = FALSE)]

## ----------------------------------------------------------------------------------
dtc$Receiver[1]

## ----------------------------------------------------------------------------------
#make new function to extract second element from a hyphen-delimited string
get_rsn <- function(x) strsplit(x, "-")[[1]][2]

#apply get_rsn() to each record in Receiver column
dtc$receiver_sn <- sapply(dtc$Receiver, get_rsn)

## ----eval = FALSE------------------------------------------------------------------
#  #make new column "receiver_sn"; parse from "Receiver"
#  dtc[ , receiver_sn := get_rsn(Receiver), by = "Receiver"]

## ----------------------------------------------------------------------------------
#make an example receiver data frame
rcv <- data.frame(
        glatos_array = "DWM",
        station = "DWM-001", 
        deploy_lat = 45.65738, 
        deploy_long = -84.46418, 
        deploy_date_time = as.POSIXct("2011-04-11 20:30:00", tz = "UTC"),
        recover_date_time = as.POSIXct("2011-07-08 17:11:00", tz = "UTC"),
        ins_serial_no = "109924",
        stringsAsFactors = FALSE) 

#left join on receiver serial number to add receiver data to detections
dtc <- merge(dtc, rcv, by.x = "receiver_sn", by.y = "ins_serial_no", 
             all.x = TRUE)

# take a look at first few rows
head(dtc, 3)

## ----------------------------------------------------------------------------------
#count rows before subset
nrow(dtc)

#subset deployments between receiver deployment and recovery (omit others)
dtc <- with(dtc, dtc[detection_timestamp_utc >= deploy_date_time & 
                     detection_timestamp_utc <= recover_date_time, ])


## ----eval = FALSE------------------------------------------------------------------
#  #subset deployments between receiver deployment and recovery (omit others)
#  dtc <- dtc[between(detection_timestamp_utc,
#                     deploy_date_time, recover_date_time), ]

## ----------------------------------------------------------------------------------
#count rows after subset
nrow(dtc)

## ----------------------------------------------------------------------------------
#make a new function to extract id from Transmitter
#i.e., get third element of hyphen-delimited string
parse_tid <- function(x) strsplit(x, "-")[[1]][3]

#make a new function to extract codespace from Transmitter
#i.e., get first two elements of hyphen-delimited string
parse_tcs <- function(x) {
    #split on "-" and keep first two extracted elements
    tx <- strsplit(x, "-")[[1]][1:2]
    #re-combine and separate by "-"
    return(paste(tx[1:2], collapse = "-"))
  }

#apply parse_tcs() to Transmitter and assign to transmitter_codespace
dtc$transmitter_codespace <- sapply(dtc$Transmitter, parse_tcs)

#apply parse_tid() to Transmitter and assign to transmitter_id
dtc$transmitter_id <- sapply(dtc$Transmitter, parse_tid)


## ----eval = FALSE------------------------------------------------------------------
#  dtc[ , `:=`(transmitter_codespace = parse_tcs(Transmitter),
#              transmitter_id = parse_tid(Transmitter),
#         by = "Transmitter"]

## ----------------------------------------------------------------------------------
#change column name
names(dtc)[match(c("Sensor Value", "Sensor Unit"), names(dtc))] <- 
                 c("sensor_value", "sensor_unit")

## ----eval = FALSE------------------------------------------------------------------
#  setnames(dtc, c("Sensor Value", "Sensor Unit"),
#                c("sensor_value", "sensor_unit"))

## ----------------------------------------------------------------------------------
str(dtc)

## ----------------------------------------------------------------------------------
#make an example animal (fish) data frame
fsh <- data.frame(
        animal_id = c("1", "4", "7", "128"), 
        tag_code_space = "A69-1601",
        tag_id_code = c("439", "442", "445", "442"), 
        common_name = "Sea Lamprey", 
        release_date_time = as.POSIXct(c("2011-05-05 12:00", 
                                         "2011-05-05 12:00", 
                                         "2011-05-06 12:00", 
                                         "2011-06-08 12:00"), 
                                       tz = "UTC"),
        recapture_date_time = as.POSIXct(c(NA, "2011-05-26 15:00", NA, NA),
                                         tz = "UTC"), 
        stringsAsFactors = FALSE)

#simple left join on codespace and id
dtc <- merge(dtc, fsh, by.x = c("transmitter_codespace", "transmitter_id"), 
                       by.y = c("tag_code_space", "tag_id_code"),
                       all.x = TRUE)
          

## ----------------------------------------------------------------------------------
#count rows before subset
nrow(dtc)

#subset detections to include only those between release and recapture 
# or after release if never recaptured
dtc <- with(dtc, dtc[detection_timestamp_utc >= release_date_time & 
                     (detection_timestamp_utc <= recapture_date_time |
                        is.na(recapture_date_time)) , ])

## ----eval = FALSE------------------------------------------------------------------
#  dtc <- dtc[between(detection_timestamp_utc, release_date_time,
#                     recapture_date_time) | is.na(recapture_date_time), ]

## ----------------------------------------------------------------------------------
#count rows after subset
nrow(dtc)

## ----echo=TRUE---------------------------------------------------------------------
#get path to example receiver_locations file
rec_file <- system.file("extdata", "sample_receivers.csv", 
                        package = "glatos")

#read sample_receivers.csv using 'read_glatos_receivers'
rcv <- read_glatos_receivers(rec_file)

#view structure
str(rcv)

## ----echo=TRUE---------------------------------------------------------------------
#get path to example walleye_workbook.xlsm file
wb_file <- system.file("extdata", "walleye_workbook.xlsm", 
                        package = "glatos")

#read walleye_workbook.xlsm using 'read_glatos_workbook'
wb <- read_glatos_workbook(wb_file)

#view structure
class(wb)
names(wb)

## ----------------------------------------------------------------------------------
#extract receivers element from workbook list
rcv2 <- wb[["receivers"]]

#view structure
str(rcv2)


## ----------------------------------------------------------------------------------
#extract animals element from workbook list
fsh <- wb[["animals"]]

#view structure
str(fsh)


## ----echo=TRUE---------------------------------------------------------------------
#get path to example lamprey_tag_specs.xls file
spec_file <- system.file("extdata", "lamprey_tag_specs.xls", 
                        package = "glatos")

#read lamprey_tag_specs.xls using 'read_vemco_tag_specs'
my_tags <- read_vemco_tag_specs(spec_file, file_format = "vemco_xls")

#view structure
class(my_tags)
names(my_tags)

## ----------------------------------------------------------------------------------
#view structure of specs element
str(my_tags$specs)


## ----------------------------------------------------------------------------------
#view structure of schedule element
str(my_tags$schedule)


