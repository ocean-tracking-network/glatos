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
csv_file <- system.file("extdata", "detection_files_raw",
                        "VR2W_109924_20110718_1.csv",
                        package = "glatos")

