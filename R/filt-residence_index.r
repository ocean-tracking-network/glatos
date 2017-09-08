#This is an R file form res-index-R in GitLab

library("dplyr")
library("lubridate")
library("plotly")
# total_days_diff()
# -----------------
# The function below determines the total days difference.
#
# The difference is determined by the minimal startdate of every detection and the maximum enddate of every detection.
# Both are converted into a datetime then subtracted to get a timedelta. The timedelta
# is converted to seconds and divided by the number of seconds in a day (86400). The function
# returns a floating point number of days (i.e. 503.76834).
#
# @var Detections - data frame pulled from the compressed detections CSV

#For GLATOS/OTN/sample detections data:
#To work with OTN/GLATOS/sample data
# To use:
# For glatos data, total_diff_days(glatos, "GLATOS")
# For OTN detections data, total_diff_days(otnDet, "OTNDet")
# For OTN compressed data, total_diff_days(otnComp, "OTNComp")
total_diff_days <- function(detections, type, detColNames=list()) {
  # Check if user has set column names
  if(length(detColNames) == 0) {
    if(type == "GLATOS") {
      detColNames = list(startDate="detection_timestamp_utc", endDate="detection_timestamp_utc")
    } else if (type == "OTNDet") {
      detColNames = list(startDate="datecollected", endDate="datecollected")
    } else if (type == "OTNComp") {
      detColNames = list(startDate="startdate", endDate="enddate")
    } else if (type == "sample") {
      detColNames = list(startDate="startdate", endDate="enddate")
    } else {
      stop(paste0("The type '",type,"' is not defined."), call.=FALSE)
    }
  }
  
  # Check that the specified columns appear in the detections dataframe
  missingCols <- setdiff(unlist(detColNames), names(detections))
  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }
  
  # Subset detections with only user-defined columns and change names
  # this makes code more easy to understand (esp. ddply)
  detections <- detections[,unlist(detColNames)] #subset
  names(detections) <- c("startdate", "enddate")
  #print(head(detections))
  
  # Check that startdate and enddate are of class 'POSIXct'
  if(!('POSIXct' %in% class(detections$startdate))){
    print(head(detections$startdate))
    print(class(detections$startdate))
    print(detColNames$startdate)
    stop(paste0("Column1 '",detColNames$startdate,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  } 
  if(!('POSIXct' %in% class(detections$enddate))) {
    stop(paste0("Column '",detColNames$enddate,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  first <- detections$startdate[which.min(detections$startdate)]
  last <- detections$enddate[which.max(detections$enddate)]
  total <- as.double(difftime(last, first, units="secs"))/86400.0
  return(total)
}


# total_days_count()
# ------------------
# The function below takes a Pandas DataFrame and determines the number of days any
# detections were seen on the array.
#
# The function converts both the startdate and enddate columns into a date with no hours, minutes,
# or seconds. Next it creates a list of the unique days where a detection was seen. The size of the
# list is returned as the total number of days as an integer.
#
# *** NOTE ****
# Possible rounding error may occur as a detection on 2016-01-01 23:59:59 and a detection on
# 2016-01-02 00:00:01 would be counted as days when it is really 2-3 seconds.
#
#
# @var Detections - data frame pulled from the compressed detections CSV

#To work with OTN/GLATOS/sample data
# To use:
# For glatos data, total_days_count(glatos, "GLATOS")
# For OTN detections data, total_days_count(otnDet, "OTNDet")
# For OTN compressed data, total_days_count(otnComp, "OTNComp")
total_days_count <- function(detections, type, detColNames=list()) {
  # Check if user has set column names
  if(length(detColNames) == 0) {
    if(type == "GLATOS") {
      detColNames = list(startDate="detection_timestamp_utc", endDate="detection_timestamp_utc")
    } else if (type == "OTNDet") {
      detColNames = list(startDate="datecollected", endDate="datecollected")
    } else if (type == "OTNComp") {
      detColNames = list(startDate="startdate", endDate="enddate")
    } else if (type == "sample") {
      detColNames = list(startDate="startdate", endDate="enddate")
    } else {
      stop(paste0("The type '",type,"' is not defined."), call.=FALSE)
    }
  }
  
  # Check that the specified columns appear in the detections dataframe
  missingCols <- setdiff(unlist(detColNames), names(detections))
  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }
  
  # Subset detections with only user-defined columns and change names
  # this makes code more easy to understand (esp. ddply)
  detections <- detections[,unlist(detColNames)] #subset
  names(detections) <- c("startdate", "enddate")
  
  # Check that startdate and enddate are of class 'POSIXct'
  if(!('POSIXct' %in% class(detections$startdate))){
    stop(paste0("Column '",detColNames$startdate,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  } 
  if(!('POSIXct' %in% class(detections$enddate))) {
    stop(paste0("Column '",detColNames$enddate,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  startdays <- distinct(select(mutate(detections, days = as.Date(startdate)), days))
  enddays <- distinct(select(mutate(detections, days = as.Date(enddate)), days))
  days <- bind_rows(startdays,enddays)
  daycount <- as.double(count(distinct(select(days,days ))))
  return(daycount)
}



# aggregate_total_with_overlap()
# ----------------------------------------
#
# The function below aggregates timedelta of startdate and enddate of each detection into
# a final timedelta then returns a float of the number of days. If the startdate and enddate
# are the same, a timedelta of one second is assumed.
#
# @var Detections -data frame pulled from the compressed detections CSV

#To work with OTN/GLATOS/sample data:
# To use:
# For glatos data, aggregate_total_with_overlap(glatos, "GLATOS")
# For OTN detections data, aggregate_total_with_overlap(otnDet, "OTNDet")
# For OTN compressed data, aggregate_total_with_overlap(otnComp, "OTNComp")
aggregate_total_with_overlap <- function(detections, type, detColNames=list()) {
  # Check if user has set column names
  if(length(detColNames) == 0) {
    if(type == "GLATOS") {
      detColNames = list(startDate="detection_timestamp_utc", endDate="detection_timestamp_utc")
    } else if (type == "OTNDet") {
      detColNames = list(startDate="datecollected", endDate="datecollected")
    } else if (type == "OTNComp") {
      detColNames = list(startDate="startdate", endDate="enddate")
    } else if (type == "sample") {
      detColNames = list(startDate="startdate", endDate="enddate")
    } else {
      stop(paste0("The type '",type,"' is not defined."), call.=FALSE)
    }
  }
  
  # Check that the specified columns appear in the detections dataframe
  missingCols <- setdiff(unlist(detColNames), names(detections))
  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }
  
  # Subset detections with only user-defined columns and change names
  # this makes code more easy to understand (esp. ddply)
  detections <- detections[,unlist(detColNames)] #subset
  names(detections) <- c("startdate", "enddate")
  
  # Check that startdate and enddate are of class 'POSIXct'
  if(!('POSIXct' %in% class(detections$startdate))){
    stop(paste0("Column '",detColNames$startdate,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  } 
  if(!('POSIXct' %in% class(detections$enddate))) {
    stop(paste0("Column '",detColNames$enddate,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  detections <- mutate(detections, timedelta = as.double(difftime(as.Date(enddate),as.Date(startdate), units="secs")))
  detections <- mutate(detections, timedelta = recode(detections$timedelta, `0` = 1))
  total <- as.double(sum(detections$timedelta))/86400.0
  return(total)
}
# aggregate_total_no_overlap()
# --------------------------------------
#
# The function below aggregates timedelta of startdate and enddate, excluding overlap between
# detections. Any overlap between two detections is converted to a new detection using the earlier
# startdate and the latest enddate. If the startdate and enddate are the same, a timedelta of one
# second is assumed.
#
# @var Detections - data frame pulled from the compressed detections CSV

#To work with OTN/GLATOS/sample data:
# To use:
# For glatos data, aggregate_total_no_overlap(glatos, "GLATOS")
# For OTN detections data, aggregate_total_no_overlap(otnDet, "OTNDet")
# For OTN compressed data, aggregate_total_no_overlap(otnComp, "OTNComp")
aggregate_total_no_overlap <- function(detections, type, detColNames=list()) {
  #Check if user has set column names
  if(length(detColNames) == 0) {
    if(type == "GLATOS") {
      detColNames = list(startDate="detection_timestamp_utc", endDate="detection_timestamp_utc")
    } else if (type == "OTNDet") {
      detColNames = list(startDate="datecollected", endDate="datecollected")
    } else if (type == "OTNComp") {
      detColNames = list(startDate="startdate", endDate="enddate")
    } else if (type == "sample") {
      detColNames = list(startDate="startdate", endDate="enddate")
    } else {
      stop(paste0("The type '",type,"' is not defined."), call.=FALSE)
    }
  }
  
  # Check that the specified columns appear in the detections dataframe
  missingCols <- setdiff(unlist(detColNames), names(detections))
  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }
  
  # Subset detections with only user-defined columns and change names
  # this makes code more easy to understand (esp. ddply)
  detections <- detections[,unlist(detColNames)] #subset
  names(detections) <- c("startdate", "enddate")
  
  # Check that startdate and enddate are of class 'POSIXct'
  if(!('POSIXct' %in% class(detections$startdate))){
    stop(paste0("Column '",detColNames$startdate,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  } 
  if(!('POSIXct' %in% class(detections$enddate))) {
    stop(paste0("Column '",detColNames$enddate,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  total <- 0.0
  detections <- arrange(detections, startdate)
  detcount <- nrow(detections) #Changed to nrow
  detections <- mutate(detections, interval = interval(startdate,enddate)) #'interval' is from lubridate #getting interval column
  detections <- mutate(detections, timedelta = as.double(difftime(as.Date(enddate),as.Date(startdate), units="secs"))) #calculate time difference between startdate and enddate
  detections <- mutate(detections, timedelta = recode(detections$timedelta, `0` = 1)) #change time difference of 0 to 1s (startdate = enddate)
  next_block <- 2
  start_block <- 1
  end_block <- 1
  while(next_block <= detcount) {
    # if it overlaps
    if(next_block < detcount && int_overlaps(nth(detections$interval, end_block), nth(detections$interval, next_block))) { #'int_overlaps' is from lubridate #first block of time overlaps with second block of time
      if(nth(detections$interval, next_block) >= nth(detections$interval, end_block)) {
        end_block <- next_block
      }
      if(end_block == detcount) {
        tdiff <- as.double(difftime(nth(detections$enddate,end_block), nth(detections$startdate, start_block), units="secs"))
        if(tdiff == 0.0) {
          tdiff <- 1
        }
        start_block <- next_block
        end_block <- next_block + 1
        total <- total + as.double(tdiff)
      }
    } else {
      #if it doesn't overlap
      tdiff <- 0.0
      # Generate the time difference between the start of the start block and the end of the end block
      tdiff <- as.double(difftime(nth(detections$enddate,end_block), nth(detections$startdate, start_block), units="secs"))
      start_block <- next_block
      end_block <- next_block
      total <- total + as.double(tdiff)
    }
    next_block <- next_block + 1
  }
  total <- total/86400.0
  return(total)
}





# get_days()
# ----------
# Determines which calculation method to use for the residency index.
#
# Wrapper method for the calulation methods above.
#
# @var dets - data frame pulled from the compressed detections CSV
# @var calculation_method - determines which method above will be used to count total time and station time

#To work with OTN/GLATOS/sample data:
# To use:
#For sample data, get_days(sample, "sample") or with calculation method
# For glatos data, get_days(glatos, "GLATOS") or with calculation method
# For OTN detections data,get_days(otnDet, "OTNDet") or with calculation method
# For OTN compressed data, get_days(otnComp, "OTNComp") or with calculation method

# @var calculation_method - determines which method above will be used to count total time and station time
get_days <- function(dets, type='OTN', calculation_method='kessel') {
  days <- 0
  if (calculation_method == 'aggregate_with_overlap') {
    days = aggregate_total_with_overlap(dets, type)
  } else if(calculation_method == 'aggregate_no_overlap') {
    days = aggregate_total_no_overlap(dets, type)
  } else if(calculation_method == 'timedelta') {
    days <- total_diff_days(dets, type)
  } else {
    days <- total_days_count(dets, type)
  }
  return(days)
}



# residence_index()
# -----------------
#
# This function takes in a commpressed detections CSV and determines the residency
# index for each station.
#
# Residence Index (RI) was calculated as the number of days an individual fish was
# detected at each receiver station divided by the total number of days the fish was
# detected anywhere on the acoustic array. - Kessel et al.
#
# @var Detections - CSV Path
# @var stations_file - CSV path
# @var type - string
# @var calculation_method - string
residency_index <- function(data, station_locs, type, calculation_method='kessel', write_file=FALSE) {
  #data <- read.csv(filename)
  
  data <- filter(data, !grepl('release', startunqdetecid))
  total_days = get_days(data, calculation_method)
  
  ri <- data.frame('days_detected'=numeric(),'residency_index'=numeric(), 'station'=character())
  
  stations <- distinct(select(data, station))
  for (index in 1:as.integer(count(stations))) {
    stn <- as.character(slice(stations, index)$station)
    stn_data <- filter(data, station == stn)
    total_stn_days <- get_days(stn_data, calculation_method)
    res_index = as.double(total_stn_days)/total_days
    row <- data.frame('days_detected'=total_stn_days,'residency_index'=res_index, 'station'=stn)
    ri <- rbind(ri, row)
  }
  #station_locs <- read.csv(stations_file)
  station_locs$station <- as.character(station_locs$station)
  ri <- left_join(ri, station_locs, by = "station")
  # write_loc <- paste('ri_files/',calculation_method,'.csv', sep = "")
  # write.csv(ri, write_loc, row.names=FALSE)
  # print(paste("File written too:", write_loc))
  return(ri)
}



# ri_plot()
# ---------
# This function uses plotly to place the caluclated RI on a map.
#
# @var df - a data frame
# @var type - string
# @ var title - string
ri_plot <- function(df, type, title="Residence Index", colNames=list()) {
  # Check if user has set column names
  if(length(colNames) == 0) {
    if(type=="OTNComp") { #Set column names for OTN data
      colNames = list(latitudeCol="latitude", longitudeCol="longitude", residency_indexCol="residency_index", stationCol="station")
    } else if (type=="GLATOS") { #Set column names for GLATOS data
      colNames = list(latitudeCol="deploy_lat", longitudeCol="deploy_long", residency_indexCol="residency_index", stationCol="glatos_array")
    } else if (type == "sample") { #Set column names for sample data
      colNames = list(latitudeCol="lat", longitudeCol="lon", residency_indexCol="ri", station="station")
    } else { # Other type
      stop(paste0("The type '",type,"' is not defined."), call.=FALSE)
    }
  }
  
  # Check that residency_index is in the dataframe
  if (!(colNames$residency_indexCol %in% names(df))){
    if(type=="OTNComp") {
      df <- residence_index(df, otnStat, "OTNComp", "timedelta") #Get residency_indexCol
    } else {
      df <- residence_index(df, stations, type, "timedelta") #Get residency_indexCol
    }
  }
  
  # Check that the specified columns appear in the dataframe
  missingCols <- setdiff(unlist(colNames), names(df))
  if (length(missingCols) > 0){
    stop(paste0("Dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }
  
  # Subset detections with only user-defined columns and change names
  # this makes code more easy to understand (esp. ddply)
  df <- df[,unlist(colNames)] #subset
  names(df) <- c("latitude","longitude","residency_index","station")
  
  lat_range <- c(df$latitude[which.min(df$latitude)], df$latitude[which.max(df$latitude)])
  lon_range <- c(df$longitude[which.min(df$longitude)], df$longitude[which.max(df$longitude)])
  m <- list(colorbar = list(title = "Residence Index"))
  g <- list(
    showland = TRUE,
    landcolor = toRGB("gray"),
    subunitcolor = toRGB("white"),
    countrycolor = toRGB("white"),
    showlakes = TRUE,
    lakecolor = toRGB("white"),
    showsubunits = TRUE,
    showcountries = TRUE,
    resolution = 50,
    projection = list(
      type = 'conic conformal',
      rotation = list(lon = -100)
    ),
    lonaxis = list(
      showgrid = TRUE,
      gridwidth = 0.5,
      range = lon_range,
      dtick = 1
    ),
    lataxis = list(
      showgrid = TRUE,
      gridwidth = 0.5,
      range = lat_range,
      dtick = 1
    )
  )
  p <- plot_geo(df, lat = ~latitude, lon = ~longitude, color = ~residency_index) %>%
    add_markers(size = ~((df$residency_index * 5) + 1),
                text = ~paste(df$station, ":",df$residency_index), hoverinfo = "text"
    ) %>%
    layout(title = title, geo = g)
  show(p)
  if(type=="sample") {
    message("The sample data works")
  }
}