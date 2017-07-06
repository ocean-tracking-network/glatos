detectionEventFilter <- function(detections, type, timeSep = Inf) {
  if(type == "GLATOS") {
    detColNames = list(locationCol="glatos_array",animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
  } else if (type == "OTN") {
    detColNames = list(locationCol="station",animalCol="collectioncode", timestampCol="datecollected",latCol="latitude", longCol="longitude")
  } else {
    detColNames = {}
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
  names(detections) <- c("location","animal","timestamp","lat","lon")
  
  
  # Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(detections$timestamp))){
    stop(paste0("Column '",detColNames$timestampCol,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  } 
  
  # Sort detections by transmitter id and then by detection timestamp.
  detections <- detections[order(detections$animal, 
                                 detections$timestamp),]
  
  # Add a column indicating the time between a given detection and the detection
  #  before it. The first detection in the dataset is assigned a value of NA.
  #Use dplyr's lag method to do this
  #detections$TimeDiff <- c(NA, diff(detections$timestamp))
  n <- as.numeric(detections$timestamp)
  detections$TimeDiff <- n-dplyr::lag(n)
  
  # Insert new columns indicating whether transmitter_id or location changed 
  #  sequentially between rows in the dataframe and whether the time between 
  #  subsequent detections exceeded the user-defined threshold for a new 
  #  detection event.
  
  # flag if animal changed
  # Use dplyr's lag method to do this
  #detections$IndividComparison <- c(NA, as.numeric(detections$animal[-1] != head(detections$animal,-1)))
  detections$IndividComparison <- (as.numeric(detections$animal[-1]) != dplyr::lag(as.numeric(detections$animal[-1])))
  
  # flag if location changed
  # Use dplyr's lag method to do this
  detections$GroupComparison <- c(NA, 
                                  as.numeric(detections$location[-1] != head(detections$location,-1)))
  
  # flag if interval exceeded time threshold
  detections$TimeThreshold <- as.numeric(detections$TimeDiff > timeSep)
  
  # Determine if each detection is the start of a new detection event.
  detections$NewEvent <- with(detections, ifelse((IndividComparison + 
                                                    GroupComparison + TimeThreshold) != 0 | is.na(IndividComparison + 
                                                                                                    GroupComparison + TimeThreshold), 1, 0))
  
  # Assign unique number to each event (increasing by one for each event)
  detections$Event <- cumsum(detections$NewEvent)
  
  # Summarize the event data using the ddply function in the plyr package.
  Results <- plyr::ddply(detections, plyr::.(Event), plyr::summarise,
                         Individual = animal[1], 
                         Location = location[1], 
                         MeanLatiude = mean(lat, na.rm=T), 
                         MeanLongitude = mean(lon, na.rm=T), 
                         FirstDetection = timestamp[1], 
                         LastDetection = timestamp[length(timestamp)], 
                         NumDetections = length(Event), 
                         ResTime_sec = as.numeric(timestamp[length(timestamp)]) - 
                           as.numeric(timestamp)[1])
  
  # Returns dataframe containing summarized detection event data
  message(paste0("The event filter distilled ", nrow(detections), 
                 " detections down to ", nrow(Results), " distinct detection events."))
  
  return(Results)
}

