#' Classify discrete events in detection data
#'
#' Reduce detection data from an acoustic telemetry receiver into discrete 
#'   detection events, defined by movement between receivers (or receiver 
#'   groups, depending on location), or sequential detections at the same 
#'   location that are separated by a user-defined threshold period of time.
#'
#' @param detections A data frame containing detection data with at least 
#'   5 columns containing 'location', 'animal', 'timestamp', 'latitude', 
#'   and 'longitude'. Column names are specified with \code{detColNames} by
#'   \code{type}.
#'   
#' @param type A character string that contains the type of data that is being 
#'   passed in, for example, "OTN", "GLATOS", or "sample".
#' 
#' @param detColNames An optional list that contains the user-defined column
#'   names
#' 
#' @details detColNames is defined as a list with names of required columns in 
#'   \code{detections}, defined by \code{type}: 
#' \itemize{
#'   \item \code{locationCol} is a character string with the name of the column 
#'   	 containing locations you wish to filter to ('glatos_array' for GLATOS data, 
#' 		 'station' for OTN data, or 'location' for sample data).
#'   \item \code{animalCol} is a character string with the name of the column 
#' 		 containing the individual animal identifier ('animal_id' for GLATOS data,
#' 		 'catalognumber' for OTN data, or 'animal' for sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' for
#'     OTN data, or 'time' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 'latitude'
#'     for OTN data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 'longitude'
#'     for OTN data, or 'longitude' for sample data).
#' }
#' 
#' @param timeSep Amount of time (in seconds) that must pass between 
#'   sequential detections on the same receiver (or group of receivers, 
#'   depending on specified location) before that detection is considered to 
#'   belong to a new detection event. The default value \code{Inf}, will not 
#'   define events based on elapsed time (only when location changes).
#'
#' @details MeanLatitude and MeanLongitude columns in the output dataframe are 
#'   the mean GPS locations for the detections comprising that detection event. 
#'   For example, if the a fish was detected at 3 receiver stations in a 
#'   glatos_array and glatos_array was selected as the location, then GPS 
#'   location for that event will be the mean of the latitude and longitude 
#'   for those three receiver stations (weighted based on the number of 
#'   detections that occurred on each station).
#'
#' @return A data frame containing discrete detection event data:
#'	\item{MeanLatitude}{Mean latitude of detections comprising each event.}
#' 	\item{MeanLongitude}{Mean longitude of detections comprising each event.}
#'  \item{FirstDetection}{The time of the first detection in a given detection 
#'    event.} 
#'  \item{LastDetection}{The time of the last detection in a given detection 
#'    event.}  
#'  \item{NumDetections}{The total number of detection that comprised a given
#' 	  detection event.}
#'  \item{ResTime_sec}{The elapsed time in seconds between the first and last 
#'		detection in a given event.}
#'
#' @author T. R. Binder, edited by A. Dini
#'
#' @usage To use:
#'   For GLATOS data, detectionEventFilter(data, "GLATOS")
#'   For OTN data, detectionEventFilter(data, "OTN")
#'   For sample data, detectionEventFilter(data, "sample")
#' @export

detectionEventFilter <- function(detections, type, timeSep = Inf, detColNames=list()) {
  #Check if user has defined detColNames
  if(length(detColNames)==0) {
    if(type == "GLATOS") { #Set column names for GLATOS data
      detColNames = list(locationCol="glatos_array",animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
    } else if (type == "OTN") { #Set column names for OTN data
      detColNames = list(locationCol="station",animalCol="catalognumber", timestampCol="datecollected",latCol="latitude", longCol="longitude")
    } else if (type == "sample") { #Set column names for sample data
      detColNames = list(locationCol="location", animalCol="animal", timestampCol="time", latCol="latitude", longCol="longitude")
    } else { # Other type
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
  # Use dplyr's lag method to do this
  # Original: detections$TimeDiff <- c(NA, diff(detections$timestamp))
  n <- as.numeric(detections$timestamp)
  detections$TimeDiff <- n-dplyr::lag(n)
  
  # Insert new columns indicating whether transmitter_id or location changed 
  #  sequentially between rows in the dataframe and whether the time between 
  #  subsequent detections exceeded the user-defined threshold for a new 
  #  detection event.
  
  # Flag if animal changed
  # Use dplyr's lag method to do this
  # Original: detections$IndividComparison <- c(NA, as.numeric(detections$animal[-1] != head(detections$animal,-1)))
  detections$IndividComparison <- (detections$animal != dplyr::lag(detections$animal))
  
  # Flag if location changed
  # Use dplyr's lag method to do this
  # Original: detections$GroupComparison <- c(NA, as.numeric(detections$location[-1] != head(detections$location,-1)))
  #n <- as.numeric(detections)
  detections$GroupComparison <- (detections$location != dplyr::lag(detections$location))
  
  # Flag if interval exceeded time threshold
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
                         Location = as.character(location[1]), 
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

