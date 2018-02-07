#' Classify discrete events in detection data
#'
#' Reduce detection data from an acoustic telemetry receiver into discrete 
#'   detection events, defined by movement between receivers (or receiver 
#'   groups, depending on location), or sequential detections at the same 
#'   location that are separated by a user-defined threshold period of time.
#'
#' @param detections A data frame containing detection data with at least 
#'   4 columns 'animal_id', 'detection_timestamp_utc', 'deploy_lat', 
#'   and 'deploy_long' plus a location column for grouping, which is specified
#'   using \code{location_variable}.
#'   
#'   \code{detections}: 
#' \itemize{
#'   \item \code{location_variable} is a character string with the name of the column 
#'   	 containing locations you wish to filter to (typically 'glatos_array' or 
#' 		 'station' for GLATOS data). Specified using the location_variable argument.
#'   \item \code{animal_id} is a character string with the name of the column 
#' 		 containing the individual animal identifier.
#'	 \item \code{detection_timestamp_utc} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct').
#'	 \item \code{deploy_lat} is a character string with the name of the column
#'     containing latitude of the receiver.
#'	 \item \code{deploy_long} is a character string with the name of the column
#'     containing longitude of the receiver.
#' }
#' 
#' @param location_variable A character string indicating the column name in the
#'   detections data frame that will be used as the location grouping variable
#'   (e.g. "glatos_array", "glatos_station")
#' 
#' @param time_sep Amount of time (in seconds) that must pass between 
#'   sequential detections on the same receiver (or group of receivers, 
#'   depending on specified location) before that detection is considered to 
#'   belong to a new detection event. The default value \code{Inf}, will not 
#'   define events based on elapsed time (only when location changes).
#'
#' @details mean_latitude and mean_longitude columns in the output dataframe are 
#'   the mean GPS locations for the detections comprising that detection event. 
#'   For example, if the a fish was detected at 3 receiver stations in a 
#'   glatos_array and glatos_array was selected as the location, then GPS 
#'   location for that event will be the mean of the latitude and longitude 
#'   for those three receiver stations (weighted based on the number of 
#'   detections that occurred on each station).
#'
#' @return A data frame containing discrete detection event data:
#'	\item{mean_latitude}{Mean latitude of detections comprising each event.}
#' 	\item{mean_longitude}{Mean longitude of detections comprising each event.}
#'  \item{first_detection}{The time of the first detection in a given detection 
#'    event.} 
#'  \item{last_detection}{The time of the last detection in a given detection 
#'    event.}  
#'  \item{num_detections}{The total number of detection that comprised a given
#' 	  detection event.}
#'  \item{res_time_sec}{The elapsed time in seconds between the first and last 
#'		detection in a given event.}
#'
#' @author T. R. Binder
#'
#' @examples
#' library(glatos)
#' data("walleye_detections") #example data
#' 
#' head(walleye_detections)
#' 
#' filt0 <- detectioneventFilter(walleye_detections) #no time filter
#' 
#' #7-day filter
#' filt_7d <- detectioneventFilter(walleye_detections , time_sep = 604800) 
#'
#' @export

detectionEventFilter <- function(detections,
                                 location_variable = "glatos_array",
                                 time_sep=Inf){
	
  library(data.table)
  
	# Check that the specified columns appear in the detections dataframe
	missingCols <- setdiff(c("animal_id",
	                         "detection_timestamp_utc",
	                         "deploy_lat",
	                         "deploy_long"),
	                       names(detections))
	if (length(missingCols) > 0){
		stop(paste0("Detections dataframe is missing the following ",
			"column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
			call.=FALSE)
	}
	
	# Check that timestamp is of class 'POSIXct'
	if(!('POSIXct' %in% class(detections$detection_timestamp_utc))){
	  stop(paste0("Column 'detection_timestamp_utc' in the detections dataframe must be of class 'POSIXct'."),
	       call.=FALSE)
	} 
	
	# Subset detections with only user-defined columns and change names
	# this makes code more easy to understand (esp. ddply)
	names(detections)[which(names(detections) == location_variable)] <- "location_variable"
	
	detections <- detections[, c("animal_id",
	                             "location_variable",
	                             "detection_timestamp_utc",
	                             "deploy_lat",
	                             "deploy_long")]
	
	# Make detections data frame a data.tabel object for increased processing speed
	data.table::setDT(detections)
	
	# Sort detections by transmitter id and then by detection timestamp.
	data.table::setkey(detections, animal_id, detection_timestamp_utc)


	# Add a column indicating the time between a given detection and the detection
	#  before it. The first detection in the dataset is assigned a value of NA.
	detections[,time_diff:=c(NA, diff(detections$detection_timestamp_utc))]
	
	# Insert new columns indicating whether transmitter_id or location changed 
	#  sequentially between rows in the dataframe and whether the time between 
	#  subsequent detections exceeded the user-defined threshold for a new 
	#  detection event.
	
	detections[,':='(individ_comparison = c(NA,as.numeric(detections$animal[-1]!=
	                                                       head(detections$animal,-1))),
	                 group_comparison = c(NA, as.numeric(detections$location[-1] !=
	                                                      head(detections$location,-1))),
	                 time_threshold = as.numeric(detections$time_diff > time_sep))]

	# Determine if each detection is the start of a new detection event.
	detections[,new_event := ifelse((individ_comparison + group_comparison + time_threshold) !=0 |
	                                 is.na(individ_comparison + group_comparison + time_threshold),
	                               1, 0)]

	# Assign unique number to each event (increasing by one for each event)
	detections[, event:=cumsum(new_event)]
	
	# Summarize the event data using the ddply function in the plyr package.
	Results = detections[, .(Individual = animal_id[1],
	                         location = location_variable[1],
	                         mean_latitude = mean(deploy_lat, na.rm=T),
	                         mean_longitude = mean(deploy_long, na.rm=T),
	                         first_detection = detection_timestamp_utc[1],
	                         last_detection = detection_timestamp_utc[length(detection_timestamp_utc)],
	                         num_detections = length(detection_timestamp_utc),
	                         res_time_sec = as.numeric(detection_timestamp_utc[length(detection_timestamp_utc)]) -
	                           as.numeric(detection_timestamp_utc)[1]),
	                     by = event]
	

	# Returns dataframe containing summarized detection event data
	message(paste0("The event filter distilled ", nrow(detections), 
		" detections down to ", nrow(Results), " distinct detection events."))

	return(as.data.frame(Results))
}

