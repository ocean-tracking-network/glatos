#' Classify discrete events in detection data
#'
#' Reduce detection data from an acoustic telemetry receiver into discrete 
#'   detection events, defined by movement between receivers (or receiver 
#'   groups, depending on location), or sequential detections at the same 
#'   location that are separated by a user-defined threshold period of time.
#'
#' @param detections A data frame containing detection data with at least 
#'   five columns; four columns must be named 'animal_id', 
#'   'detection_timestamp_utc', 'deploy_lat', and 'deploy_long' as described 
#'   below and a fifth column containing a location grouping variable, is 
#'   specified using \code{location_col}.
#'   
#'   The following four columns must appear in \code{detections}: 
#' \itemize{
#'   \item \code{animal_id} A character string with the name of the column 
#' 		 containing the individual animal identifier.
#'	 \item \code{detection_timestamp_utc} is a character string with the name 
#'	   of the column containing datetime stamps for the detections (MUST be of 
#'	   class 'POSIXct').
#'	 \item \code{deploy_lat} A character string with the name of the column
#'     containing latitude of the receiver in decimal degrees (NAD83).
#'	 \item \code{deploy_long} A character string with the name of the column
#'     containing longitude of the receiver in decimal degrees (NAD83).
#' }
#' 
#' @param location_col A character string indicating the column name in the
#'   detections data frame that will be used as the location grouping variable
#'   (e.g. "glatos_array", "station").
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
#' @return If \code{collapse = TRUE}, a data frame containing discrete 
#'   detection event data with the following columns:
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
#'	If \code{collapse = FALSE}, a data frame matching the input data frame 
#'	\code{detections} with the following columns added:
#'  \item{time_diff}{Lagged time difference in seconds between successive 
#'    detections of each animal_id.}
#'  \item{arrive}{Flag (0 or 1) representing the first detection in each 
#'    event.} 
#'  \item{depart}{Flag (0 or 1) representing the last detection in each 
#'    event.} 
#'  \item{event}{Integer representing the event number.}
#'
#' @author T. R. Binder, T. A. Hayden, C. M. Holbrook
#'
#' @examples
#'
#' #get path to example detection file
#' det_file <- system.file("extdata", "walleye_detections.csv",
#'                          package = "glatos")
#' det <- read_glatos_detections(det_file)
#'
#' filt0 <- detection_events(det) #no time filter
#' 
#' #7-day filter
#' filt_7d <- detection_events(det , time_sep = 604800) 
#'
#' @export

detection_events <- function(det,
                             location_col = "glatos_array",
                             time_sep = Inf,
                             condense = TRUE){
 
   # Make detections data frame a data.table object for processing speed
  detections <- data.table::as.data.table(det)
  
  
  # Check value of condense
  if(!is.logical(condense)) stop(
    "input argument 'condense' must be either TRUE or FALSE (unquoted).")
  
	# Check that the specified columns appear in the detections dataframe
	missingCols <- setdiff(c("animal_id",
	                         "detection_timestamp_utc",
	                         "deploy_lat",
	                         "deploy_long"),
	                       names(detections))
	if (length(missingCols) > 0){
		stop(paste0("Detections dataframe is missing the following ",
			"column(s):\n", paste0("       '", missingCols, "'", collapse = "\n")), 
			call. = FALSE)
	}
	
	# Check that timestamp is of class 'POSIXct'
	if(!('POSIXct' %in% class(detections$detection_timestamp_utc))){
	  stop(paste0(
    	    "Column 'detection_timestamp_utc' in the detections dataframe",
          "must be of class 'POSIXct'."),
	       call. = FALSE)
	} 
	
	# Set variables to NULL that will appear in data.table calls 
	#  to avoid R CMD check NOTES
	animal_id <- detection_timestamp_utc <- time_diff <- arrive <-
	  depart <- event <- deploy_lat <- deploy_long <- NULL
	
	# Change name of location variable column for convenience 
	data.table::setnames(detections, location_col, "location_col")
		
	# Sort detections by transmitter id and then by detection timestamp.
	data.table::setkey(detections, animal_id, detection_timestamp_utc)

	# Add a column with time between a given detection and the detection
	detections[ , time_diff :=  c(NA, diff(as.numeric(detection_timestamp_utc))), 
	           by = c("animal_id", "location_col")]
	
	# Flag as arrival if location not equal to previous or time_diff > time_sep
	detections[ , arrive := as.numeric(
	                (location_col != 
	                   data.table::shift(location_col, fill = TRUE)) | 
	                (time_diff > time_sep)), 
	           by = animal_id]

	# Flag as departure if location not equal to next or time_diff > time_sep
	detections[ , depart := as.numeric(
	                (location_col != 
	                   data.table::shift(location_col, fill = TRUE, 
	                     type = "lead")) | 
	                  (data.table::shift(time_diff, fill = TRUE, type = "lead") > 
	                      time_sep)), 
	           by = animal_id]
	
	# Add unique event number (among all fish, not within each fish)
	detections[ , event := cumsum(arrive)]  
	
	# Summarize the event data using the ddply function in the plyr package.
	Results = detections[, .(Individual = animal_id[1],
	                         location = location_col[1],
	                         mean_latitude = mean(deploy_lat, na.rm = T),
	                         mean_longitude = mean(deploy_long, na.rm = T),
	                         first_detection = detection_timestamp_utc[1],
	                         last_detection = detection_timestamp_utc[.N],
	                         num_detections = .N,
	                         res_time_sec = 
	                           diff(as.numeric(range(detection_timestamp_utc)))),
	                     by = event]
	
	# Return conditional on 'condense'
  if(condense){
	
  	# Returns dataframe containing summarized detection event data
  	message(paste0("The event filter distilled ", nrow(detections), 
  		" detections down to ", nrow(Results), " distinct detection events."))
  
  	return(as.data.frame(Results))
  } else {
   
    # Returns input dataframe with new columns
    message(paste0("The event filter identified ", 
      max(detections$event, na.rm = TRUE), " distinct events in ",
      nrow(detections), " detections."))
    
    # Rename location variable column back to original
    data.table::setnames(detections, "location_col", location_col)

    return(as.data.frame(detections))     
    
  }
}

