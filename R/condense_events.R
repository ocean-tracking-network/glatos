#' Classify detection data into events
#'
#' Reduce detection data from an acoustic telemetry receiver into discrete 
#'   detection events, defined by movement between receivers (or receiver 
#'   groups, depending on location), or sequential detections at the same 
#'   location that are separated by a user-defined threshold period of time.
#'
#' @param detections A data frame containing detection data with at least 
#'   3 columns containing 'location', 'animal', and 'timestamp' columns. Column names are specified with \code{detColNames}.
#'   
#' @param detColNames A list with names of required columns in 
#'   \code{detections}: 
#' \itemize{
#'   \item \code{locationCol} is a character string with the name of the column 
#'   	 containing locations you wish to filter to (typically 'glatos_array' or 
#' 		 'station' for GLATOS data).
#'   \item \code{animalCol} is a character string with the name of the column 
#' 		 containing the individual animal identifier.
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct').
#' }
#' 
#' @param timeSep Amount of time (in seconds) that must pass between 
#'   sequential detections on the same receiver (or group of receivers, 
#'   depending on specified location) before that detection is considered to 
#'   belong to a new detection event. The default value \code{Inf}, will not 
#'   define events based on elapsed time (only when location changes).
#'
#' @return Input dataframe with columns containing flags:
#'  \item{arrival}{Flag (0 or 1) representing the first detection in each event.} 
#'  \item{depart}{Flag (0 or 1) representing the last detection in each event.} 
#'  \item{event}{integer representing the event number}
#'  \item{TimeDiff}{Lagged time difference in seconds between successive detections.}
#'  \item{newfsh}{Flag representing first detection of each fish}
#' 
#' @author T. R. Binder, Todd Hayden
#' @export
#' @examples
#' data("walleye_detections")
#' dtx <- condense_events(walleye_detections, timeSep = 3600)


condense_events <- function(detections, detColNames = list(locationCol="glatos_array", animalCol="animal_id", timestampCol="detection_timestamp_utc"), timeSep=Inf){

## Check that the specified columns appear in the detections dataframe
    missingCols <- setdiff(unlist(detColNames), names(detections))
    if (length(missingCols) > 0){
        stop(paste0("Detections dataframe is missing the following ",
                    "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
             call.=FALSE)
    }

## Check that timestamp is of class 'POSIXct'
    if(!('POSIXct' %in% class(detections[,detColNames$timestampCol]))){
        stop(paste0("Column '", detColNames$timestampCol,
                    "' in the detections dataframe must be of class 'POSIXct'."),
             call.=FALSE)
    } 

## Sort detections by transmitter id and then by detection timestamp.
    detections <- detections[order(detections[,detColNames$animalCol], detections[,detColNames$timestampCol]),]

    detections$TimeDiff <- c(NA, diff(detections[,detColNames$timestampCol]))	

## new fish.
    detections$newfsh <- as.numeric(c(1, head(detections[, detColNames$animalCol], -1)) != detections[,detColNames$animalCol])
    detections$TimeDiff[detections$newfsh == 1] <- NA

    ## arrival detections
    detections$arrival <- as.numeric(c(1, detections[,detColNames$locationCol][2:nrow(detections)] != detections[,detColNames$locationCol][1:(nrow(detections) - 1)]) | detections$TimeDiff > timeSep | detections$newfsh == 1)

    ## depart detections
    detections$depart = 0
    detections$depart[((which(detections$arrival == 1)) - 1)] <- 1
    detections$depart[(nrow(detections))] <- 1

    ## assign events
    detections$event <- cumsum(detections$arrival)    
    return(detections)
}





