#' Classify detection data into events
#'
#' Reduce detection data from an acoustic telemetry receiver into discrete 
#'   detection events, defined by movement between receivers (or receiver 
#'   groups, depending on location), or sequential detections at the same 
#'   location that are separated by a user-defined threshold period of time.
#'
#' @param dtc An object of class \code{glatos_detections} of data
#'   frame containing spatiotemporal data with a location column
#'   (typically 'glatos_array' or 'station'), 'animal_id' and
#'   'detection_timestamp_utc' columns.
#'
#' @param loc_col Name of column in 'dtc' that specifies location column
#'   ' 
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
#'
#' # load detection data
#' det_file <- system.file("extdata", "walleye_detections.zip", package = "glatos")
#' det_file <- unzip(det_file, "walleye_detections.csv")
#' dtc <- read_glatos_detections(det_file)
#'
#' dtx <- condense_events(dtc, timeSep = 3600)

condense_events <- function(dtc, loc_col = "glatos_array", timeSep=Inf){
  # Sort detections by transmitter id and then by detection timestamp.
  setDT(dtc)
  setkey(dtc, animal_id, detection_timestamp_utc)
  dtc$TimeDiff <- c(NA, diff(dtc[, detection_timestamp_utc]))

  # new fish.
  dtc$newfsh <- as.numeric(c(1, head(dtc[, animal_id], -1)) != dtc[, animal_id])
  dtc$TimeDiff[dtc$newfsh == 1] <- NA

  # arrival detections
  dtc$arrival <- as.numeric(c(1, dtc[, loc_col, with = FALSE]
                              [2:nrow(dtc)] != dtc[, loc_col, with = FALSE]
                              [1:(nrow(dtc) - 1)]) | dtc$TimeDiff >
                              timeSep | dtc$newfsh == 1)
  # depart detections
  dtc$depart = 0
  dtc$depart[((which(dtc$arrival == 1)) - 1)] <- 1
  dtc$depart[(nrow(dtc))] <- 1

  # assign events
  dtc$event <- cumsum(dtc$arrival)    
  return(as.data.frame(dtc))
  }
