#' Minimum lag filter
#'
#' Calculate minimum time interval between successive detections for identifying
#' false detections
#' 
#' Calculates minimum time interval between detections for each tag on
#' a receiver
#'
#' @param dtc An object of class \code{glatos_detections} or data
#'   frame containing acoustic detection data with
#'   \code{detection_timestamp}, \code{transmitter_id}, and
#'   \code{receiver_sn}.  \code{min_lag} is loosely based on the the
#'   "short interval" method described by Pincock (2012) and
#'   replicates the \code{min_lag} column in the standare glatos
#'   export file. In this case (GLATOS), \code{min_lat} is defined for each
#'   detection as the shortest interval (in seconds) between either
#'   the previous or next detection (whichever is closest) of the same
#'   transmitter on the same receiver.
#' 
#' @details A new column (\code{min_lag}) is added to the input
#'     dataframe that represents the time (in seconds) between the
#'     current detection and the next detection (either before or
#'     after) of the same transmitter on the same receiver. This
#'     function replicates the 'min_lag' column included in the
#'     standard glatos export.
#'
#' @return a column is added to input dataframe with the shortest time interval
#' for each observation
#'
#' 
#' @author Todd Hayden
#' @export
#' 
#' @examples
#'
#' # load detection file
#'
#' det_file <- system.file("extdata", "walleye_detections.csv",
#'                          package = "glatos")
#' dtc <- read_glatos_detections(det_file)
#'
#' # calculate min_lag
#' min_lag(dtc)
#'
#'
#' 

min_lag <- function(dtc){
  setDT(dtc)
  setkey(dtc, transmitter_id, receiver_sn, detection_timestamp_utc)
  priorTDiff <- as.numeric( dtc[,detection_timestamp_utc]) - as.numeric(
    c(NA, head(dtc[,detection_timestamp_utc], -1)))
  futureTDiff <- as.numeric(c(tail(dtc[,detection_timestamp_utc], -1), NA)) -
    as.numeric(dtc[,dtc$detection_timestamp_utc])
  futureRec <- as.numeric(c(tail(dtc[,receiver_sn], -1), NA) != dtc[,receiver_sn])
  priorRec <- as.numeric(c(NA, head(dtc[,receiver_sn], -1)) != dtc[,receiver_sn])
  futureTran <- as.numeric(c(tail(dtc[,transmitter_id], -1), NA) !=
                             dtc[,transmitter_id])
  priorTran <- as.numeric(c(NA, head(dtc[, transmitter_id], -1)) !=
                            dtc[,transmitter_id])

  futureTDiff[futureRec == 1] <- NA
  priorTDiff[priorRec ==1] <- NA
  futureTDiff[futureTran ==1] <- NA
  priorTDiff[priorTran ==1] <- NA
  dtc$min_lag <- pmin(priorTDiff, futureTDiff, na.rm = TRUE)
  setkey(dtc, animal_id, detection_timestamp_utc)
  return(as.data.frame(dtc))
}
