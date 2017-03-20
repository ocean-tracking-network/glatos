#' Minimum lag filter
#'
#' Calculate minimum time interval between successive detections for identifying false detections
#' 
#' Calculates minimum time interval between detections for each tag on
#' a receiver ' @param detections A data frame containing detection
#' data (e.g., from the standard GLATOS detection export file
#' '*_detectionsWithLocs.csv').  Must contain the columns
#' \code{detection_timestamp}, \code{transmitter_id}, and
#' \code{receiver_sn} or equivalent (specified by
#' \code{detColNames}). \code{min_lag} is loosely based on the the
#' "short interval" method described by Pincock (2012) and replicates
#' the \code{min_lag} column in the standare glatos export file. In
#' this case (GLATOS), it is defined for each detection as the
#' shortest interval (in seconds) between either the previous or next
#' detection (whichever is closest) of the same transmitter on the
#' same receiver.
##' @param detColNames @param detColNames An optional list of
#'     character strings with names of required columns in
#'     \code{detections}:
#' \itemize{
#'    \item \code{transmitter_id} A character scalar with the name (in quotes) of the column
#'     containing transmitter_id.  The default value
#'     ('transmitter_id') is consistent with the GLATOS standard.
#'
#' \item \code{timestampCol} A character scalar with the name (in
#'     quotes) of the column containing the timestamp data. The
#'     default value ("detection_timestamp_utc") is consistent with
#'     GLATOS standard.
#' \item \code{receiver_sn} A character scalar with the name (in quotes) of the column
#'     containing receiver_id.  The default value
#'     ('receiver_id') is consistent with the GLATOS standard.
#'
#' @details A new column (\code{min_lag}) is added to the input
#'     dataframe that represents the time (in seconds) between the
#'     current detection and the next detection (either before or
#'     after) of the same transmitter on the same receiver. This
#'     function replicates the 'min_lag' column included in the
#'     standard glatos export.
#'
#' @return a column is added to input dataframe with the shortest time interval for each observation
#'
#' 
#' @author Todd Hayden
#'
#' @examples
#' data('walleye_detections') # example data
#'
#' head(walleye_detections)
#'
#' # calculate min_lag
#' dtx <- min_lag(walleye_detections)
#'
#'
#' 
min_lag <- function(detections, detColNames = list(transmitter_id = "transmitter_id", timestampCol = "detection_timestamp_utc", receiver_sn = 'receiver_sn')){

    missingCols <- setdiff(unlist(detColNames), names(detections))
    if (length(missingCols) > 0) {
        stop(paste0("Detections dataframe is missing the following ", 
                    "column(s):\n", paste0("       '", missingCols, "'", 
                                           collapse = "\n")), call. = FALSE)
    }

    if (!("POSIXct" %in% class(detections[,c(detColNames$timestampCol)]))) {
        stop(paste0("Column '", detColNames$timestampCol, "' in the detections dataframe must be of class 'POSIXct'."), 
             call. = FALSE)
    }

    detections <- detections[order(detections[,detColNames$transmitter_id], detections[,detColNames$receiver_sn], detections[,detColNames$timestampCol]),]
    
    priorTDiff <- as.numeric( detections[,detColNames$timestamp]) - as.numeric(c(NA, head(detections[,detColNames$timestampCol], -1)))

    futureTDiff <- as.numeric(c(tail(detections[,detColNames$timestamp], -1), NA)) - as.numeric(detections[,detColNames$timestamp])
    
    futureRec <- as.numeric(c(tail(detections[,detColNames$receiver_sn], -1), NA) != detections[,detColNames$receiver_sn])
    priorRec <- as.numeric(c(NA, head(detections[,detColNames$receiver_sn], -1)) != detections[,detColNames$receiver_sn])

    futureTran <- as.numeric(c(tail(detections[,detColNames$transmitter_id], -1), NA) != detections[,detColNames$transmitter_id])
    priorTran <- as.numeric(c(NA, head(detections[,detColNames$transmitter_id], -1)) != detections[,detColNames$transmitter_id])

    futureTDiff[futureRec == 1] <- NA
    priorTDiff[priorRec ==1] <- NA

    futureTDiff[futureTran ==1] <- NA
    priorTDiff[priorTran ==1] <- NA

    detections$min_lag <- pmin(priorTDiff, futureTDiff, na.rm = TRUE)
    return(detections)
}


