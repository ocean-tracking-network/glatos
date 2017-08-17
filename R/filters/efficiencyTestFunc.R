#' Perform velocity test (Steckenreuter et al., 2016), which checks if the minimum velocity is above a threshold
#'   and returns a validity column using the geosphere and dplyr libraries
#'
#' @param detections A data frame containing detection data with at least 
#'   5 columns containing 'timestamp', 'transmitters', 'receivers', 'longitude', and 'latitude'. 
#'   Column names are specified by \code{type}.
#'   
#' @param type A character string that contains the type of data that is being passed in,
#'   for example, "OTN", "GLATOS", or "sample".
#' 
#' @param detColNames An optional list that allows the user to set their own column
#'   names
#'   
#' @details detColNames is defined as a list with the names of the required columns in
#' \code{detections}:
#'  \itemize{
#'    \item \code{timestampCol} is a character string with the name of the column
#'    containing datetime stamps for the detections (MUST be of class 'POSIXct')
#'    ('detection_timestamp_utc' for GLATOS data, 'datecollected' for OTN data, or
#'                                                 'time' for sample data).
#'    \item \code{transmittersCol} is a character string with the name of the column
#'     containing the ids of the transmitters
#'     ('transmission_id' for GLATOS data, 'collectornumber' for OTN data, or 
#'                                                 'transmitter' for sample data).
#'     \item \code{receiversCol} is a character string with the name of the column
#'     containing the ids of the receivers
#'     ('receiver_sn' for GLATOS data, 'rcvrcatnumber' for OTN data, or 'receiver'
#'                                                 for sample data).
#'     \item \code{longitudeCol} is a character string with the name of the column
#'     containing the longitude coordinate for the detections
#'     ('deploy_long' for GLATOS data, 'longitude' for OTN data, or 'longitude' for 
#'                                                 sample data).
#'     \item \code{latitudeCol} is a character string with the name of the column
#'     containing the latitude coordinate for the detections
#'     ('deploy_lat' for GLATOS data, 'latitude' for OTN data, or 'latitude' for
#'                                                 sample data).
#'   }
#'
#' @details Each value in the min_dist column indicates the minimum of the distance between the 
#'   current instance and instance before, and the distance between the current instance and the
#'   instance after
#' @details Each value in the min_time column indicates the minimum of the time between the
#'   current instance and instance before, and the time between the current instance and the
#'   instance after
#' @details Each value in the min_vel column indicates the value in the min_dist column divided by
#'   the value in the min_time column
#' @details Each value in the velValid column indicates whether the value in the min_vel column
#'   is greater than the threshold 'minVelValue'
#' 
#' @return A data frame containing the data with the four columns appended to it:
#'	\item{min_dist}{Minimum of the distance between the current instance and instance 
#'	  before, and the distance between the current instance and the instance after.}
#' 	\item{min_time}{Minimum of the time between the current instance and instance 
#'	  before, and the time between the current instance and the instance after.}
#' 	\item{min_vel}{The min_dist value divided by the min_time value.} 
#'  \item{velValid}{A value to check if the min_vel value is greater than or equal to (1), or less than (0)
#'    the threshold.}
#'  \item{}
#'
#' @references (in APA) Steckenreuter, A., Hoenner, X., Huveneers, C., Simpfendorfer, C., Buscot, M.J., 
#'   Tattersall, K., ... Harcourt, R. (2016). Optimising the design of large-scale acoustic telemetry
#'   curtains. Marine and Freshwater Research. doi: 10.1071/mf/16126
#' 
#' @author A. Dini
#'
#' @usage To use:
#'   For GLATOS data, velTest(data, "GLATOS")
#'   For OTN data, velTest(data, "OTN")
#'   For sample data, velTest(data, "sample")
#'
#' @export

# Similar wording in method headers to detectionEventFilter from GLATOS
efficiencyTest <- function(detections, type, detColNames=list(), minVelValue=-100, minDistValue=-100, shortIntSec=2*60*60, longIntSec=24*60*60) {
  
  # print("detections initially: ")
  # print(detections)
  
  # Perform number of detections and interval test
  detections <- numIntervalTest(detections, type, detColNames, shortIntSec = shortIntSec, longIntSec = longIntSec)
  
  # print("detections after numIntervalTest:")
  # print(detections)
  
  # Perform distance test
  detections <- distTest(detections, type=type, detColNames = detColNames, minDistValue = minDistValue)
  
  # print("detections after distTest:")
  # print(detections)
  
  # Perform velocity test
  detections <- velTest(detections, type, detColNames = detColNames, minVelValue = minVelValue)
  
  # print("detections after velTest:")
  # print(detections)
  
  # Calculate valid column for efficiency test using first method (described below)
  #   and append to \code{detections}
  # 1 if all columns are valid (all 1), 2 if questionable validity (contains a 0), 3 if missing information
  detections$effValid1 <- apply(detections, 1, function(x) {
    if(x["numIntervalValid"] == 1 && x["velValid"] == 1 && x["distValid"] == 1) {
      1 #valid
    } else if(x["numIntervalValid"] == 0 || x["velValid"] == 0 || x["distValid"] == 0) {
      2 #questionable
    } else {
      3 #missing information
    }
  })
  
  # Calculate valid column for efficiency test using second method (described below)
  #   and append to \code{detections}
  # 1 if valid (valid for number detections and interval test OR valid for both velocity and distance test)
  # 2 if questionable, 3 if missing information
  detections$effValid2 <- apply(detections, 1, function(x) {
    if(x["numIntervalValid"] == 1 || (x["velValid"] == 1 && x["distValid"] == 1)) {
      1 #valid
    } else if (x["velValid"] == 0 || x["distValid"] == 0) {
      2 #questionable
    } else {
      3 #missing information
    }
  })
  
  #Print out results
  numVal1 <<- 0
  numVal2 <<- 0
  val1 <- detections$effValid1
  val2 <- detections$effValid2
  l1 <- sapply(val1, function(x) {
    if(x == 1) {
      numVal1 <<- numVal1+1
    }
  })
  l2 <- sapply(val2, function(x) {
    if(x == 1) {
      numVal2 <<- numVal2+1
    }
  })
  nr <- nrow(detections)
  message(paste0("The filter identified ", nr-numVal1," (", round((nr - numVal1)/nr*100, 2), "%) of ", nr, " detections as invalid using the first method of the efficiency test."))
  message(paste0("The filter identified ", nr-numVal2," (", round((nr - numVal2)/nr*100, 2), "%) of ", nr, " detections as invalid using the second method of the efficiency test."))
  return(detections)
}