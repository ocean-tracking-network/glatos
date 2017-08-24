#' Perform velocity test (Steckenreuter et al., 2016), which checks if the minimum velocity is above a threshold
#'   and returns a validity column using the geosphere and dplyr libraries
#'
#' @param detections A data frame containing detection data with at least 
#'   5 columns containing 'timestamp', 'transmitters', 'receivers', 'longitude', and 'latitude'. 
#'   Column names are specified by \code{type}.
#'   
#' @param type A character string that contains the type of data that is being passed in,
#'   for example, "OTNDet", "OTNQual", "GLATOS", or "sample".
#' 
#' @param detColNames An optional list that allows the user to set their own column
#'   names
#'   
#' @details detColNames is defined as a list with the names of the required columns in
#' \code{detections}:
#'  \itemize{
#'    \item \code{timestampCol} is a character string with the name of the column
#'    containing datetime stamps for the detections (MUST be of class 'POSIXct')
#'    ('detection_timestamp_utc' for GLATOS data, 'datecollected' for OTN detection 
#'                                                 data, 'datecollected' for OTN
#'                                                 qualified data, or 'time' for 
#'                                                 sample data).
#'    \item \code{transmittersCol} is a character string with the name of the column
#'     containing the ids of the transmitters
#'     ('transmission_id' for GLATOS data, 'tagname' for OTN detection data, 
#'                                                 'fieldnumber' for OTN qualified
#'                                                 data, or 
#'                                                 'transmitter' for sample data).
#'     \item \code{receiversCol} is a character string with the name of the column
#'     containing the ids of the receivers
#'     ('receiver_sn' for GLATOS data, 'receiver_group' for OTN detection data, 
#'                                                 'rcvrcatnumber' for OTN qualified 
#'                                                 data, or 'receiver' for sample data).
#'     \item \code{longitudeCol} is a character string with the name of the column
#'     containing the longitude coordinate for the detections
#'     ('deploy_long' for GLATOS data, 'longitude' for OTN detection data, 'longitude' for
#'                                                 OTN qualified data, or 'longitude' for 
#'                                                 sample data).
#'     \item \code{latitudeCol} is a character string with the name of the column
#'     containing the latitude coordinate for the detections
#'     ('deploy_lat' for GLATOS data, 'latitude' for OTN detection data, 'latitude for OTN
#'                                                 qualified data, or 'latitude' for sample 
#'                                                 data).
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
#'   For GLATOS data, efficiencyTest(data, "GLATOS")
#'   For OTN detection data, efficiencyTest(data, "OTNDet")
#'   For OTN qualified data, efficiencyTest(data, "OTNQual")
#'   For sample data, efficiencyTest(data, "sample", minVelValue = 20, minDistValue = 1300, shortIntSec = 3*60, longIntSec = 5*60)
#'
#' @export

# Similar wording in method headers to detectionEventFilter from GLATOS
efficiencyTest <- function(detections, type, detColNames=list(), minVelValue=-100, minDistValue=-100, shortIntSec=2*60*60, longIntSec=24*60*60) {
  
  # print("detections initially: ")
  # print(detections)
  
  # Perform number of detections and interval test
  detections <- numIntervalTest(detections, type = type, detColNames = detColNames, shortIntSec = shortIntSec, longIntSec = longIntSec)
  
  # print("detections after numIntervalTest:")
  # print(detections)
  
  # Perform distance test
  detections <- distTest(detections, type = type, detColNames = detColNames, minDistValue = minDistValue)
  
  # print("detections after distTest:")
  # print(detections)
  
  # Perform velocity test
  detections <- velTest(detections, type = type, detColNames = detColNames, minVelValue = minVelValue)
  
  # print("detections after velTest:")
  # print(detections)
  
  # Calculate valid column for efficiency test using first method (described below)
  #   and append to \code{detections}
  # 1 if all columns are valid (all 1), 2 if questionable validity (contains a 2), 3 if missing information
  detections$effValid1 <- apply(detections, 1, function(x) {
    if(x["numIntervalValid"] == 3 || x["velValid"] == 3 || x["distValid"] == 3) {
      3 #Missing information
    } else if(x["numIntervalValid"] == 1 && x["velValid"] == 1 && x["distValid"] == 1) {
      1 #Valid
    } else {
      2 #Invalid
    }
  })
  
  # Calculate valid column for efficiency test using second method (described below)
  #   and append to \code{detections}
  # 1 if valid (valid for number detections and interval test OR valid for both velocity and distance test)
  # 2 if questionable, 3 if missing information
  detections$effValid2 <- apply(detections, 1, function(x) {
    if(x["numIntervalValid"] == 3 || (x["velValid"] == 3 || x["distValid"] == 3)) {
      3 #Missing information
    } else if(x["numIntervalValid"] == 1 || (x["velValid"] == 1 && x["distValid"] == 1)) {
      1 #Valid
    } else {
      2 #Invalid
    }
  })
  
  #Count number of rows of each validity score
  num11 <<- 0
  num12 <<- 0
  num13 <<- 0
  num21 <<- 0
  num22 <<- 0
  num23 <<- 0
  val1 <- detections$effValid1
  val2 <- detections$effValid2
  l1 <- sapply(val1, function(x) {
    if(x == 1) { #row with validity score of 1 (valid)
      num11 <<- num11+1
    } else if (x==2) { #row with validity score of 2 (questionable)
      num12 <<- num12+1
    } else { #row with validity score of 3 (missing information)
      num13 <<- num13+1
    }
  })
  l2 <- sapply(val2, function(x) {
    if(x == 1) { #row with validity score of 1 (valid)
      num21 <<- num21+1
    } else if (x==2) { #row with validity score of 2 (questionable)
      num22 <<- num22+1
    } else { #row with validity score of 3 (missing information)
      num23 <<- num23+1
    }
  })
  
  nr <- nrow(detections)
  
  #Print results
  message(paste0("The filter identified ", num11," (", round((num11)/nr*100, 2), "%) of ", nr, " detections as valid using the first method of the efficiency test."))
  message(paste0("The filter identified ", num12," (", round((num12)/nr*100, 2), "%) of ", nr, " detections as questionable using the first method of the efficiency test."))
  message(paste0("The filter identified ", num13," (", round((num13)/nr*100, 2), "%) of ", nr, " detections as having missing information using the the first method of the efficiency test."))
  message(paste0("The filter identified ", num21," (", round((num21)/nr*100, 2), "%) of ", nr, " detections as valid using the second method of the efficiency test."))
  message(paste0("The filter identified ", num22," (", round((num22)/nr*100, 2), "%) of ", nr, " detections as questionable using the second method of the efficiency test."))
  message(paste0("The filter identified ", num23," (", round((num23)/nr*100, 2), "%) of ", nr, " detections as having missing information using the second method of the efficiency test."))
  
  return(detections)
}