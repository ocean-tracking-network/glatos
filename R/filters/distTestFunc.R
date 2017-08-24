#' Perform distance test (Steckenreuter et al., 2016), which checks if the minimum distance is above a threshold
#'   and returns a validity column using the geosphere and dplyr libraries
#'
#' @param detections A data frame containing detection data with at least 
#'   5 columns containing 'timestamp', 'transmitters', 'receivers', 'longitude', and 'latitude'. 
#'   Column names are specified by \code{type}.
#'   
#' @param type A character string that contains the type of data that is being passed in,
#'   for example, "OTNDet", "OTNQual", "GLATOS", or "sample".
#' 
#' @param detColNames An optional list that contains the user-defined column names
#' 
#' @param minVelValue An optional integer that contains the minimum velocity
#'   
#' @details detColNames is defined as a list with the names of the required columns in
#' \code{detections}:
#'  \itemize{
#'    \item \code{timestampCol} is a character string with the name of the column
#'    containing datetime stamps for the detections (MUST be of class 'POSIXct')
#'    ('detection_timestamp_utc' for GLATOS data, 'datecollected' for OTN detection data, 
#'                                                 'datecollected' for OTN qualified data,
#'                                                 or 'time' for sample data).
#'    \item \code{transmittersCol} is a character string with the name of the column
#'     containing the ids of the transmitters
#'     ('transmission_id' for GLATOS data, 'tagname' for OTN detection data, 'fieldnumber' 
#'                                                 for OTN qualified data, or 'transmitter'
#'                                                 for sample data).
#'     \item \code{receiversCol} is a character string with the name of the column
#'     containing the ids of the receivers
#'     ('receiver_sn' for GLATOS data, 'receiver_group' for OTN detection data, 
#'                                                 'rcvrcatnumber' for OTN qualified
#'                                                 data, or 'receiver'for sample data).
#'     \item \code{longitudeCol} is a character string with the name of the column
#'     containing the longitude coordinate for the detections
#'     ('deploy_long' for GLATOS data, 'longitude' for OTN detection data, 'longitude'
#'                                                 for OTN qualified data, or 'longitude' 
#'                                                 for sample data).
#'     \item \code{latitudeCol} is a character string with the name of the column
#'     containing the latitude coordinate for the detections
#'     ('deploy_lat' for GLATOS data, 'latitude' for OTN detection data, 'latitude' for
#'                                                 OTN qualified data, or 'latitude' for
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
#'   is less than the threshold 'minDistValue'
#' 
#' @return A data frame containing the data with the two columns appended to it:
#'	\item{min_dist}{Minimum of the distance between the current instance and instance 
#'	  before, and the distance between the current instance and the instance after.}
#'  \item{distValid}{A value to check if the min_dist value is greater than or equal to (1), or less than (0)
#'    the threshold.}
#'
#' @references (in APA) Steckenreuter, A., Hoenner, X., Huveneers, C., Simpfendorfer, C., Buscot, M.J., 
#'   Tattersall, K., ... Harcourt, R. (2016). Optimising the design of large-scale acoustic telemetry
#'   curtains. Marine and Freshwater Research. doi: 10.1071/mf/16126
#' 
#' @author A. Dini
#'
#' @usage To use:
#'   For GLATOS data, distTest(data, "GLATOS")
#'   For OTN detection data, distTest(data, "OTNDet")
#'   For OTN qualified data, distTest(data, "OTNQual")
#'   For sample data, distTest(data, "sample")
#'
#' @export

distTest <- function(detections, type, detColNames=list(), minDistValue=-100) {
  #Different column names from different types of data
  #Set different minimum velocity values to test against
  # Check if user has set column names
  if(length(detColNames) == 0) {
    if(type == "sample") { #Set column names for sample data
      detColNames <- list(timestampCol = "time", transmittersCol = "transmitter", receiversCol = "receiver", longCol = "longitude", latCol = "latitude")
    } else if (type == "GLATOS") { #Set column names for GLATOS data
      detColNames <- list(timestampCol = "detection_timestamp_utc", transmittersCol = "transmitter_id", receiversCol = "receiver_sn", longCol = "deploy_long", latCol = "deploy_lat")
    } else if (type == "OTNDet") { #Set column names for OTN detection data
      detColNames <- list(timestampCol = "datecollected", transmittersCol = "tagname", receiversCol = "receiver_group", longCol = "longitude", latCol = "latitude")
    } else if (type == "OTNQual") { #Set column names for OTN qualified data
      detColNames <- list(timestampCol = "datecollected", transmittersCol = "fieldnumber", receiversCol = "rcvrcatnumber", longCol = "longitude", latCol = "latitude")
    } else { #Other type
      stop(paste0("The type '", type, "' is not defined."), call.=FALSE)
    }
  }
  # Check if user has defined minimum distance threshold
  if(minDistValue == -100) {
    if(type == "sample") { #Set minimum distance for sample data
      minDistValue <- 1100
    } else if (type == "GLATOS") { #Set minimum distance for GLATOS data
      minDistValue <- 100
    } else if (type == "OTNDet" || type=="OTNQual") { #Set minimum distance for OTN data
      minDistValue <- 100
    } else { #Other type
      stop(paste0("The type '", type, "' is not defined."), call.=FALSE)
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
  # this makes code easier to understand (especially ddply)
  data3 <- detections[,unlist(detColNames)] #subset
  names(data3) <- c("timestamp", "transmitters","receivers","long", "lat")
  # data2$num <- as.numeric(data2$timestamp)
  
  #Save row names to keep original ordering later
  data3$rn <- row.names(data3)
  
  
  # Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(data3$timestamp))){
    stop(paste0("Column '",detColNames$timestampCol,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  #Get data points with missing information
  #Subset detections that have NA (missing values, should have validity of 3)
  liNA <- subset(data3, is.na(data3$timestamp) | is.na(data3$transmitters) | is.na(data3$receivers) | is.na(data3$long) | is.na(data3$lat))
  #Add columns
  liNA$min_dist <- rep(NA, nrow(liNA))
  liNA$distValid <- rep(3, nrow(liNA))
  #Subset detections without any NA (no missing values)
  data2 <- subset(data3, !is.na(data3$timestamp) & !is.na(data3$transmitters) & !is.na(data3$receivers) & !is.na(data3$long) & !is.na(data3$lat))
  
  #Set of points, which are made up of: (longitude, latitude)
  points <- data.frame(long=data2$long, lat=data2$lat)
  
  #Gets list of points before the current and after the current point (lag and lead, respectively)
  pointsBefore <- data.frame(long=dplyr::lag(data2$long), lat=dplyr::lag(data2$lat))
  pointsAfter <- data.frame(long=dplyr::lead(data2$long), lat=dplyr::lead(data2$lat))
  
  #Calculates distance between each set of points by calculating distance between previous and current point, and distance between current and next point and labelling each list distB and distA, respectively
  distB <- geosphere::distHaversine(pointsBefore[,c('long', 'lat')], points[,c('long','lat')])
  distA <- geosphere::distHaversine(points[,c('long', 'lat')], pointsAfter[,c('long', 'lat')])
  
  #Get minimum of distance before point and distance after point, if not calculated
  if(!('min_dist' %in% names(detections))){
    distances <- data.frame(distB=distB, distA=distA)
    di <- apply(distances, 1, function(x) min(x, na.rm=TRUE))
    data2$min_dist <- di #Find minimum distance of before and after
  }
  
  #Check if min_dist is valid (below the threshold, minDistValue) (1 if yes, 0 if no)
  data2$distValid<-apply(data2, 1, function(x) {
    val <- as.numeric(x["min_dist"])
    if (val < minDistValue) {
      1 #Valid
    } else {
      2 #Invalid
    }
  })
  
  #Combine liNA and data2 into detections
  detections2 <- rbind(liNA, data2)
  
  #Order detections2 by row names (numerically) to put in same order as detections
  detections2 <- detections2[order(as.numeric(detections2$rn)),]
  
  #Append added columns from detections2 into detections
  detections$min_dist <- detections2$min_dist
  detections$distValid <- detections2$distValid
  
  #Count number of rows of each validity score
  num1 <<- 0
  num2 <<- 0
  num3 <<- 0
  val <- detections$distValid
  l <- sapply(val, function(x) {
    if(x == 1) { #row with validity score of 1 (valid)
      num1 <<- num1+1
    } else if (x==2) { #row with validity score of 2 (questionable)
      num2 <<- num2+1
    } else { #row with validity score of 3 (missing information)
      num3 <<- num3+1
    }
  })
  nr <- nrow(detections)
  
  #Print results
  message(paste0("The filter identified ", num1," (", round((num1)/nr*100, 2), "%) of ", nr, " detections as valid using the distance test."))
  message(paste0("The filter identified ", num2," (", round((num2)/nr*100, 2), "%) of ", nr, " detections as questionable using the distance test."))
  message(paste0("The filter identified ", num3," (", round((num3)/nr*100, 2), "%) of ", nr, " detections as having missing information using the distance test."))
  
  return(detections)
}