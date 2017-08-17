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
#'     ('transmission_id' for GLATOS data, 'tagname' for OTN data, or 'transmitter'
#'                                                 for sample data).
#'     \item \code{receiversCol} is a character string with the name of the column
#'     containing the ids of the receivers
#'     ('receiver_sn' for GLATOS data, 'receiver_group' for OTN data, or 'receiver'
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

velTest <- function(detections, type, detColNames=list(), minVelValue=-100) {
  #Different column names from different types of data
  #Set different minimum velocity values to test against
  # If detColNames has not been set by user
  if(length(detColNames) == 0) {
    if(type == "sample") { #Set column names for sample data
      detColNames <- list(timestampCol = "time", transmittersCol = "transmitter", receiversCol = "receiver", longCol = "longitude", latCol = "latitude")
      minVelValue <- 1
    } else if (type == "GLATOS") { #Set column names for GLATOS data
      detColNames <- list(timestampCol = "detection_timestamp_utc", transmittersCol = "transmitter_id", receiversCol = "receiver_sn", longCol = "deploy_long", latCol = "deploy_lat")
      minVelValue <- 10
    } else if (type == "OTN") { #Set column names for OTN data
      detColNames <- list(timestampCol = "datecollected", transmittersCol = "tagname", receiversCol = "receiver_group", longCol = "longitude", latCol = "latitude")
      minVelValue <- 10
    } else { #Other type
      stop(paste0("The type '",type,"' is not defined."), call.=FALSE)
    }
  }
  # If minVelValue has not been set by user
  if(minVelValue == -100) {
    if(type == "sample") { #Set minimum velocity for sample data
      minVelValue <- 1
    } else if (type == "GLATOS") { #Set minimum velocity for GLATOS data
      minVelValue <- 10
    } else if (type == "OTN") { #Set minimum velocity for OTN data
      minVelValue <- 10
    } else { #Other type
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
  # this makes code easier to understand (especially ddply)
  data2 <- detections[,unlist(detColNames)] #subset
  names(data2) <- c("timestamp", "transmitters","receivers","long", "lat")
  # data2$num <- as.numeric(data2$timestamp)
  
  
  # Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(data2$timestampCol))){
    stop(paste0("Column '",detColNames$timestampCol,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  #Set of points, which are made up of: (longitude, latitude)
  points <- data.frame(long=data2$long, lat=data2$lat)
  
  #Gets list of points before the current and after the current point (lag and lead, respectively)
  pointsBefore <- data.frame(long=dplyr::lag(data2$long), lat=dplyr::lag(data2$lat))
  pointsAfter <- data.frame(long=dplyr::lead(data2$long), lat=dplyr::lead(data2$lat))
  
  #Calculates distance between each set of points by calculating distance between previous and current point, and distance between current and next point and labelling each list distB and distA, respectively
  distB <- geosphere::distHaversine(pointsBefore[,c('long', 'lat')], points[,c('long','lat')])
  distA <- geosphere::distHaversine(points[,c('long', 'lat')], pointsAfter[,c('long', 'lat')])
  
  #Get minimum of distance before point and distance after point
  distances <- data.frame(distB=distB, distA=distA)
  di <- apply(distances, 1, function(x) min(x, na.rm=TRUE))
  detections$min_dist <- di #Find minimum distance of before and after
  
  #Calculate minimum time (min_time) between current point and the point before or after
  n <- as.numeric(data2$timestamp)
  lagBefore <- n - dplyr::lag(n) #Time between current point and before point
  lagAfter <- dplyr::lead(n)-n #Time between current point and after point
  d <- data.frame(before = lagBefore, after = lagAfter)
  mLag <- apply(d, 1, function(x) min(x, na.rm=TRUE)) #Minimum of time before and after
  detections$min_time <- mLag
  
  #Calculate minimum velocity (min_vel) between current point and the point before or after
  detections$min_vel<- apply(detections, 1, function(x) {
    timeS <- as.numeric(x["min_time"])
    # if(is.na(timeS))
    #   timeS <- strsplit(x["min_time"], " ")[[1]][1]
    if(timeS == 0) { #If time of current point and before or after point is the same, return 0 as dividing it will give an error
      0
    } else {
      as.numeric(x["min_dist"])/as.numeric(timeS) #Dividing distance and time to calculate minimum speed
    }
  })
  
  #Check if min_vel is valid (below the threshold, minVelValue) (1 if yes, 0 if no)
  detections$velValid<-apply(detections, 1, function(x) {
    val<-as.numeric(x["min_vel"])
    if (val<minVelValue) {
      1 #valid
    } else {
      0 #not valid
    }
  })
  
  return(detections)
}