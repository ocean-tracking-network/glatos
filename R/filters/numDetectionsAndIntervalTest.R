#' Perform number of detections and interval test (Steckenreuter et al., 2016), which checks if the minimum velocity is above a threshold
#'   and returns a validity column using the geosphere and dplyr libraries
#'
#' @param detections A data frame containing detection data with at least 
#'   3 columns containing 'transmitters', 'receivers', and 'timestamp'. 
#'   Column names are specified by \code{type}.
#'   
#' @param type A character string that contains the type of data that is being passed in,
#'   for example, "OTNDet", "OTNQual", "GLATOS", or "sample".
#' 
#' @param detColNames An optional list that contains the user-defined column
#'   names
#' 
#' @param shortIntSec An optional integer that contains the maximum number of seconds in a 
#'   'short interval' (usually 2 hours or 2*60*60 seconds)
#' 
#' @param longIntSec An optional integer that contains the minimum number of seconds in a
#'   'long interval' (usually 24 hours or 24*60*60 seconds)
#'   
#' @details detColNames is defined as a list with the names of the required columns in
#' \code{detections}, defined by \code{type}:
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
#'                                                 'rcvrcatnumber' for OTN qualified data,
#'                                                 or 'receiver' for sample data).
#'   }
#'
#' @details An instance is invalid (0) if it is the only detection with its transmitter id and receiver id
#' @details An instance is valid (1) if for its transmitter id and receiver id, it has more short 
#'   intervals (<=2 hours) than long intervals (>=24 hours)
#' 
#' @return A data frame containing the data with a valid column appended to it which appears as 1 if it is
#'   valid and 0 if not
#'
#' @references (in APA) Steckenreuter, A., Hoenner, X., Huveneers, C., Simpfendorfer, C., Buscot, M.J., 
#'   Tattersall, K., ... Harcourt, R. (2016). Optimising the design of large-scale acoustic telemetry
#'   curtains. Marine and Freshwater Research. doi: 10.1071/mf/16126
#' 
#' @author A. Dini
#'
#' @usage To use:
#'   For GLATOS data, numIntervalTest(data, "GLATOS")
#'   For OTN detection data, numIntervalTest(data, "OTNDet")
#'   For OTN qualified data, numIntervalTest(data, "OTNQual")
#'   For sample data, numIntervalTest(data, "sample")
#'
#' @export

# Similar wording in method headers to detectionEventFilter from GLATOS
numIntervalTest <- function(detections, type, detColNames=list(), shortIntSec= 2*60*60, longIntSec=24*60*60) {
  # Check if user has set column names
  if(length(detColNames) == 0) {
    if(type == "GLATOS") { #Set column names for GLATOS data
      detColNames <- list(timestamp = "detection_timestamp_utc", transmittersCol = "transmitter_id", receiversCol = "receiver_sn")
      detections$minLag <- detections$min_lag
    } else if (type == "OTNDet"){ #Set column names for OTN detection data
      detColNames <- list(timestamp = "datecollected", transmitters = "tagname", receivers = "receiver_group")
    } else if (type == "OTNQual"){ #Set column names for OTN qualified data
      detColNames <- list(timestamp = "datecollected", transmitters = "fieldnumber", receivers = "rcvrcatnumber")
    } else if (type == "sample") { #Set column names for sample data described above
      detColNames <- list(timestamp = "time", transmitters = "transmitter", receivers = "receiver")
    } else { #Other
      stop(paste0("The type '",type,"' is not defined."), call.=FALSE)
    }
  }
  
  # Check that the specified columns above appear in the detections dataframe
  missingCols <- setdiff(unlist(detColNames), names(detections))
  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }
  
  # Subset detections with only user-defined columns and change names
  # this makes code easier to understand (especially ddply)
  detections2 <- detections[,unlist(detColNames)] #subset
  names(detections2) <- c("timestamp", "transmitters", "receivers")
  detections2$num <- as.numeric(detections2$timestamp) #Add another column
  
  #Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(detections2$timestamp))){
    stop(paste0("Column '",detColNames$timestampCol,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  #Split detections by transmitter id and receiver
  li <- split(detections2, list(detections2$transmitters, detections2$receivers))
  
  #Remove empty data frames from the list
  li <- li[sapply(li, function(x) dim(x)[1]) > 0]
  
  #Calculate each minimum lag of each row for each data frame in the list
  list2 <- lapply(li, function(x) {
    if(nrow(x)==1) {
      #Set data frames in list with 1 entry to have a validity of 0 (number of detections test)
      x$valid <- 0
      x
    } else {
      n <- as.numeric(x$time)#stamp)
      #Calculate time between current and previous entry for each entry
      mL1a <- (n-dplyr::lag(n))
      #Calculate time between current and next entry for each entry
      mL1b <- (dplyr::lead(n)-n)
      #Combine into a data frame
      d <- data.frame(d1=mL1a, d2=mL1b)
      #Calculate minimum of the two for each row, ignoring the NA entries
      minLag2 <- apply(d,1,function(x) min(x, na.rm = TRUE))
      
      #Count number of short intervals (<=2 hours) and number of long intervals (>=24 hours)
      shortInt <<- 0
      longInt <<- 0
      sapply(minLag2, function(y) {
        if(y<=shortIntSec) {#short interval
          shortInt <<- shortInt + 1
        } else if (y>=longIntSec) {#long interval
          longInt <<- longInt + 1
        }
      })
      #Check if number of short intervals is larger than number of long intervals (interval ratio test)
      x$valid <- sapply(minLag2, function(y) {
        #Return valid if number of short intervals is larger than number of long intervals
        if(shortInt > longInt){
          1
        } else {
          0
        }
      })
      x
    }
  })
  
  #Combine list of data frames
  detections2 <- do.call("rbind", list2)
  detections2 <- detections2[order(detections2$num),] #Put them back in the original order to be able to append the answers to detections
  
  #Add results to original 'detections' dataframe
  detections$numIntervalValid <- detections2$valid
    
  #Print out results
  numVal <<- 0
  val <- detections2$valid
  l <- sapply(val, function(x) {
    if(x == 1) {
      numVal <<- numVal+1
    }
  })
  nr <- nrow(detections2)
  message(paste0("The filter identified ", nr-sum(detections2$valid)," (", round((nr - sum(detections2$valid))/nr*100, 2), "%) of ", nr, " detections as invalid using the number of detections and interval ratio test."))

  return(detections)
}