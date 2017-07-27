#' Calculate 'min_lag' column of a dataset, add this column into the dataset,
#'   and return the dataset with the new column
#'
#' @param detections A data frame containing detection data with at least 
#'   3 columns containing 'transmitter', 'receiver', and 'timestamp'. 
#'   Column names are specified by \code{type}.
#'   
#' @param type A character string that contains the type of data that is being passed in,
#'   for example, "OTN", "GLATOS", or "sample".
#'   
#' @details detColNames is defined as a list with the names of the required columns in
#' \code{detections}, defined by \code{type}:
#'  \itemize{
#'    \item \code{transmitters} is a character string with the name of the column
#'     containing the ids of the transmitters
#'     ('transmission_id' for GLATOS data, 'tagname' for OTN data, or 'transmitter'
#'                                                 for sample data).
#'     \item \code{receivers} is a character string with the name of the column
#'     containing the ids of the receivers
#'     ('receiver_sn' for GLATOS data, 'receiver_group' for OTN data, or 'receiver'
#'                                                 for sample data).
#'     \item \code{timestampCol} is a character string with the name of the column
#'     containing datetime stamps for the detections (MUST be of class 'POSIXct')
#'     ('detection_timestamp_utc' for GLATOS data, 'datecollected' for OTN data, or
#'                                                 'time' for sample data).
#'   }
#'
#' @details Each value in the min_lag column defines the minimum number of seconds
#'  between either a time with the same transmitter and receiver if before or after
#'  the current time. This is calculated with the dplyr package in R. This min_lag 
#'  column will be used in falseDetectionFilter.R to filter false data.
#'  @details 
#' A new column (\code{min_lag}), indicating the minimum lag of each entry (according
#' to the definition above), is added to the input data frame.
#' 
#' @return A data frame containing the data with a min_lag column appended:
#'	\item{min_lag}{Minimum lag between.}
#' 	\item{MeanLongitude}{Mean longitude of detections comprising each event.}
#'  \item{FirstDetection}{The time of the first detection in a given detection 
#'    event.} 
#'  \item{LastDetection}{The time of the last detection in a given detection 
#'    event.}  
#'  \item{NumDetections}{The total number of detection that comprised a given
#' 	  detection event.}
#'  \item{ResTime_sec}{The elapsed time in seconds between the first and last 
#'		detection in a given event.}
#'
#' @author A. Dini
#'
#' @usage To use:
#'   For GLATOS data, getMinLag(data, "GLATOS")
#'   For OTN data, getMinLag(data, "OTN")
#'   For sample data, getMinLag(data, "sample")
#'
#' @export

getMinLag <- function(detections, type) {
  if(type=="GLATOS") { #Set column names for GLATOS data
    detColNames <- list(transmittersCol = "transmitter_id", receiversCol = "receiver_sn", timestampCol = "detection_timestamp_utc")
    detections$minLag <- detections$min_lag
  } else if (type == "OTN"){ #Set column names for OTN data
    detColNames <- list(transmittersCol = "tagname", receiversCol = "receiver_group", timestampCol = "datecollected")
  } else if (type == "sample") { #Set column names for sample data
    detColNames <- list(transmittersCol = "transmitter", receiversCol = "receiver", timestampCol = "time")
  } else { #Other type
    stop(paste0("The type '",type,"' is not defined."), call.=FALSE)
  }
  
  # Check that the specified columns above appear in the detections dataframe
  missingCols <- setdiff(unlist(detColNames), names(detections))
  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }
  
  # Subset detections with only user-defined columns and change names
  # This makes code easier to understand
  detections2 <- detections[,unlist(detColNames)] #subset
  names(detections2) <- c("transmitters","receivers","timestamp")
  detections2$num <- as.numeric(detections2$timestamp) #Add another column
  
  #Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(detections2$timestamp))){
    stop(paste0("Column '",detColNames$timestampCol,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  #Split detections by transmitter id and receiver
  li <- split(detections2, list(detections2$transmitters, detections2$receivers))
  
  #Create empty list for minimum lag
  mL <<- {} # <<- makes mL a global variable
  #Remove empty data frames from the list
  li <- li[sapply(li, function(x) dim(x)[1]) > 0]
  
  #Calculate each minimum lag of each row for each data frame in the list
  list2 <- lapply(li, function(x) {
    if(nrow(x)==1) {
      #Set data frames in list with 1 entry to have a minimum lag of NA
      minLag2 <- NA
    } else {
      n <- as.numeric(x$timestamp)
      #Calculate time between current and previous entry for each entry
      mL1a <- (n-dplyr::lag(n))
      #Calculate time between current and next entry for each entry (using dplyr)
      mL1b <- (dplyr::lead(n)-n)
      #Combine into a data frame
      d <- data.frame(d1=mL1a, d2=mL1b)
      #Calculate minimum of the two for each row, ignoring the NA entries
      minLag2 <- apply(d,1,function(x) min(x, na.rm = TRUE))
    }
    #Append answer to mL list
    mL <<- append(mL, minLag2)
  })
  #Combine list of data frames
  detections2 <- do.call("rbind", li)
  #Add min lag column to detections data frame
  detections2$mLag <- mL
  detections2 <- detections2[order(detections2$num),] #Put them back in the original order to be able to append the answers to detections
  detections$min_lag <- detections2$mLag
  
  #Return original dataset with "min_lag" column attached to it
  return(detections)
}
