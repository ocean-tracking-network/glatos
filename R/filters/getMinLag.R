# General method to return a 'min_lag' column in the data
# Can work with OTN or GLATOS data
# To use:
# For glatos data, getMinLag(glatos, "GLATOS")
# For OTN data, getMinLag(otn, "OTN")
#
# Similar wording to detectionEventFilter from glatos
getMinLag <- function(detections, type) {
  if(type=="GLATOS") { #Set column names for Glatos data
    detColNames <- list(transmitters = "transmitter_id", receivers = "receiver_sn", timestamp = "detection_timestamp_utc")
    detections$minLag <- detections$min_lag
  } else if (type == "OTN"){ #Set column names for OTN data
    detColNames <- list(transmitters = "tagname", receivers = "receiver_group", timestamp = "datecollected")
  }else { #Other
    detColNames <- {}
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
      minLag2<-NA
    } else {
      n <- as.numeric(x$timestamp)
      #Calculate time between current and previous entry for each entry
      mL1a <- (n-dplyr::lag(n))
      #Calculate time between current and next entry for each entry
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
  detections2 <- detections2[order(detections2$num),]
  detections$min_lag <- detections2$mLag
  
  #Return original dataset with "min_lag" column attached to it
  return(detections)
}
