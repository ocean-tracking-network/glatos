#GENERAL ONE
#Similar wording to detectionEventFilter
getMinLag <- function(detections, type) {
  if(type=="GLATOS") {
    detColNames <- list(transmitters = "transmitter_id", receivers = "receiver_sn", timestamp = "detection_timestamp_utc")
    detections$minLag <- detections$min_lag
  } else if (type == "OTN"){
    detColNames <- list(transmitters = "tagname", receivers = "receiver_group", timestamp = "datecollected")
  }
  else {
    detColNames <- {}
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
  detections2 <- detections[,unlist(detColNames)] #subset
  names(detections2) <- c("transmitters","receivers","timestamp")
  detections2$num <- as.numeric(detections2$timestamp)
  
  
  #Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(detections2$timestamp))){
    stop(paste0("Column '",detColNames$timestampCol,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  #Split detections by transmitter id and receiver
  li <- split(detections2, list(detections2$transmitters, detections2$receivers))
  
  #Create empty list for minimum lag
  mL <<- {} #<<- makes mL a global variable
  #Remove empty data frames from the list
  li <- li[sapply(li, function(x) dim(x)[1]) > 0]
  
  #Calculate each minimum lag of each row for each data frame in the list
  list2 <- lapply(li, function(x) {
    #print(paste0("nrow(x):",nrow(x)))
    if(nrow(x)==1) {
      #Set data frames in list with 1 entry to have a minimum lag of NA
      minLag2<-0 #Changed from NA to 0
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
  
  return(detections)
}
