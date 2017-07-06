#FOR GLATOS
#Similar wording to detectionEventFilter
getMinLagGLATOS <- function(detections) {
  detColNames <- list(transmitters = "transmitter_id", receivers = "receiver_sn", timestamp = "detection_timestamp_utc")
  
  # Check that the specified columns appear in the detections dataframe
  missingCols <- setdiff(unlist(detColNames), names(detections))
  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }

  # Subset detections with only user-defined columns and change names
  # this makes code more easy to understand (esp. ddply)
  detections2 <- detections[,unlist(detColNames)] #subset
  names(detections2) <- c("transmitters","receivers","timestamp")
  detections2$num <- as.numeric(detections2$timestamp)


  # Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(detections2$timestamp))){
    stop(paste0("Column '",detColNames$timestampCol,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }

  #Sort detections by transmitter, then receiver, then time
  #detections <- detections[order(detections$transmitters, detections$receivers, detections$timestamp),]

  #Split detections by transmitter id and receiver
  li <- split(detections2, list(detections2$transmitters, detections2$receivers))

  #Create empty list for minimum lag
  mL <<- {} #<<- makes mL a global variable
  #print(mL)
  #Remove empty data frames from the list
  li <- li[sapply(li, function(x) dim(x)[1]) > 0]

  #Calculate each minimum lag of each row for each data frame in the list
  list2 <- lapply(li, function(x) {
    #print(paste0("nrow(x):",nrow(x)))
    if(nrow(x)==1) {
      #Set data frames in list with 1 entry to have a minimum lag of NA
      minLag2<-NA
    } else {
      n <- as.numeric(x$timestamp)
      #Calculate time between current and previous entry for each entry
      mL1a <- (n-lag(n))
      #print(paste0("mL1a:",mL1a))
      #Calculate time between current and next entry for each entry
      mL1b <- (lead(n)-n)
      #print(paste0("mL1b:",mL1b))
      #Combine into a data frame
      d <- data.frame(d1=mL1a, d2=mL1b)
      #print(paste0("d:",d))
      #Calculate minimum of the two for each row, ignoring the NA entries
      minLag2 <- apply(d,1,function(x) min(x, na.rm = TRUE))
      #print(paste0("minLag2:",minLag2))
    }
    #Append answer to mL list
    #mL <- c(mL, minLag2)
    mL <<- append(mL, minLag2)
    #print(length(mL))
    #c<-c+1
  })
  #print(length(mL))
  # #Combine list of data frames
  detections2 <- do.call("rbind", li)
  #print(mL)
  #Add min lag column to detections data frame
  #print(length(mL))
  detections2$mLag <- mL
  #detections$minLag <- mL
  detections2 <- detections2[order(detections2$num),]
  #detections <- detections[order(as.numeric(detections$timestamp))]
  detections$minLag <- detections2$mLag
  return(detections)
}
