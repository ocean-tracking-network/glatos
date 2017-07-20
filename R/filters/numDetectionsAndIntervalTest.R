# General method to do the number of detections and interval ratio test, a new tool
# Uses dplyr package
# Can work with OTN or GLATOS data
# To use:
# For glatos data, numIntervalTest(glatos, "GLATOS")
# For OTN gata, numIntervalTest(otn, "OTN")
# For sample data, numIntervalTest(sample, "sample")

# Similar wording in method headers to detectionEventFilter from GLATOS
numIntervalTest <- function(detections, type) {
  if(type=="GLATOS") { #Set column names for Glatos data
    detColNames <- list(transmitters = "transmitter_id", receivers = "receiver_sn", timestamp = "detection_timestamp_utc")
    detections$minLag <- detections$min_lag
  } else if (type == "OTN"){ #Set column names for OTN data
    detColNames <- list(transmitters = "tagname", receivers = "receiver_group", timestamp = "datecollected")
  }else if (type == "sample") { #Set column names for sample data described above
    detColNames <- list(transmitters = "transmitter", receivers = "receiver", timestamp = "time")
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
        if(y<=2*60*60) {#short interval, <=2 hours
          shortInt <<- shortInt + 1
        } else if (y>=24*60*60) {#long interval, >=24 hours
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

  return(detections2)
}