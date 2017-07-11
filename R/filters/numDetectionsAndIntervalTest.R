# General method to do the number of detections and interval ratio test, a new tool
# Uses dplyr package
# Can work with OTN or GLATOS data
# To use:
# For glatos data, numIntervalTest(glatos, "GLATOS")
# For OTN gata, numIntervalTest(otn, "OTN")
# For sample data, numIntervalTest(sample, "sample")
#
# To test this, I used the sample data:
# id  time                  tr    rec
# 1   2010/10/11 11:11:11   11    21
# 2   2010/10/11 11:12:52   12    22
# 3   2010/10/11 11:13:11   11    21
# 4   2010/10/11 12:52:13   11    22
# 5   2010/10/12 07:23:52   12    21
# 6   2010/10/12 11:12:52   12    22
# 7   2010/10/12 12:16:43   12    22
# 8   2010/10/12 12:52:13   11    22
# 9   2010/10/12 12:52:14   11    21
#10   2010/10/12 14:16:44   12    22
#11   2010/10/12 19:23:37   11    22
#12   2010/10/12 19:24:37   11    22
#13   2010/10/12 21:24:36   11    22
#14   2010/10/13 11:11:11   12    22
#15   2010/10/13 15:23:44   11    21

#
# You can use the following code to make this data:

# id <- 1:15
# d <- c("2010/10/11 11:11:11", "2010/10/11 11:12:52", "2010/10/11 11:13:11", "2010/10/11 12:52:13", "2010/10/12 07:23:52", "2010/10/12 11:12:52", "2010/10/12 12:16:43", "2010/10/12 12:52:13", "2010/10/12 12:52:14", "2010/10/12 14:16:44", "2010/10/12 19:23:37", "2010/10/12 19:24:37", "2010/10/12 21:24:36", "2010/10/13 11:11:11", "2010/10/13 15:12:23")
# d <- as.POSIXct(d, tz="UCT")
# tr <- c(11, 12, 11, 11, 12, 12, 12, 11, 11, 12, 11, 11, 11, 11, 12)
# rec <- c(21, 22, 21, 22, 21, 22, 22, 22, 21, 22, 22, 22, 22, 21, 22)
# 
# dataS <- data.frame(id=id, time=d, tr=tr, rec=rec)

# Similar wording in method headers to detectionEventFilter from GLATOS
numIntervalTest <- function(detections, type) {
  if(type=="GLATOS") { #Set column names for Glatos data
    detColNames <- list(transmitters = "transmitter_id", receivers = "receiver_sn", timestamp = "detection_timestamp_utc")
    detections$minLag <- detections$min_lag
  } else if (type == "OTN"){ #Set column names for OTN data
    detColNames <- list(transmitters = "tagname", receivers = "receiver_group", timestamp = "datecollected")
  }else if (type == "sample") { #Set column names for sample data described above
    detColNames <- list(transmitters = "tr", receivers = "rec", timestamp = "time")
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