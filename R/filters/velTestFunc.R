#General velocity test 
#Uses geosphere and dplyr packages
velTest <- function(data, type) {
  
  #Different column names from different types of data
  #Set different minimum velocity values to test against
  if(type == "sample") {
    dataColNames <- list(timestamp = "time", transmitters = "tr", receivers = "rec", long = "long", lat = "lat")
    minVelValue <- 1
  } else if (type == "GLATOS") {
    dataColNames <- list(timestamp = "detection_timestamp_utc", transmitters = "transmitter_id", receivers = "receiver_sn", long = "deploy_long", lat = "deploy_lat")
    minVelValue <- 10
  } else if (type == "OTN") {
    dataColNames <- list(timestamp = "datecollected", transmitters = "tagname", receivers = "receiver_group", long = "longitude", lat = "latitude")
    minVelValue <- 10
  } else {
    detColNames <- {}
    minVelValue <- 0
  }
  
  # Check that the specified columns appear in the detections dataframe
  missingCols <- setdiff(unlist(dataColNames), names(data))
  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }
  
  # Subset detections with only user-defined columns and change names
  # this makes code easier to understand (especially ddply)
  data2 <- data[,unlist(dataColNames)] #subset
  names(data2) <- c("timestamp", "transmitters","receivers","long", "lat")
  # data2$num <- as.numeric(data2$timestamp)
  
  
  # Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(data2$timestamp))){
    stop(paste0("Column '",dataColNames$timestampCol,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  distB <- {NA}
  distA <- {}
  points <- data.frame(long=data2$long, lat=data2$lat)
  
  #Calculate distance between each set of points using geosphere package
  for (i in 2:nrow(points)) {
    di <- geosphere::distHaversine(points[i-1,], points[i,])
    distB <- c(distB, di)
  }
  for(i in 1:(nrow(points)-1)) {
    di<- geosphere::distHaversine(points[i,], points[i+1,])
    distA <- c(distA, di)
  }
  distA <- c(distA, NA)
  
  #Get minimum of distance before point and distance after point
  distances <- data.frame(distB=distB, distA=distA)
  di <- apply(distances, 1, function(x) min(x, na.rm=TRUE))
  data$min_dist <- di #Find minimum distance of before and after
  
  
  #Get min_time
  n <-as.numeric(data2$timestamp)
  lagBefore <- n - dplyr::lag(n) #Time before point
  lagAfter <- dplyr::lead(n)-n #Time after point
  d <- data.frame(before = lagBefore, after = lagAfter)
  mLag<- apply(d, 1, function(x) min(x, na.rm=TRUE)) #Minimum of time before and after
  data$min_time <- mLag
  
  #Get min_vel
  data$min_vel<- apply(data, 1, function(x) {
    timeS <- as.numeric(x["min_time"])
    if(is.na(timeS))
      timeS <- strsplit(x["min_time"], " ")[[1]][1]
    if(timeS == 0) {
      0
    } else {
      as.numeric(x["min_dist"])/as.numeric(timeS)
    }
  })
  
  #Checking if min_vel is valid (1 if yes, 0 if no)
  data$velValid<-apply(data, 1, function(x) {
    val<-as.numeric(x["min_vel"])
    if (val<minVelValue) {
      1 #valid
    } else {
      0 #not valid
    }
  })
  
  return(data)
}