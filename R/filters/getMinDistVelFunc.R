#From sample
velTestSample <-  function(data, minVelValue) {
  
  dataColNames <- list(timestamp = "time", transmitters = "tr", receivers = "rec", long = "long", lat = "lat")
  
  # Check that the specified columns appear in the detections dataframe
  missingCols <- setdiff(unlist(dataColNames), names(data))
  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }
  
  # Subset detections with only user-defined columns and change names
  # this makes code more easy to understand (esp. ddply)
  data2 <- data[,unlist(dataColNames)] #subset
  names(data2) <- c("timestamp, transmitters","receivers","long", "lat")
  data2$num <- as.numeric(data2$timestamp)
  
  
  # Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(data2$timestamp))){
    stop(paste0("Column '",dataColNames$timestampCol,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  distB <- {NA}
  distA <- {}
  points <- data.frame(long=data$long, lat=data$lat)
  
  #Get distance
  #dataDistTest <- dataS[with(dataS, order(time)), ]
  
  for (i in 2:nrow(points)) {
    di <- distHaversine(points[i-1,], points[i,])
    distB <- c(distB, di)
  }
  for(i in 1:(nrow(points)-1)) {
    di<- distHaversine(points[i,], points[i+1,])
    distA <- c(distA, di)
  }
  distA <- c(distA, NA)
  
  distances <- data.frame(distB=distB, distA=distA)
  di <- apply(distances, 1, function(x) min(x, na.rm=TRUE))
  data$min_dist <- di #Finding minimum distance of before and after
  
  
  #Getting min_time
  n <-data2$num
  lagBefore <- n - lag(n) #minLag1a
  lagAfter <- lead(n)-n #minLag1b
  d <- data.frame(before = lagBefore, after = lagAfter)
  mLag<- apply(d, 1, function(x) min(x, na.rm=TRUE)) #minLag2 #Use this for distance measurement
  data$min_time <- mLag
  
  # #Getting minLag
  # dataS2 <- dataS[with(dataS, order(tr, rec, time)), ]
  # li <- split(dataS2, list(dataS2$tr, dataS2$rec)) #Splitting by transmission id and receiver
  # mL<<-{}
  # #Calculating each min lag of each row for each data frame in the list
  # li<-li[sapply(li, function(x) dim(x)[1]) > 0]
  # list2<-lapply(li, function(x) {
  #   if(nrow(x)==1) {
  #     minLag2<-NA
  #   } else {
  #     n<-x$num
  #     mL1a <-(n-lag(n))
  #     mL1b <- (lead(n)-n)
  #     d<-data.frame(d1=mL1a, d2=mL1b)
  #     #print(d)
  #     minLag2<-apply(d,1, function(x) min(x, na.rm = TRUE))
  #     #print(minLag2)
  #   }
  #   mL<<- c(mL, minLag2) #appending minLag2 to mL
  # })
  # 
  # #Combining list of dataframes together
  # dataS2<- do.call("rbind", li)
  # dataS2$minLag <- mL #Setting min lag
  # dataS <-dataS2[with(dataS2, order(num)),] #Putting back in original order
  # rownames(dataS) <- 1:nrow(dataS) #Changing row names to 1,2,...
  
  #Getting min_vel
  data$min_vel<- apply(data, 1, function(x) {
    timeS <- as.numeric(x["min_time"])
    if(is.na(timeS))
      timeS <- strsplit(x["min_time"], " ")[[1]][1]
    as.numeric(x["min_dist"])/as.numeric(timeS)
    
  })

  #Checking if min_vel is valid (1 if yes, 0 if no, <1 means valid)
  data$velValid<-apply(data, 1, function(x) {
    val<-as.numeric(x["min_vel"])
    if (val<minVelValue) {
      1#"Valid"
    } else {
      0#"Not valid"
    }
  })
  
  return(data)
}