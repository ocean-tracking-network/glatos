# General method to do the velocity test, a new tool
# Uses geosphere and dplyr packages
# Can work with OTN or GLATOS data
# To use:
# For glatos data, velTest(glatos, "GLATOS")
# For OTN data, velTest(otn, "OTN")
#
# To test this, I used the sample data:
# id  time                  tr   rec  long      lat
# 1   2010/10/10 10:52:07   111   11  -63.5949  44.6358 (Steele Ocean Science building)
# 2   2010/10/10 10:59:23   111   22  -63.5931  44.6362 (Henry Hicks building)
# 3   2010/10/10 11:03:26   111   33  -63.5912  44.6373 (Killam Library)
# 4   2010/10/10 11:06:15   111   44  -63.5890  44.6368 (Student Union Building)
# 5   2010/10/10 11:07:15   111   55  -63.5882  44.6371 (Kenneth Rowe building)
# 6   2010/10/10 11:09:10   111   66  -63.5873  44.6375 (Goldberg Computer Science building)
# 7   2010/10/10 11:11:11   111   77  -63.5885  44.6380 (Rebecca Cohn)
# 8   2010/10/10 11:16:16   111   33  -63.5912  44.6373 (Killam Library)
# 9   2010/10/10 11:19:27   111   44  -63.5890  44.6368 (Student Union Building)
# 10  2010/10/10 11:28:17   111   11  -63.5949  44.6358 (Steele Ocean Science building)
#
# You can use the following code to make this data:

# d <- c("2010/10/10 10:52:07", "2010/10/10 10:59:23", "2010/10/10 11:03:26", "2010/10/10 11:06:15", "2010/10/10 11:07:15", "2010/10/10 11:09:10", "2010/10/10 11:11:11", "2010/10/10 11:16:16", "2010/10/10 11:19:27", "2010/10/10 11:28:17")
# d <- as.POSIXct(d, tz="UCT")
# tr <- c(111, 111, 111, 111, 111, 111, 111, 111, 111,111)
# rec <- c(11, 22, 33, 44,55, 66, 77, 33, 44, 11)
# lat <- c(44.6358, 44.6362, 44.6373, 44.6368, 44.6371, 44.6375, 44.6380, 44.6373, 44.6368, 44.6358) 
# long <- c(-63.5949, -63.5931, -63.5912, -63.5890, -63.5882, -63.5873, -63.5885, -63.5912, -63.5890, -63.5949)
# 
# dataS <- data.frame(id=c(1,2,3,4,5,6,7,8,9,10),time=d, tr=tr, rec=rec, long=long, lat=lat)

# and check it against a minimum velocity of 1 to see if it is valid

# Similar wording in method headers to detectionEventFilter from glatos
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
  data$min_dist <- di #Find minimum distance of before and after
  
  #Calculate minimum time (min_time) between current point and the point before or after
  n <- as.numeric(data2$timestamp)
  lagBefore <- n - dplyr::lag(n) #Time between current point and before point
  lagAfter <- dplyr::lead(n)-n #Time between current point and after point
  d <- data.frame(before = lagBefore, after = lagAfter)
  mLag <- apply(d, 1, function(x) min(x, na.rm=TRUE)) #Minimum of time before and after
  data$min_time <- mLag
  
  #Calculate minimum velocity (min_vel) between current point and the point before or after
  data$min_vel<- apply(data, 1, function(x) {
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