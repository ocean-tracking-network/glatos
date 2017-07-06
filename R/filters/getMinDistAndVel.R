install.packages("dplyr")
library(dplyr)

load("/Users/dinian/Desktop/glatos/data/walleye_detections.rdata")
glatos <- walleye_detections
otn <- read.csv("/Users/dinian/Desktop/glatos/nsbs_matched_detections_2014.csv")
otn$datecollected <- as.POSIXct(otn$datecollected, tz="UCT")

#Practice data
d <- c("2010/10/10 10:52:07", "2010/10/10 10:59:23", "2010/10/10 11:03:26", "2010/10/10 11:06:15", "2010/10/10 11:07:15", "2010/10/10 11:09:10", "2010/10/10 11:11:11", "2010/10/10 11:16:16", "2010/10/10 11:19:27", "2010/10/10 11:28:17")
d <- as.POSIXct(d, tz="UCT")
#n <- as.numeric(d)
tr <- c(111, 111, 111, 111, 111, 111, 111, 111, 111,111)
rec <- c(11, 22, 33, 44,55, 66, 77, 33, 44, 11)
lat <- c(44.6358, 44.6362, 44.6373, 44.6368, 44.6371, 44.6375, 44.6380, 44.6373, 44.6368, 44.6358) 
long <- c(-63.5949, -63.5931, -63.5912, -63.5890, -63.5882, -63.5873, -63.5885, -63.5912, -63.5890, -63.5949)

dataS <- data.frame(id=c(1,2,3,4,5,6,7,8,9,10),time=d, tr=tr, rec=rec, lat=lat, long=long)

distB <- {NA}
distA <- {}
points <- data.frame(long=long, lat=lat)

#Get distance
#dataDistTest <- dataS[with(dataS, order(time)), ]

for (i in 2:nrow(points)) {
  di<-distHaversine(points[i-1,], points[i,])
  distB <- c(distB, di)
}
for(i in 1:(nrow(points)-1)) {
  di<- distHaversine(points[i,], points[i+1,])
  distA<- c(distA, di)
}
distA<- c(distA, NA)

distances <- data.frame(distB=distB, distA=distA)
di <- apply(distances, 1, function(x) min(x, na.rm=TRUE))
dataS$min_dist <- di #Finding minimum distance of before and after


#Getting min_time
lagBefore <- n - lag(n) #minLag1a
lagAfter <- lead(n)-n #minLag1b
d <- data.frame(before = lagBefore, after = lagAfter)
mLag<- apply(d, 1, function(x) min(x, na.rm=TRUE)) #minLag2 #Use this for distance measurement
dataS$min_time <- mLag

#Getting minLag
dataS2 <- dataS[with(dataS, order(tr, rec, time)), ]
li <- split(dataS2, list(dataS2$tr, dataS2$rec)) #Splitting by transmission id and receiver
mL<<-{}
#Calculating each min lag of each row for each data frame in the list
li<-li[sapply(li, function(x) dim(x)[1]) > 0]
list2<-lapply(li, function(x) {
  if(nrow(x)==1) {
    minLag2<-NA
  } else {
    n<-x$num
    mL1a <-(n-lag(n))
    mL1b <- (lead(n)-n)
    d<-data.frame(d1=mL1a, d2=mL1b)
    #print(d)
    minLag2<-apply(d,1, function(x) min(x, na.rm = TRUE))
    #print(minLag2)
  }
  mL<<- c(mL, minLag2) #appending minLag2 to mL
})

#Combining list of dataframes together
dataS2<- do.call("rbind", li)
dataS2$minLag <- mL #Setting min lag
dataS <-dataS2[with(dataS2, order(num)),] #Putting back in original order
rownames(dataS) <- 1:nrow(dataS) #Changing row names to 1,2,...

#Getting min_vel
dataS$min_vel<- apply(dataS, 1, function(x) {
  as.numeric(x["min_dist"])/as.numeric(x["min_time"])
})

#Checking if min_vel is valid (1 if yes, 2 if no, <1 means valid)
dataS$velValid<-apply(dataS, 1, function(x) {
  val<-as.numeric(x["min_vel"])
  if (val<1) {
    1
  } else {
    2
  }
})


#Another practice (between CS and Rebecca Cohn) (6 and 7)
CS2 <- c(-63.5873, 44.6375)
RC2 <- c(-63.5885, 44.6380)
times2 <- c("2010-10-10 11:09:10", "2010-10-10 11:11:11")
times2 <- as.POSIXct(times2, tz="UCT")

dist2 <- distm(CS2, RC2, fun = distHaversine)


#For GLATOS data



