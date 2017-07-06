install.packages("dplyr")
library(dplyr)

load("/Users/dinian/Desktop/glatos/data/walleye_detections.rdata")
glatos <- walleye_detections
otn <- read.csv("/Users/dinian/Desktop/glatos/nsbs_matched_detections_2014.csv")
otn$datecollected <- as.POSIXct(otn$datecollected, tz="UCT")


dataSample <- data.frame(id=seq(from=1, to=10, by=1))
dates <- c("2013-09-17 08:14:22", "2013-09-17 08:15:22","2013-09-17 09:00:00", "2013-09-17 10:23:55", "2013-09-17 11:23:55", "2013-09-17 11:24:55", "2013-09-17 11:24:56", "2013-09-17 11:52:44", "2013-09-17 11:58:44", "2013-09-17 11:59:45")
dates<- as.POSIXct(dates, tz="UCT")
dataSample$date <- dates
calcMinLag <- function(n,i) {
  if(i==1) {
    return(NA)
  } else {
    return(n[i] - n[i-1])
  }
}

calcMinLag2 <- function(n,i) {
  #print(n)
  len <- length(n)
  if(i==1 && len > 1) {
      return (n[2] - n[1])
  } else if (i==len && len > 1) {
    return (n[len] - n[len-1])
  }
  lagBefore <- (n[i] - n[i-1])
  lagAfter <- (n[i+1] - n[i])
  
  if(lagBefore < lagAfter) {
    return(lagBefore)
  }
  else {
    return(lagAfter)
  }
}
n <- as.numeric(dataSample$date)
mL <- rep(x=0, times=nrow(dataSample))
for(i in 1:nrow(dataSample)) {
  mL[i] <- calcMinLag2(n,i)
}
dataSample$minLag <- mL



#CalcMinLag method for both
calcMinLag3 <- function(n, trans, rec, i, len) {
  iB <- 0
  iA <- len+1
  iBefore <-0
  iAfter <- len+1
  if(i==1 && len > 1) {
    iBefore <- 0
  } else {
    foundB <- FALSE
    iB <- i-1
    while(foundB == FALSE && iB > 0) {
      if(trans[iB]==trans[i] && rec[iB]==rec[i]) {
        foundB <- TRUE
        iBefore<-iB
      }
      iB <- iB-1
    }
  }
  if (i==len && len > 1) {
    iA <-len+1
  } else {
    foundA <- FALSE
    iA <- i+1
    while(foundA == FALSE && iA <= len) {
      if(trans[iA]==trans[i] && rec[iA]==rec[i]) {
        foundA <- TRUE
        iAfter <- iA
      }
      iA <- iA+1
    }
  }
  if(iAfter==(len+1) && iBefore==0) {
    return(NA)
  }else if (iAfter == (len+1)) {
    return (n[i]-n[iBefore])
  }else if (iBefore == 0) {
    return (n[iAfter]-n[i])
  }else{
    lagBefore <- (n[i] - n[iBefore])
    lagAfter <- (n[iAfter] - n[i])
    if(lagBefore < lagAfter) {
      return(lagBefore)
    }
    else {
      return(lagAfter)
    }
  }
}



#For GLATOS
glatos3 <- glatos

glatos3 <- glatos3[order(glatos3$detection_timestamp_utc, decreasing = FALSE), ]

n <- as.numeric(glatos3$detection_timestamp_utc)
len<-nrow(glatos3)
mL <- rep(x=0, times=len)
ptm<- proc.time()
for(i in 1:len) {
  mL[i] <- calcMinLag3(n, glatos3$transmitter_id, glatos3$receiver_sn, i, len)
}
glatos3$min_lag2<- mL
proc.time()-ptm


#For OTN
otn3 <- otn
otn3$datecollected<- as.POSIXct(otn3$datecollected, tz="UCT")
glatos3 <- glatos3[order(glatos3$detection_timestamp_utc, decreasing = FALSE), ]

n <- as.numeric(otn3$datecollected)
len <- nrow(otn3)
mL <- rep(x=0, times=len)
ptm<-proc.time()
for(i in 1:len) {
  mL[i] <- calcMinLag3(n, otn3$collectioncode, otn3$receiver_group, i, len)
}
otn3$min_lag2<-mL
proc.time()-ptm



#Practice
d <- c("2010/10/10 08:14:22", "2010/10/10 08:15:22", "2010/10/10 09:00:00", "2010/10/10 10:23:55", "2010/10/10 11:23:55", "2010/10/10 11:24:55", "2010/10/10 11:24:56", "2010/10/10 11:52:44", "2010/10/10 11:58:44", "2010/10/10 11:59:45")
d <- as.POSIXct(d)
n <- as.numeric(d)
tr <- c(121, 151, 161, 151, 151, 161,121, 151,161,121)
rec <- c(4,2,3,2,2,2,4,2,2,4)
dataS4 <- data.frame(id=c(1,2,3,4,5,6,7,8,9,10),time=d,num=n, tr=tr, rec=rec)
len<- nrow(dataS4)
mL <- rep(x=0, times=len)
mL2 <- mL
mL3 <- mL
for (i in 1: len) {
  mL[i] <- calcMinLag(dataS4$num, i)
  mL2[i] <- calcMinLag2(dataS4$num, i)
  mL3[i] <- calcMinLag3(dataS4$num, dataS4$tr, dataS4$rec, i, len)
}
dataS4$minLag <- mL
dataS4$minLag2 <- mL2
dataS4$minLag3 <- mL3


#Using dplyr
dataS5 <- dataS4[with(dataS4, order(tr, rec, time)), ]
lagBefore <- n - lag(n) #minLag1a
lagAfter <- lead(n)-n #minLag1b
d <- data.frame(before = lagBefore, after = lagAfter)
mLag<- apply(d, 1, function(x) min(x, na.rm=TRUE)) #minLag2
li <- split(dataS5, list(dataS5$tr, dataS5$rec)) #Splitting by transmission id and receiver

mL<<-{}
#Calculating each min lag of each row for each data frame in the list
li<-li[sapply(li, function(x) dim(x)[1]) > 0]
list2<-lapply(li, function(x) {
  if(nrow(x)==1) {
    minLag2<-NA
  } else {
    n<-x$num
    mL1a <-(n-lag(n))
    #print(mL1a)
    mL1b <- (lead(n)-n)
    #print(mL1b)
    d<-data.frame(d1=mL1a, d2=mL1b)
    #print(d)
    minLag2<-apply(d,1, function(x) min(x, na.rm = TRUE))
    #print(minLag2)
  }
  mL<<- c(mL, minLag2) #appending minLag2 to mL
})

#Combining list of dataframes together
dataSTogether <- do.call("rbind", li)
dataSTogether$minLag4 <- mL




#The one to use:

#With glatos data
#Using dplyr
#pmt <- proc.time() #Calculating time
glatos4 <- glatos[with(glatos, order(transmitter_id, receiver_sn, detection_timestamp_utc)), ]
li <- split(glatos4, list(glatos4$transmitter_id, glatos4$receiver_sn)) #Splitting by transmission id and receiver

mL<<-{}
#Calculating each min lag of each row for each data frame in the list
li<-li[sapply(li, function(x) dim(x)[1]) > 0]
list2<-lapply(li, function(x) {
  if(nrow(x)==1) {
    minLag2<-NA
  } else {
    n<-as.numeric(x$detection_timestamp_utc)
    mL1a <-(n-lag(n))
    #print(mL1a)
    mL1b <- (lead(n)-n)
    #print(mL1b)
    d<-data.frame(d1=mL1a, d2=mL1b)
    #print(d)
    minLag2<-apply(d,1, function(x) min(x, na.rm = TRUE))
    #print(minLag2)
  }
  mL<<- c(mL, minLag2) #appending minLag2 to mL
})

#Combining list of dataframes together
glatos5 <- do.call("rbind", li)
glatos5$minLag4 <- mL
#proc.time()-pmt #Calculating time





#With OTN data
#Using dplyr
#pmt<-proc.time() #calculating time
otn5 <- otn[with(otn, order(collectioncode, receiver_group, datecollected)), ] #Ordering data by transmitter, receiver_group, and date collected
li <- split(otn5, list(otn5$collectioncode, otn5$receiver_group)) #Splitting by transmitter id and receiver

mL<<-{}
#Calculating each min lag of each row for each data frame in the list
li<-li[sapply(li, function(x) dim(x)[1]) > 0]
list2<-lapply(li, function(x) {
  if(nrow(x)==1) {
    minLag2<-NA
  } else {
    n<-as.numeric(x$datecollected)
    mL1a <-(n-lag(n))
    mL1b <- (lead(n)-n)
    d<-data.frame(d1=mL1a, d2=mL1b)
    minLag2<-apply(d,1, function(x) min(x, na.rm = TRUE))
  }
  mL<<- c(mL, minLag2) #appending minLag2 to mL
})

#Combining list of dataframes together
otn6 <- do.call("rbind", li)
otn6$minLag4 <- mL

#proc.time()-pmt #calculating time

#OTN is faster than GLATOS
#dplyr is faster than the method
