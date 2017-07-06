otn <- read.csv("/Users/dinian/Desktop/glatos/nsbs_matched_detections_2014.csv")

#OTN: falseDetectionFilter()
otnSorted <- otn[order(as.Date(otn$datecollected)), ,drop=FALSE]
otnSorted$datecollected <- as.POSIXct(otnSorted$datecollected, tz = "UCT")
calcMinLag <- function(n,i) {
  if(i == 1) {
    return(NA)
  }
  else {
    return (n[i]-n[i-1])
  }
}
n <- as.numeric(otnSorted$datecollected)
mL <- rep(x=0, times=nrow(otnSorted))
for( i in 1:nrow(otnSorted)) {
  mL[i] <- calcMinLag(n,i)
}
otnSorted$min_lag <- mL

otnFalse <- falseDetectionFilter(detections = otnSorted, tf=3600)

#OTN: detectionEventFilter()
otnChanged <- otn
otnChanged$datecollected <- as.POSIXct(otnChanged$datecollected, tz="UCT")
otnFilter <- detectionEventFilter(otnChanged, detColNames=list(locationCol="station", animalCol="tagname", timestampCol="datecollected", latCol="latitude", longCol="longitude"), timeSep=172800)

