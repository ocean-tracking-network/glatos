falseDetectionFilter <- function(detections, type, tf, minLagCol = "min_lag"){
  # Check that the minLag column is in the detections dataframe
  if (!(minLagCol %in% names(detections))){
    #stop(paste0("The column '",minLagCol, "' must appear in the detections data frame."), call.=FALSE)
    detections <- getMinLag(detections, type) #Get min_lag column
  }
  
  # Identify possible false detections by comparing "min_lag" column to 
  #  threshold defined in object "tf".
  detections$passedFilter <- ifelse(!is.na(detections[,minLagCol]) & 
                                      detections[,minLagCol] <= tf, 1, 0)
  nr <- nrow(detections)
  with(detections,
       message(paste0("The filter identified ", 
                      nr - sum(passedFilter), " (", 
                      round((nr - sum(passedFilter))/
                              nr*100, 2), "%) of ", nr, 
                      " detections as potentially false.")))
  return(detections)
}