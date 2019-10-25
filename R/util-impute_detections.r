#' Impute missed detections in a time series
#'
#' @param x A dataframe containing (minimally) timestamp (POSIXct), transmitter id, and receiver id
#' @param dtc_col_names A list with name of required columns for timestamp, transmitter, and receiver
#' @param trnsm Name of specific transmitter ids (or vector of ids) to subset; all others will be ignored and omitted from result
#' @param rcvr Name of specific receiver id (or vector of ids) to subset; all others will be ignored and omitted from result
#' @param int 2-element vector of timestamps (POSIXct) that define the start and end of the time period of interest; if NA (default) then first and last timestamp in x will be used
#' @details Only supports fixed delay (period) transmitters.
#' @return a dataframe with new column \code{detected} and imputed (missed) detections added (new rows) 
#' @examples
#' mydelay <- 45 #fixed 45 sec delay
#' tx <-  as.POSIXct('1999-01-01 12:59:40', tz="GMT") + mydelay*(1:99) #simulate 100 delays
#' tx <- sample(tx, size=30) #randomly select 30
#' tx <- sort(tx) #sort increasing
#' df <- data.frame(tx=tx, tid=123, rid=999) #make data frame
#' myint <- as.POSIXct(c("1999-01-01 13:00:00", "1999-01-01 14:15:00"), tz="UTC")
#' df2 <- impute_detections(df, 
#'          dtc_col_names = list(timestampCol = "tx", 
#'                               transmitterCol = "tid", 
#'                               receiverCol = "rid"),
#'          int = myint)
#' @export
impute_detections <- function(dtc, 
  dtc_col_names = list(timestampCol = "detection_timestamp_utc", 
                       transmitterCol = "transmitter", 
                       receiverCol = "receiver"), 
    trnsm = NULL, rcvr = NULL, int = NULL) {
  
  dtcx <- data.table::as.data.table(dtc)
  
  #set names of required column names to defaults
  data.table::setnames(dtcx, dtc_col_names$timestampCol, "detection_timestamp_utc") 
  data.table::setnames(dtcx, dtc_col_names$transmitterCol, "transmitter") 
  data.table::setnames(dtcx, dtc_col_names$receiverCol, "receiver") 
  
    
  #check that timestamp increasing
  if(any(diff(dtcx$detection_timestamp_utc)<0)) 
    warning("timestamps were not sorted ",
    "decreasingly, so records have been sorted in output.")
  
  data.table::setkey(dtcx, detection_timestamp_utc)
  
  #preallocate new column for detection indicator
  #check for existing column named "detected"
  if("detected" %in% tolower(names(dtcx))) stop("Input object `x` already ", 
    "contains a column named `detected`.\n\tRename that column to use ",
    "this function.")
  dtcx[ , detected := 1]
  
	#assign int from data if NA
	if(missing(int) | is.null(int) | all(is.na(int))) 
	  int <- range(dtcx$detection_timestamp_utc)

	#omit records outside int
	omit_rows <- findInterval(dtcx$detection_timestamp_utc, int, 
	                         rightmost.closed = TRUE) != 1
	if(sum(omit_rows) > 0){
		dtx <- dtcx[-omit_rows,] #drop rows
		warning(sum(omit_rows)," detections were removed from the beginning or end",
      "of the dataframe because they were outside the specified interval.")
	}
	
	#define transmitter list if missing
	if(missing(trnsm) | is.null(trnsm) | all(is.na(trnsm)))
	  trnsm <- data.table(
	      transmitter = unique(dtcx$transmitter),
	      delay_min = NA_real_,
	      delay_max = NA_real_)
	
	#define receiver list
	if(missing(rcvr) | is.null(rcvr) | all(is.na(rcvr))) 
	  rcvr <- sort(unique(dtcx$receiver))
			
	#estimate fixed delay (random delays not supported)
	trnsm[, delay_min := 
	    estimate_delay(dtcx[, list(detection_timestamp_utc,
                            	   transmitter,
                            	   receiver)], 
	                   recs = rcvr,
	                   tags = .BY$transmitter)$median_obs_delay[1],
	  by = "transmitter"]
	
	trnsm[, delay_max := delay_min]
	
	#loop through each tag-receiver combination
	for(i in 1:length(rcvr)){
		for(j in 1:nrow(trnsm)){
					
			#subset data for ith receiver and jth transmitter
			x.ij <- dtcx[receiver == rcvr[i] & 
			             transmitter == trnsm$transmitter[j], ]
			
			#get median expected delay for this tag (among receivers)
			exp_delay_j <- unlist(trnsm[transmitter == trnsm$transmitter[j],
			                            list(median(delay_min, delay_max))])
			
			
			#if no detections, impute using random starting point
			if(nrow(x.ij) == 0){
			  
			  #check for detections on another reciever; otherwise use random
			  t0 <- dtcx[transmitter == 
			      trnsm$transmitter[j],]$detection_timestamp_utc[1]
			  
			  #convert int to fractional number of delays from first detection
			  int_del <- (as.numeric(int) - as.numeric(t0)) / exp_delay_j			  
			  
			  #get sequence of transmissions numbers
			  trns <- ceiling(int_del[1]):floor(int_del[2])
			  
			  #estimate
			  trns_miss <- trns * exp_delay_j
			  trns_miss <- t0 + trns_miss			  
			}
			
			#if detections, impute missed using linear regression
			if(nrow(x.ij) > 0){
  			# note that y=TRUE is an lm argument (passed via ...) to return x
  			lm_ij <- estimate_fixed_delay_lm(x.ij, exp_delay_j, x = TRUE)
  			
  			#convert int to fractional number of delays from first detection
  			int_del <- (as.numeric(int) - 
  			            as.numeric(x.ij$detection_timestamp_utc[1]) + 
  			            coef(lm_ij)[1]) / coef(lm_ij)[2]
  			
  			#get sequence of transmissions numbers
  			trns <- ceiling(int_del[1]):floor(int_del[2])
  			trns <- setdiff(trns, lm_ij$x[,"y"])
  			
  			#estimate
  			trns_miss <- predict(lm_ij, newdata = data.frame(y = trns)) 
  			trns_miss <- x.ij$detection_timestamp_utc[1] + trns_miss
			}
  				
			#check
			#foo <- predict(lm_ij, newdata = data.frame( y = lm_ij$x[,"y"]))
			#cbind(as.numeric(x.ij[[dtc_col_names$timestampCol]]), 
			#      as.numeric(x.ij[[dtc_col_names$timestampCol]][1]) + foo)
		
			#add missed transmissions
			x2 <- as.data.frame(x.ij)[0,] #make like x
			x2[1:length(trns_miss),] <- NA
			x2$detection_timestamp_utc <- trns_miss
			x2$transmitter <- trnsm$transmitter[j]
			x2$receiver <- rcvr[i]
			x2[["detected"]] <- 0	
			
			#append to missed detections
			if(i==1 & j==1) { x_missed <- x2 } else {
				x_missed <- rbind(x_missed, x2) 
			}
		} #end j
	} #end i
	
	#combine
	xout <- dtcx[dtcx$receiver %in% rcvr & 
	             dtcx$transmitter %in% trnsm$transmitter,]
	xout <- rbind(xout ,x_missed)
	data.table::setkey(xout, detection_timestamp_utc) #sort by time  
	
	#change names back
	data.table::setnames(xout, "detection_timestamp_utc", 
	                      dtc_col_names$timestampCol)
	data.table::setnames(xout, "transmitter", dtc_col_names$transmitterCol)
	data.table::setnames(xout, "receiver", dtc_col_names$receiverCol)
		
  return(xout)
}


#' estimate fixed delay
#' 
#' function to estimate time between detections
#' @export
estimate_delay <- function(dtc, tags = NA, recs = NA, tolerance = 1){
  if(all(is.na(recs))) recs <- unique(dtc$receiver) #unique receivers
  if(all(is.na(tags))) tags <- unique(dtc$transmitter) #unique transmitters
  
  expdel <- matrix(NA, nrow=length(recs), ncol=length(tags))
  rownames(expdel) <- recs
  colnames(expdel) <- tags
  
  for(i in 1:length(recs)){
    for(j in 1:length(tags)){
      #find mode of time between detections  - expected delay
      dtc_ij <- dtc[dtc$receiver == recs[i] & dtc$transmitter == tags[j]]

      if(nrow(dtc_ij) > 3){
        #first pass (coarse)
        exp_delay1 <- estimate_fixed_delay_density(dtc_ij, bw = 0.1)
        
        #second pass (focus around first pass estimate)
        x_range <- exp_delay1$period + c(-1, 1) * tolerance
        exp_delay2 <- estimate_fixed_delay_density(dtc_ij, 
                                                   from = x_range[1], 
                                                   to = x_range[2], 
                                                   bw = 0.01)
        
        #third pass (use regression to dial it in)
        exp_delay3 <- estimate_fixed_delay_lm(dtc_ij, exp_delay2$period)
        
        expdel[i, j] <- coef(exp_delay3)[2]
      }
    } #end j
  } #end i
  
  expdel <- reshape2::melt(expdel, id = 1:ncol(expdel), 
                           value.name = "median_obs_delay")
  setDT(expdel)
  setnames(expdel, c("Var1", "Var2"), c("receiver", "transmitter"))
  #among-receiver median for each tag
  expdel <- expdel[ , list(median_obs_delay = median(median_obs_delay,
                                                     na.rm = TRUE)),
                  keyby = "transmitter"]
  return(expdel)
}


#' estimate fixed delay from detection data using density method
#' @param dtc A data frame with detection data
#' @param ... Optional named list of arguments passed to \code{density}.
#' @details This only accepts data for a single tag-receiver pair.
#' 
#' @export
estimate_fixed_delay_density <- function(dtc, ...){
  
  dtcx <- as.data.table(dtc)
  data.table::setkey(dtcx, detection_timestamp_utc)
  
  #check for more than one receiver
  if(data.table::uniqueN(dtcx$receiver) > 1) stop("input `dtc` must only ",
    "contain data from one receiver.")
  
  #check for more than one transmitter
  if(data.table::uniqueN(dtcx$transmitter) > 1) stop("input `dtc` must only ",
    "contain data from one transmitter.")
  
  #calculate time differences
  dif <- diff(as.numeric(dtcx$detection_timestamp_utc))
      
  if(length(dif) >= 3) {
      
    #update default values based on optional arguments passed via ...
    args_density <- list() #default values
    in_args_density <- list(...)
    args_density[names(in_args_density)] <- in_args_density
    
    dens <- do.call(density, c(list(x=dif), args_density))
    #plot(dens)
    
    x_at_peak <- dens$x[which.max(dens$y)] #expected delay

    return(list(period = x_at_peak, dens = dens))
  } else {
    warning("At least four detections are required.")
  }
}


#' estimate fixed delay from detection data using regression method
#' 
#' @param dtc A data frame with detection data
#' @param ... Optional named list of arguments passed to \code{lm}.
#' @details This only accepts data for a single tag-receiver pair.
#' 
#' @export
estimate_fixed_delay_lm <- function(dtc, exp_delay, ...){
  
  dtcx <- as.data.table(dtc)
  data.table::setkey(dtcx, detection_timestamp_utc)
  
  #check for more than one receiver
  if(data.table::uniqueN(dtcx$receiver) > 1) stop("input `dtc` must only ",
    "contain data from one receiver.")
  
  #check for more than one transmitter
  if(data.table::uniqueN(dtcx$transmitter) > 1) stop("input `dtc` must only ",
    "contain data from one transmitter.")
  
  #update default values based on optional arguments passed via ...
  args_lm <- list() #default values
  in_args_lm <- list(...)
  args_lm[names(in_args_lm)] <- in_args_lm
  
  #convert timestamps to elapsed time since first detection
  x <- cumsum(c(0, diff(as.numeric(dtcx$detection_timestamp_utc))))
  # then convert to number of transmissions since first detection
  y <- round(x / exp_delay)
  #estimate best fit line 
  
  #lm_xy <- lm(x~y)
  lm_xy <- do.call(lm, c(list(formula = x~y), args_lm))
  
  return(lm_xy)
  
  # #diagnostics
  # foo$fitted <- predict(lm_xy, foo)
  # foo$difft <- with(foo, as.numeric(fitted) - as.numeric(x))
  # foo$diffdifft <- with(foo, c(0,diff(difft)))
  # diff(foo$difft)
  # plot(difft~x, data = foo, type = "o", pch = 20, cex = 0.1)
  # plot(diffdifft~x, data = foo, type = "o", pch = 20, cex = 0.1)
  # 
  #use predict to impute missing timestamps  
}