#simulate tag signal detections
detectTransmissions <- function(trnsLoc=NA,recLoc=NA,detRngFun=NA){
	 
	 #preallocate detection data frame
	 dtc <- data.frame(
          trns_id = NA,
          recv_id = NA,
          recv_x = NA,
          recv_y = NA,
          trns_x = NA,
          trns_y = NA,
          etime = NA)[0,]
	 
	 #loop through receivers (because should be much smaller than transmissions)
	 for(g in 1:nrow(recLoc)){
		#initialize progress bar
	     if(g==1)  pb <- winProgressBar(title=paste0("Simulating detections for current track"), label="0% done", min=0, max=    nrow(recLoc), initial=0)
	  
	    #distance between gth receiver and each transmission
	    distM.g <- sqrt((trnsLoc$x-recLoc$x[g])^2 +
                    (trnsLoc$y-recLoc$y[g])^2)
        detP.g <- detRngFun(distM.g) #calculate probability of detection
        succ.g <- as.logical(rbinom(length(detP.g), 1, detP.g)) #simulate detection
	 
	    #output detection data
      if(sum(succ.g) > 0){
		  dtc.g <- data.frame(
			  trns_id = which(succ.g),
			  recv_id = g,
			  recv_x = recLoc$x[g],
			  recv_y = recLoc$y[g],
			  trns_x = trnsLoc$x[succ.g],
			  trns_y = trnsLoc$y[succ.g],
			  etime = trnsLoc$et[succ.g])
		 
		  dtc <- rbind(dtc,dtc.g) #append
	  } # end if
	 
		#update progress bar
		info <- sprintf("%d%% done", round(g/nrow(recLoc)*100))
		setWinProgressBar(pb, g, label=info)
		if(g==nrow(recLoc)) close(pb)
	 } #end g
      dtc <- dtc[order(dtc$etime),]#sort by time	 
             
      return(dtc)
}