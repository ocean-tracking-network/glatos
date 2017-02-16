#' @title Simulate detection of transmitter signals in a receiver network
#' 
#' @description
#' Simulates detection of transmitter signals in a receiver network based on
#'   detection range curve (detection probability as a function of distance),
#'   location of transmitter, and location of receivers.
#'
#' @param trnsLoc A three-column data frame with locations (numeric columns 
#'   named 'x' and 'y') and timestamps (numeric or POSIXct column named 'et')
#'   where signals were transmitted.
#' @param recLoc A two-column data frame with receiver locations (numeric 
#'   columns named 'x' and 'y')
#' @param detRngFun A function that defines detection range curve;
#'   must accept a numeric vector of distances and return a numeric vector of 
#'   detection probabilities at each distance.
#'
#' @details
#' Distances between each signal transmission location and receiver are 
#' calculated using pythagorean theorem. The probability of detecting each 
#' signal on each receiver is determined from the detection range curve. 
#' Detection of each signal on each receiver is determined stochastically by 
#' draws from a Bernoulli distribution with probability p (detection prob).  
#'  
#' This function was written to be used along with 
#'   \code{\link{transmitAlongPath}}.
#' 
#' @return A data frame containing:
#' \item{trns_id}{Unique signal transmission ID}
#' \item{recv_id}{Unique receiver ID}
#' \item{recv_x}{Receiver x coordinate}
#' \item{recv_y}{Receiver y coordinate}
#' \item{trns_x}{Transmitter x coordinate}
#' \item{trns_y}{Transmitter y coordinate}
#' \item{etime}{Elapsed time}
#'
#' @seealso \code{\link{transmitAlongPath}} to simulate transmissions along a 
#' path (i.e., create \code{trnsLoc}).
#'
#' @author C. Holbrook (cholbrook@usgs.gov) 
#'
#' @examples
#' #none yet
#'
#' @export
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