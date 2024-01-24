#' Estimate probability of collision for telemetry transmitters
#' 
#' Estimate (by simulation) probability of collision for co-located telemetry 
#'   transmitters with pulse-period-modulation type encoding 
#'
#' @param delayRng A 2-element numeric vector with minimum and maximum delay 
#'   (time in seconds from end of one coded burst to beginning of next).
#'   
#' @param burstDur A numeric scalar with duration (in seconds) of each coded 
#'   burst (i.e., pulse train).
#'   
#' @param maxTags A numeric scalar with maximum number of co-located  
#'   transmitters (within detection range at same time).
#'   
#' @param nTrans A numeric scalar with the number of transmissions to simulate 
#'   for each co-located transmitter.
#'
#' @details
#' Calculates the detection probability associated with collision, given delay
#' range (delayRng), burst duration (burstDur), maximum number of co-located 
#' tags (maxTags), and number of simulated transmission per tag (nTrans). The
#' simulation estimates detection probability due only to collisions (i.e., when
#' no other variables influence detection probability) and assuming that all 
#' tags are co-located at a receiver for the duration of the simulation.
#' 
#' @return A data frame containing summary statistics:
#'   \item{nTags}{Number of tags within detection range at one time}
#'   \item{min}{Minimum detection probability among simulated tags}
#'   \item{q1}{First quartile of detection probabilities among simulated tags}
#'   \item{median}{Median detection probability among simulated tags}
#'   \item{q3}{Third quartile of detection probabilities among simulated tags}
#'   \item{max}{Maximum detection probability among simulated tags}
#'   \item{mean}{Mean detection probability among simulated tags}
#'   \item{expDetsPerHr}{Expected number of detections per hour assuming 
#'      perfect detection probability, given the number of tags within 
#'      detection range}
#'   \item{totDetsPerHr}{Observed number of detections per hour for a given 
#'      number of tags}
#'   \item{effDelay}{Eeffective delay of the transmitter after incorporating 
#'      collisions}
#'   \item{detsPerTagPerHr}{Mean number of detections per hour per tag}

#' @references
#' For application example, see: \cr\cr
#' Binder, T.R., Holbrook, C.M., Hayden, T.A. and Krueger, C.C., 2016. Spatial and
#'   temporal variation in positioning probability of acoustic telemetry arrays:
#'   fine-scale variability and complex interactions. Animal Biotelemetry, 4(1):1.
#'   \cr <http://animalbiotelemetry.biomedcentral.com/articles/10.1186/s40317-016-0097-4>
#'
#' @author C. Holbrook (cholbrook@usgs.gov) and T. Binder
#'
#' @examples
#' # parameters analagous to Vemco tag, global coding, 45 s nominal delay
#' foo <- calc_collision_prob(delayRng = c(45, 90), burstDur = 5.12, maxTags = 50,
#'     nTrans = 10000)
#'     
#' # plot probabilities of detection
#' plot(med~nTags, data=foo, type='p', pch=20, ylim=c(0,1),
#'     xlab="# of transmitters within range", ylab="Probability of detection")
#'   
#' # plot probability of collision by subtracting detection probability from 1
#' plot((1 - med)~nTags, data=foo, type='p', pch=20, ylim=c(0,1),
#'   xlab="# of transmitters within range", ylab="Probability of collision")
#'
#' @export
calc_collision_prob = function(delayRng = c(60, 180), burstDur = 5.0, 
  maxTags = 50, nTrans = 10000)
    {

	# preallocate objects
  
  # transmission history (before collisions)
	pingHist <- vector("list", length = maxTags) 
	# list of logical value; indicates collision
	collide <- vector("list", length = maxTags) 
	# preallocate
	detProbs <- data.frame(nTags= 1:maxTags, min= NA, q1= NA, med= NA, q3= NA, 
	  max= NA, mean= NA) 	
	# observed detection history (after collisions)
	pingHistObs <- vector("list", length = maxTags) 

  # define transmission and detection histories for each tag
  for (i in 1:maxTags){
    #create transmission history; each list element is a tag, 
    # odd ind = start, even ind = end 
		# draw transmissions from uniform distribution within delayRng
    #random start time 
    pingStart <- cumsum(runif(nTrans, delayRng[1], delayRng[2])+burstDur) 
    pingHist[[i]] <- sort(c(pingStart, pingStart + burstDur))
		
		if(i==1) detProbs[i,] <- c(i, rep(1,6))
    if(i>1){  # check to see if collided with any previous tag
      for(j in 1:(i-1)) {
				#check to see if ith tag transmissions overlaps 
        # with any jth tag transmissions
        pingInts <- findInterval(pingHist[[i]], pingHist[[j]]) 
				
				#identify collisions (TRUE) or nonCollisions (FALSE)
        collisions <- (pingInts/2) != floor(pingInts/2) 
            
        collide[[j]] <- unique(c(collide[[j]], ceiling(pingInts[collisions]/2)))
        collide[[i]] <- unique(c(collide[[i]], 
				ceiling(row(as.matrix(collisions))[collisions]/2)))                  
      }
        
      detProb.k <- 1 - (sapply(collide[1:i], length)/
        (sapply(pingHist[1:i], length)/2))
        
      detProbs[i,2:7] <- c(fivenum(detProb.k), mean(detProb.k))                       
    } #end if
    
    detProbs <- round(detProbs, 3)  
  } #end i
  
  # calculate total number of hourly detects across all fish
  nomDelay <- median(delayRng) # nominal delay
  #expected detects per tag per hour
  expDetsPerTagPerHr <- (3600/(nomDelay + burstDur)) 

  detProbs$expDetsPerHr <- expDetsPerTagPerHr*detProbs$nTags
  detProbs$totDetsPerHr <- round(with(detProbs, expDetsPerHr*mean), 0)
  detProbs$effDelay <- round(with(detProbs, nomDelay*(1/mean)), 0) 
  detProbs$detsPerTagPerHr <- round(with(detProbs, totDetsPerHr/nTags))

  return(detProbs)
}
