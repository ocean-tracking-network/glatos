calcCollisionProb = function(delayRng = c(60, 180), burstDur = 5.0, maxTags = 50, nTrans = 10000)
    {

	# preallocate objects
	pingHist <- vector("list", length = maxTags) # transmission history (before collisions)
	collide <- vector("list", length = maxTags) # list of logical value; indicates collision
	# preallocate
	detProbs <- data.frame(nTags= 1:maxTags, min= NA, q1= NA, med= NA, q3= NA, max= NA, mean= NA) 	
	pingHistObs <- vector("list", length = maxTags) # observed detection history (after collisions)

    # define transmission and detection histories for each tag
    for (i in 1:maxTags){
        #create transmission history; each list element is a tag, odd ind = start, even ind = end 
		# - draw transmissions from uniform distribution within delayRng
        pingStart <- cumsum(runif(nTrans, delayRng[1], delayRng[2])+burstDur) #random start time 
        pingHist[[i]] <- sort(c(pingStart, pingStart + burstDur))
		
		if(i==1) detProbs[i,] <- c(i, rep(1,6))
        if(i>1){  # check to see if collided with any previous tag
            for(j in 1:(i-1)) {
				#check to see if ith tag transmissions overlaps with any jth tag transmissions
                pingInts <- findInterval(pingHist[[i]], pingHist[[j]]) 
				
				#identify collisions (TRUE) or nonCollisions (FALSE)
                collisions <- (pingInts/2) != floor(pingInts/2) 
            
                collide[[j]] <- unique(c(collide[[j]], ceiling(pingInts[collisions]/2)))
                collide[[i]] <- unique(c(collide[[i]], 
					ceiling(row(as.matrix(collisions))[collisions]/2)))                  
              }
        
            detProb.k <- 1 - (sapply(collide[1:i], length)/(sapply(pingHist[1:i], length)/2))
        
            detProbs[i,2:7] <- c(fivenum(detProb.k), mean(detProb.k))                       
          } #end if
    
         detProbs <- round(detProbs, 3)  
      } #end i
  
      # calculate total number of hourly detects across all fish
      nomDelay <- median(delayRng) # nominal delay
      expDetsPerTagPerHr <- (3600/(nomDelay + burstDur)) #expected detects per tag per hour
    
      detProbs$expDetsPerHr <- expDetsPerTagPerHr*detProbs$nTags
      detProbs$totDetsPerHr <- round(with(detProbs, expDetsPerHr*mean), 0)
      detProbs$effDelay <- round(with(detProbs, nomDelay*(1/mean)), 0) 
      detProbs$detsPerTagPerHr <- round(with(detProbs, totDetsPerHr/nTags))
    
      return(detProbs)
    }