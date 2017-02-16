#' @export
#function to simulate crw within a polygon
 crwInPolygon <- function(polyg=data.frame(x=c(0,0,150000,150000),y=c(0,50000,50000,0)),
              theta=c(0,10), 
              stepLen=100, 
              initPos=c(NA,NA), 
              initHeading=NA,
			  nsteps=30){          
              			  
      #randomly select one point in the study area
      if(any(is.na(initPos))){
		  inPoly <- FALSE #logical flag; preallocate
		  while(inPoly == FALSE){
			   init <- c(runif(1, min(polyg$x),max(polyg$x)),
					runif(1, min(polyg$y),max(polyg$y)))
			   inPoly <- point.in.polygon(init[1],init[2],polyg$x,polyg$y)
			   inPoly <- switch(inPoly+1,FALSE,TRUE,FALSE,FALSE)
		  } #end while
	  } #end if
	  
	  if(all(!is.na(initPos))) {
		init <- initPos
		if(point.in.polygon(init[1],init[2],polyg$x,polyg$y)==0) stop("initPos is outside polygon boundary.")
	  } #end if
	  
	  #randomly select heading
	  if(is.na(initHeading)) initHeading <- runif(1,0,360)
	  
	  winSz <- 1000 #size of window
	  
	  mod <- nsteps%/%winSz #number of times to loop through full window
	  rmd <- nsteps%%winSz #size of last window

	  path.fwd <- data.frame(x=rep(NA,nsteps+1),y=NA) #preallocate
	  path.fwd[1,] <- init
	  rows.i <- 1 #rows for ith window iteration; preallocate
		
	  #initialize progress bar
	  pb <- txtProgressBar(title="Status of current path", label="0% done", min=0, max=mod+1, initial=0)	  

	  nwin <- mod+(rmd>0) #number of windows
	  for(i in 1:nwin){
		  #simulate track forward
		  #update starting point and heading
		  init <- as.vector(path.fwd[max(rows.i),]) #start at previous end
		  if(i>1) {
			initHeading <- vectorHeading(path.fwd$x[max(rows.i)-1:0],path.fwd$y[max(rows.i)-1:0])
		  }
		  if(i==nwin & rmd>0) winSz <- rmd #size of last window
		  rows.i <- max(rows.i)+(1:winSz) #update rows for this iteration
		  path.fwd[rows.i,] <- crw(theta=theta,stepLen=stepLen,initPos=init,
				   initHeading,nsteps=winSz)

		  #check if in polygon
		  inPoly <- point.in.polygon(path.fwd$x[rows.i],path.fwd$y[rows.i],polyg$x,polyg$y)

		  while(!all(inPoly)){
			  outside <- match(0,inPoly) #identify first step outside
			  #truncate turn angle distribution based on encounter with boundary
			  
			  constrainThetaToPolygon <- function(x,y,len,polyg,theta){
					heading_deg <- vectorHeading(x,y)
					heading_deg2 <- ((heading_deg-180):(heading_deg+180)) %% 360
					heading_rad <- heading_deg2 * pi/180 #convert to radians
					xlen <- sin(heading_rad)*len #x-component vector
					ylen <- cos(heading_rad)*len #y-component vector
					bar <- data.frame(x=x[2]+xlen,y=y[2]+ylen,theta=-180:180)
					#check if sampled points in polygon
					bar$inPoly <- point.in.polygon(bar$x,bar$y,polyg$x,polyg$y)
					if(sum(bar$inPoly) == 0) stop("Path stuck at boundary.")
					probs <- bar$inPoly*dnorm(-180:180,theta[1],theta[2])
					theta2 <- sample(bar$theta,1,prob=probs)
					return(theta2) #new turn angle			
			  } 
			  
			  theta2 <- constrainThetaToPolygon(x=path.fwd$x[rows.i[outside]+(-2:-1)],
				y=path.fwd$y[rows.i[outside]+(-2:-1)],stepLen,polyg,theta)	  
			  
			  #rotate track from first step outside polygon to end
			  path.fwd[rows.i,][outside:length(rows.i),] <- rotatePoints(
				x=path.fwd$x[rows.i][outside:length(rows.i)],
				y=path.fwd$y[rows.i][outside:length(rows.i)],
				theta=theta2,
				focus=path.fwd[rows.i[outside]-1,])
			  
			  #identify points in and out of polygon	
			  inPoly <- point.in.polygon(path.fwd$x[rows.i],path.fwd$y[rows.i],polyg$x,polyg$y)
			  
		  } #end while
		 
		  #update progress bar
			info <- sprintf("%d%% done", round(i/(nwin)*100))
			setTxtProgressBar(pb, i, label=info)
			if(i==(nwin)) close(pb)		
		} #end i
      
      return(path.fwd)
    } #end crwInRect def