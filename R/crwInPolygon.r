#' Simulate a correlated random walk inside a polygon
#' 
#' Uses glatos::crw to simulate a random walk as series of equal-length steps 
#'   with turning angles drawn from a normal distribution inside a polygon.
#'
#' @param polyg A polygon defined as data frame with columns x and y.
#' @param theta A 2-element numeric vector with turn angle parameters 
#'   (theta[1] = mean; theta[2] = sd) from normal distribution.
#' @param stepLen A numeric scalar with total distance moved in each step.
#' @param initPos A 2-element numeric vector with nital position (initPos[1]=x, 
#'   initPos[2]=y).
#' @param initHeading A numeric scalar with initial heading in degrees.
#' @param nsteps A numeric scalar with number of steps to simulate.
#'
#' @details
#' If initPos = NA, then a starting point is randomly selected within the 
#' polygon boundary. A path is simulated forward using the crw function. 
#' Initial heading is also randomly selected if initHeading = NA. When a step 
#' crosses the polygon boundary, a new heading for that step is drawn (from 
#' turn angle distribution truncated by polygon boundary), and the remainder of 
#' the track is rotated accordingly.
#' 
#' @return A two-column data frame containing:
#' \item{x}{x coordinates}
#' \item{y}{y coordinates}
#'
#' @author C. Holbrook (cholbrook@usgs.gov) 
#'
#' @note 
#' The path is constructed in 1000-step segments (currently hard coded at 1000).
#' In the event that the truncated turn angle distribution (at boundary) leaves
#' no possible solution (i.e., path has entered a crevice in the polygon 
#' boundary) then the simulation will stop and an error will indicate that the 
#' path is stuck at a boundary. Solution is to try simulation again or to 
#' simplify/smooth polygon boundary.
#'
#' @examples
#' mypolygon <- data.frame(x=c(-50,-50, 50, 50),y=c(-50,50,50,-50))
#' foo <- crwInPolygon(mypolygon,theta=c(0,5), stepLen=10, initPos=c(0,0),
#'   initHeading=0, nsteps=50)
#' plot(foo,type="b",pch=20,cex=0.2,asp=c(1,1),xlim=range(mypolygon$x),
#'   ylim=range(mypolygon$y))
#' polygon(mypolygon)
#'
#' @export
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