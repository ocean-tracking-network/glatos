#' Interpolate positions within a pair of geographic coordinates using
#' using linear ("great circle") or non-linear ("lowest cost") methods
#'
#' \code{movePath} calculates interpolated positions between a pair of 
#' coordinates using linear ("great circle") or non-linear 
#' ("lowest cost") interpolation methods. The main purpose of this function is 
#' to prevent interpolation of movement paths through inaccessible locations 
#' (e.g., over land for fish). 
#' 
#' @param startX start longitude, vector of length 1
#' 
#' @param startY start latitude, vector of length 1 
#' 
#' @param endX end longitude, vector of length 1
#' 
#' @param endY end latitude, vector of length 1
#' 
#' @param iTime A vector of datetime stamps (MUST be of class POSIXct) at which 
#'   to interpolate geographic positions between start and end locations. The 
#'   first element must correspond with the position at \code{startX} and 
#'   \code{startY} and the last element must correspond with the position at 
#'   \code{startX} and \code{startY}. 
#'   
#' @param trans An optional transition matrix with the "cost" of moving across 
#'   across each cell within the map extent. Must be of class 
#'   \code{TransitionLayer}. See \code{gdistance} package.
#'   
#' @param iThresh A numeric threshold for determining if linear or non-linear 
#'   interpolation will be used based on the ratio of linear-to-non-linear
#'   shortest path distances.
#' 
#' @details 
#' This function operates on a single pair of geographic (start and end) 
#' positions and is the primary function in \code{\link{interpolatePath}}. 
#'
#' @details 
#' Non-linear interpolation uses the 'gdistance' package to find the shortest 
#' pathway between two locations (i.e., receivers) that avoid 'impossible' 
#' movements (e.g., over land for fish). The shortest non-linear path between 
#' two locations is calculated using a 'transition matrix layer' (\code{trans}) 
#' that represents the 'cost' of an animal moving between adjacent grid cells. 
#' For example, each cell in \code{trans} may be coded as water (1) or land (0)
#' to represent possible (1) and impossible (0) movement path. 
#' 
#' @details
#' If \code{trans} is not specified, or if the start and end positions are 
#' the same, then new positions are calculated using linear interpolation 
#' according to the values in \code{iTime}. If \code{trans} is specified and 
#' start and end points are different, then method used (linear or non-linear)
#' depends on the ratio of linear-to-non-linear distances between points. 
#' Linear interpolation will be used when the ratio is larger than 
#' \code{iThresh} and non-linear interpretation will be used when the ratio is 
#' smaller than \code{iThresh}. \code{iThresh} can be used to control whether 
#' non-linear or linear interpolation is used for all points. For example, 
#' non-linear interpolation will be used for all points when 
#' \code{iThresh} = 1 and linear interpolation will be used for all points 
#' when \code{iThresh} = 0.
#' 
#' @return Data frame with interpolated timestamp, lat, and lon
#'
#' @seealso \code{\link{interpolatePath}}
#'
#' @author Todd Hayden
#' 
#' @examples
#' #example transition matrix
#' data(greatLakesTrLayer)
#'  
#' #example map background
#' data(greatLakesPoly)
#' library(sp) #to plot SpatialPolygon without error
#' plot(greatLakesPoly)
#' 
#' #make up two points
#' x <- c(-87,-82.5)
#' y <- c(44, 44.5)
#'
#' #coerce to SpatialPoints object and plot
#' pts <- SpatialPoints(cbind(x,y))
#' points(pts, pch=20, col='red', cex=3)
#'
#' #interpolate path using linear method
#' path1 <- movePath(startX=x[1], startY=y[1], endX=x[2], endY=y[2],
#'   iTime=as.POSIXct("2000-01-01 00:00") + 1:30)
#' 
#' #coerce to SpatialPoints object and plot
#' pts1 <- SpatialPoints(path1[,c("lon","lat")])
#' points(pts1, pch=20, col='blue', lwd=2, cex=1.5) 
#'
#' #interpolate path using non-linear method (requires 'trans')
#' path2 <- movePath(startX=x[1], startY=y[1], endX=x[2], endY=y[2],
#'   iTime=as.POSIXct("2000-01-01 00:00") + 1:30,
#'   trans=greatLakesTrLayer)
#'
#' #coerce to SpatialPoints object and plot
#' pts2 <- SpatialPoints(path2[,c("lon","lat")])
#' points(pts2, pch=20, col='green', lwd=2, cex=1.5) 
#'
#' #can also force linear interpolation by setting 'lnlThresh' = 0
#' path3 <- interpolatePath(pos, 
#'   rast=greatLakesTrLayer, lnlThresh=0,
#'   detColNames=list(individualCol="id", timestampCol="time",
#'   longitudeCol="x", latitudeCol="y"))
#' 
#coerce to SpatialPoints object and plot
#' pts3 <- SpatialPoints(path3[,c("x","y")])
#' points(pts3, pch=20, col='magenta', lwd=2, cex=1.5) 
#' 
#' @export

movePath <- function (startX, startY, endX, endY, iTime, trans=NULL, 
	iThresh=0.9){

	#check if iTime is of class POSIXct 
	if(!("POSIXct" %in% class(iTime))) stop("'iTime' must be of class POSIXct.")
	
	#calculate the "great circle" (linear) distance between points 
	gcd <- geosphere::distHaversine(c(startX, startY), c(endX, endY))
	
	#calculate the least cost (non=linear) distance between points
	# if trans was given and start and end points differ; NA otherwise
	lcd <- ifelse(is.null(trans) | gcd == 0, NA, 
		gdistance::costDistance(trans, c(startX, startY), c(endX, endY)))
		
	#define start and end timestamps
	startTS <- iTime[1]
	endTS <- iTime[(length(iTime))]
	
	#use linear interpolation if lcd is NA or if linear-non-linear distance
	# ratio exceeds iThresh
	if (is.na(lcd) | (gcd/lcd) >= iThresh ){
		#linear interpolation	
		x <- approx(c(startTS, endTS), c(startX, endX), xout = iTime)$y
		y <- approx(c(startTS, endTS), c(startY, endY), xout = iTime)$y
		z <- data.frame(lon=x, lat=y, timestamp = iTime)
		z <- z[c(-1, -(nrow(z))),] #remove first and last
		
		return(z)
	} else {
		#use non-linear interpolation
		path <- as.data.frame(sp::coordinates(gdistance::shortestPath(trans, 
			c(startX, startY), c(endX, endY), output = "SpatialLines")))
			
		#add original points
		path <- rbind(c(startX, startY), path, c(endX, endY))
		
		#calculate cumulative distance moved
		cumdist <- cumsum(c(0, sqrt(diff(path$x)^2 + diff(path$y)^2)))
		
		#interpolate timestamps based on cumulative distance moved
		out <- as.POSIXct(approx(cumdist,	c(startTS, rep(NA,(length(cumdist)-2)), 
			endTS) , xout = cumdist)$y, origin = "1970-01-01 00:00:00", 
			tz = attr(iTime, "tzone"))

		#interpolate x and y locations based on timestamps
		pathLon <- approx(out, path$x, xout = iTime)$y
		pathLat <- approx(out, path$y, xout = iTime)$y
		z <- data.frame(lon = pathLon, lat = pathLat, timestamp = iTime)
		z <- z[c(-1, -(nrow(z))),] #remove first and last
		return(z)
	}
}
