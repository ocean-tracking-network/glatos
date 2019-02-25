#' @title Simulate telemetry transmitter signals along a path
#' 
#' @description
#' Simulate tag signal transmission along a pre-defined path (x, y coords)
#'   based on constant movement velocity, transmitter delay range, and duration
#'   of signal.
#'
#' @param path A two-column data frame with at least two rows and columns 
#'   \code{x} and \code{y} with coordinates that define path.
#' @param vel A numeric scalar with movement velocity along track; assumed 
#'   constant.
#' @param delayRng A 2-element numeric vector with minimum and maximum delay 
#'   (time in seconds from end of one coded burst to beginning of next).
#' @param burstDur  A numeric scalar with duration (in seconds) of each coded 
#'   burst (i.e., pulse train).
#'   
#' @param EPSG Numeric EPSG code of the coordinate system used for simulations. 
#'   Default is 3175, a projected coordinate system for the North American 
#'   Great Lakes Basin and St. Lawrence River system. 
#'   \url{http://spatialreference.org/ref/epsg/nad83-great-lakes-and-st-lawrence-albers/} 
#' 
#' @param sp_out Logical. If TRUE (default) then output is a 
#'  \link[sp]{SpatialPoints} object. If FALSE, then output is a 
#'  data.frame.
#'
#' @details
#' Delays are drawn from uniform distribution defined by delay range.
#' First, elapsed time in seconds at each node in path is calculated based on 
#' path length and velocity. Next, delays are simulated and burst durations 
#' are added toeach delay to determine the time of each signal transmission. 
#' Location of each signal transmission along the path is linearly interpolated.
#' 
#' @details If \code{path} object is a data frame with x and y columns and
#'   \code{sp_out} argument is TRUE, then SpatialPointsDataFrame
#'   output object will have coordinate system of \code{EPSG}. Coordinate
#'   system on output will be same as input if path object is
#'   \code{\link[sp]{SpatialPoints}}.
#'
#' @return A SpatialPointsDataFrame object in the same CRS as the
#'   input \code{path} object. \cr \emph{OR} \cr \item{x}{ x coordinates for
#'   start of each transmission } \item{y}{ y coordinates for start of each
#'   transmission } \item{et}{ elapsed time to start of each transmission }
#'
#' @note
#' This function was written to be called before 
#'   \code{\link{detect_transmissions}}, which was designed to accept the result
#'   as input (\code{trnsLoc}).
#' 
#' @author C. Holbrook \email{cholbrook@usgs.gov} 
#'
#' @examples
#' mypath <- data.frame(x=seq(0,1000,100),y=seq(0,1000,100))
#' mytrns <- transmit_along_path(mypath,vel=0.5,delayRng=c(60,180),burstDur=5.0)
#' plot(mypath,type="b")
#' points(mytrns,pch=20,col="red")
#'
#' @export
transmit_along_path <- function(path = NA, vel = 0.5, delayRng = c(60, 180),
  burstDur = 5.0, EPSG = 3175, sp_out = TRUE){
  
  #CRS
  projargs <- paste0("+init=epsg:", EPSG)
  
  #convert to SpatialPoints if not already
  if(!inherits(path, c("SpatialPointsDataFrame", "SpatialPoints"))){ 
    path <- sp::SpatialPoints(path, proj4string = sp::CRS(projargs))
    projargs_in <- projargs
  } else { 
    projargs_in <- sp::proj4string(path) #get crs to assign output
  }
  
  #convert CRS to EPSG if not needed
  if(!identical(sp::proj4string(path), rgdal::CRSargs(sp::CRS(projargs)))){
    path <- sp::spTransform(path, sp::CRS(projargs))
  }
  
  #cumulative distance travelled in meters 
  path$cumdistm <- c(0,cumsum(sqrt(diff(path$x)^2 + diff(path$y)^2)))
  path$etime <- path$cumdistm/vel #elapse time in seconds
  ntrns <- max(path$etime)/(delayRng[1]+burstDur)
  ints <- runif(ntrns,delayRng[1]+burstDur,delayRng[2]+burstDur)
  ints[1] <- runif(1,0,ints[1]) #draw random the start time
  etime <- cumsum(ints) #elapsed time
  etime <- etime[etime <= max(path$etime)] #subset trans during track duration

  #interpolate transmit locations along track
  trns <- data.frame(
        x = approx(path$etime, path$x, xout=etime)$y,      
        y = approx(path$etime, path$y, xout=etime)$y,
        et = etime)
              
  trns <- sp::SpatialPointsDataFrame(trns[, c("x", "y")], data = trns,
                                       proj4string = sp::CRS(projargs))
  trns <- sp::spTransform(trns, CRSobj = projargs_in)
  
  #convert to input coordinate system 
  if(!sp_out){
    trns <- as.data.frame(cbind(sp::coordinates(trns), et = trns$et))
  }
  
  return(trns)
}
