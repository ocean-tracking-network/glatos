#' Simulate a correlated random walk inside a polygon
#' 
#' Uses \link{crw} to simulate a random walk as series of equal-length steps 
#'   with turning angles drawn from a normal distribution inside a polygon.
#'
#' @param polyg A \code{\link[sp]{SpatialPolygons}} object.
#' \cr \emph{OR} \cr
#' A polygon defined as data frame with numeric columns x and y.
#' 
#' @param theta A 2-element numeric vector with turn angle parameters 
#'   (theta[1] = mean; theta[2] = sd) from normal distribution.
#'   
#' @param stepLen A numeric scalar with total distance moved in each step. 
#'  Units are same as the units of the coordinate reference system specified 
#'  by argument \code{EPSG} (meters for the default Great Lakes projected 
#'  coordinate system).
#'   
#'
#' @param initPos A 2-element numeric vector with initial position
#'   (initPos[1]=x, initPos[2]=y) in same units as \code{polyg}.
#'   
#' @param initHeading A numeric scalar with initial heading in degrees.
#' 
#' @param nsteps A numeric scalar with number of steps to simulate.
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
#' @param show_progress Logical. Progress bar and status messages will be 
#'  shown if TRUE (default) and not shown if FALSE.
#'   
#' @details If initPos = NA, then a starting point is randomly
#'   selected within the polygon boundary. A path is simulated forward
#'   using the crw function.  Initial heading is also randomly
#'   selected if initHeading = NA. When a step crosses the polygon
#'   boundary, a new heading for that step is drawn and the turn angle
#'   standard deviation is enlarged slightly for each subsequent point
#'   that lands outside the polygon.
#'
#' @details If polyg object is a data frame with x and y columns and
#'   \code{sp_out} argument is TRUE, then \link[sp]{SpatialPoints} output object
#'   will have coordinate system of \code{EPSG}.  Coordinate system on output
#'   will be same as input if polyg object is \code{[sp]{SpatialPolygons}}.
#'
#' 
#' @return A \link[sp]{SpatialPoints} object in the same CRS as the input 
#' \code{polyg} object. 
#' \cr \emph{OR} \cr 
#' A two-column data frame containing:
#' \item{x}{x coordinates}
#' \item{y}{y coordinates}
#' in the same units as \code{polyg}.
#' \cr See argument \code{sp_out}.
#'
#' @author C. Holbrook \email{cholbrook@usgs.gov}
#' 
#' @seealso \link{crw}
#'
#' @note 
#' The path is constructed in segments based on the minimum distance between 
#' the previous point and the closest polygon boundary.
#' 
#' Simulations are conducted within the coordinate system specified by 
#' argument \code{EPSG}. The default EPSG (3175), covers only the Great Lakes
#' of North America. Simulations conducted in other areas will need to specify 
#' a valid EPSG for the study area.
#'
#' @examples
#' 
#' #Simple box example
#' mypolygon <- data.frame(x = c(-50,-50, 50, 50), y = c(-50,50,50,-50))
#' foo <- crw_in_polygon(mypolygon, theta = c(0, 20), stepLen = 10, 
#'   initPos=c(0,0), initHeading=0, nsteps=50)
#' class(foo) #note object is SpatialPoints
#' plot(sp::coordinates(foo), type = "o", pch = 20, asp = c(1,1), 
#'   xlim = range(mypolygon$x), ylim = range(mypolygon$y))
#' polygon(mypolygon, border = "red")
#' 
#' 
#' #Great Lakes Example
#' data(greatLakesPoly)
#' 
#' #simulate in great lakes polygon
#' foo2 <- crw_in_polygon(greatLakesPoly,theta=c(0,25), stepLen=10000,
#'   initHeading=0, nsteps=100, sp_out = TRUE)
#' 
#' #plot
#' sp::plot(greatLakesPoly, col = "lightgrey", border = "grey")
#' points(foo2,type="o", pch = 20, col = "red")
#' 
#' #zoom in
#' sp::plot(greatLakesPoly, col = "lightgrey", border = "grey", 
#'   xlim = sp::bbox(foo2)[1,], ylim = sp::bbox(foo2)[2,])
#' points(foo2,type="o", pch = 20, col = "red")
#'
#' @export

crw_in_polygon <- function(polyg, theta = c(0,10), stepLen = 100, 
  initPos = c(NA,NA), initHeading = NA, nsteps = 30, 
  EPSG = 3175, sp_out = TRUE, show_progress = TRUE){          
  
  #convert to Polygon if not already
  if(!inherits(polyg, c("SpatialPolygonsDataFrame", "SpatialPolygons", 
    "Polygons", "Polygon"))) polyg <- sp::Polygon(polyg, 
      hole = FALSE)
  
  #convert to Polygons if not already
  if(!inherits(polyg, c("SpatialPolygonsDataFrame", "SpatialPolygons", 
    "Polygons"))) polyg <- sp::Polygons(list(polyg), ID = 1)
  
  #CRS
  projargs <- paste0("+init=epsg:", EPSG)
  
  #convert to SpatialPolygons if not already
  if(!inherits(polyg, c("SpatialPolygonsDataFrame", "SpatialPolygons"))){ 
    polyg <- sp::SpatialPolygons(list(polyg), proj4string = sp::CRS(projargs))
    projargs_in <- projargs
  } else { 
    projargs_in <- sp::proj4string(polyg) #get crs to assign output
  }
  
  #convert CRS to EPSG if not needed
  if(!identical(sp::proj4string(polyg), rgdal::CRSargs(sp::CRS(projargs)))){
    polyg <- sp::spTransform(polyg, sp::CRS(projargs))
  }
  
  #if any initPos were not given
  #randomly select one point in the study area
  if(any(is.na(initPos))){
    inPoly <- FALSE #logical flag; preallocate
    while(inPoly == FALSE){
      init <- c(runif(1, floor(sp::bbox(polyg)["x", "min"]),
        ceiling(sp::bbox(polyg)["x", "max"])),
        runif(1, floor(sp::bbox(polyg)["y", "min"]),
          ceiling(sp::bbox(polyg)["y", "max"])))
      init <- sp::SpatialPoints(matrix(as.numeric(init), nrow = 1), 
        proj4string = sp::CRS(sp::proj4string(polyg)))
      inPoly <- rgeos::gDistance(polyg, init) == 0
      inPoly <- switch(inPoly+1,FALSE,TRUE,FALSE,FALSE)
    } #end while
  } #end if
  
  
  #if initPos are both given, check to see if in polyg
  if(all(!is.na(initPos))) {
    init <-  sp::SpatialPoints(matrix(as.numeric(initPos), nrow = 1), 
      proj4string = sp::CRS(projargs_in)) 
    init <-sp::spTransform(init, sp::CRS(projargs))
    inPoly <- rgeos::gDistance(polyg, init) == 0
    if(!inPoly) stop("initPos is outside polygon boundary.")
  } #end if   
  
  
  #randomly select heading if not given
  if(is.na(initHeading)) initHeading <- runif(1,0,360)
  
  path.fwd <- data.frame(x = rep(NA, nsteps + 1), y = NA) #preallocate
  path.fwd[1,] <- sp::coordinates(init)
  
  #create lines object from polyg (for distance measurement)
  xl <- methods::as(polyg, "SpatialLines") 
  
  rows_i <- 1
  init_i <- init
  dist_i <- rgeos::gDistance(init_i, xl) #smallest distance to boundary
  nsteps_i <- (dist_i %/% stepLen) + 1
  rows_i <- 1 + 1:nsteps_i 
  rows_i <- rows_i[rows_i <= (nsteps + 1)]  
  
  #initalize counter for boundary failures
  k <- 0

  #initialize progress bar
  if(show_progress) {
    message("Simulating tracks...")
    pb <- txtProgressBar(min = 0, max = nsteps, initial = 0, style = 3)	  
  }
  
  while(max(rows_i) <= (nsteps + 1)){

    #calculate theta based on k (failed boundary attempts)
    theta_i <- c(theta[1], theta[2] * (1 + 0.1 * k^2))
    
    #operate on temporary object for ith window
    path.fwd.i <- crw(theta = theta_i, stepLen = stepLen, 
      initPos = as.vector(sp::coordinates(init_i)),
      initHeading, nsteps = length(rows_i))
    
    #check if in polygon
    check_in_polygon <- function(points, polygon){
      points_sp <- sp::SpatialPoints(as.matrix(points), 
        proj4string = sp::CRS(sp::proj4string(polygon)))
      inPoly <- sapply(1:nrow(sp::coordinates(points)),function(i)
        rgeos::gDistance(polygon, points_sp[i,]) == 0)
      return(inPoly)
    }
    
    inPoly <- check_in_polygon(path.fwd.i, polyg)
    if(all(!inPoly)) {
      k <- k + 1 #counter
      next #repeat this iteration if all outside polygon
    }
    if(any(!inPoly)) rows_i <- rows_i[inPoly] #retain only rows inside
    
    k <- 0
    
    #update path.fwd
    path.fwd[rows_i , ] <- path.fwd.i[inPoly, ]
    
    
    #simulate track forward
    init_i <- sp::SpatialPoints(path.fwd[max(rows_i), ], 
      proj4string = sp::CRS(projargs))
    dist_i <- rgeos::gDistance(init_i, xl) #smallest distance to boundary
    
    #calculate heading at end (start of next)
    initHeading <- vector_heading(path.fwd$x[max(rows_i) - 1:0],
      path.fwd$y[max(rows_i) - 1:0]) 
    
    #conservative estimate of the number of rows/steps to simulate
    #i.e., without reaching barrier
    nsteps_i <- (dist_i %/% stepLen) + 1
    rows_i <- max(rows_i) + 1:nsteps_i 
    rows_i <- min(rows_i[rows_i <= (nsteps + 1)], nsteps + 2)
    
    
    #update progress bar
    if(show_progress){
      setTxtProgressBar(pb, max(rows_i))
      if(max(rows_i) > (nsteps + 1)) close(pb)		
    }
  } #end while
  
  if(show_progress) message("Done.")
  
  
  #convert to input coordinate system 
  path.fwd.sp <- sp::SpatialPoints(path.fwd, proj4string = sp::CRS(projargs))
  path.fwd.sp <- sp::spTransform(path.fwd.sp, CRSobj = projargs_in)
  
  if(!sp_out) path.fwd.sp <- as.data.frame(sp::coordinates(path.fwd.sp))
  
  return(path.fwd.sp)
} #end
 
 
 
