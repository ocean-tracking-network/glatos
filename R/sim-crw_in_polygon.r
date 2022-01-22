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
#' @param output_class Specifies class of output object. Possible values are
#'   \code{"sf"} (default), \code{"sp"} (for \code{SpatialPointsDataFrame}),
#'   \code{"df"}, (for \code{data.frame}, or \code{"matrix"}.  If TRUE (default)
#'   then output is a spatial object of \link[sf]{sf}. If FALSE, then output is
#'   a data.frame.
#'
#' @param sp_out DEPRECATED but maintained for backward compatibility. If
#'   specified and \code{sp_out = TRUE}, then \code{output_class} is set to
#'   \code{"sp"}. If specificed and ]code{sp_out = FALSE}, then
#'   ]code{output_class} is set to \code{"df"}.
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
#'   \code{output_class = "sp"}, then \link[sp]{SpatialPoints} output object
#'   will have coordinate system of \code{EPSG}.  Coordinate system on output
#'   will be same as input if polyg object is \code{[sp]{SpatialPolygons}} 
#'   or \code{[sf]{sf}}.
#'
#' 
#' @return A \link[sf]{sf}, \link[sp]{SpatialPoints}, \code{data.frame}, or 
#' \code{matrix} object in the same CRS as the input 
#' \code{polyg} object. 
#' \cr \emph{OR} \cr 
#' A two-column data frame containing:
#' \item{x}{x coordinates}
#' \item{y}{y coordinates}
#' in the same units as \code{polyg}.
#' \cr See argument \code{output_class}.
#'
#' @author C. Holbrook \email{cholbrook@@usgs.gov}
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
  EPSG = 3175, output_class = "sf", sp_out = TRUE, show_progress = TRUE){          
  
  #coerce to sf_polygon if polyg is data.frame
  if(inherits(polyg, "data.frame")){
    
    #check for names
    if(!all(c("x", "y") %in% names(polyg))) stop("Input data.frame 'polyg' ",
                                                 "must have columns named ",
                                                 "'x' and 'y'.")
    
    #close polyg if needed
    if(!identical(polyg[1,], tail(polyg, 1))) polyg <- rbind(polyg, polyg[1,])
    
    polyg_sf <- sf::st_polygon(list(as.matrix(polyg[c("x","y")])))
    polyg_sf <- sf::st_sfc(polyg_sf, crs = EPSG)
    polyg_sf <- sf::st_sf(ID=1:length(polyg_sf), geom = polyg_sf)
  }
  
  
  #convert to sf_polygon if polyg is SpatialPolygonsDataFrame or SpatialPolygons
  if(inherits(polyg, c("SpatialPolygonsDataFrame", "SpatialPolygons"))){

    polyg_sf <- sf::st_as_sf(polyg)
    
    #set CRS
    polyg_sf <- sf::st_transform(polyg_sf, crs = EPSG)
  }
  
  #if any initPos were not given
  #randomly select one point in the study area
  if(any(is.na(initPos))){
    inPoly <- FALSE #logical flag; preallocate
    while(inPoly == FALSE){
      init <- c(runif(1, floor(sf::st_bbox(polyg_sf)["xmin"]),
                      ceiling(sf::st_bbox(polyg_sf)["xmax"])),
                runif(1, floor(sf::st_bbox(polyg_sf)["ymin"]),
                      ceiling(sf::st_bbox(polyg_sf)["ymax"])))
      init <- sf::st_as_sf(data.frame(x=init[1], y = init[2]),
                           coords = c("x", "y"),
                           crs = EPSG)
      inPoly <- any(sf::st_contains(polyg_sf, init, sparse = FALSE))
    } #end while
  } #end if
  
  
  #if initPos are both given, check to see if in polyg
  if(all(!is.na(initPos))) {
    init_crs <- sf::st_crs(polyg)
    if(is.na(init_crs)) init_crs <- sf::st_crs(EPSG)
    init <- sf::st_as_sf(data.frame(x=initPos[1], y = initPos[2]),
                         coords = c("x", "y"),
                         crs = init_crs)
    init <- sf::st_transform(init, crs = sf::st_crs(EPSG))
    inPoly <- any(sf::st_contains(polyg_sf, sparse = FALSE))
    if(!inPoly) stop("initPos is outside polygon boundary.")
  } #end if   
  
  
  #randomly select heading if not given
  if(is.na(initHeading)) initHeading <- runif(1, 0, 360)
  
  path_fwd <- data.frame(x = rep(NA, nsteps + 1), y = NA) #preallocate
  path_fwd[1,] <- sf::st_coordinates(init)
  
  #create lines object from polyg (for distance measurement)
  xl <- sf::st_cast(polyg_sf, "MULTILINESTRING")

  rows_i <- 1
  init_i <- init
  dist_i <- min(sf::st_distance(init_i, xl, sparse = TRUE), na.rm = TRUE)
  nsteps_i <- (as.numeric(dist_i) %/% stepLen) + 1
  rows_i <- 1 + 1:nsteps_i 
  rows_i <- rows_i[rows_i <= (nsteps + 1)]  
  
  #initalize counter for boundary failures
  k <- 0

  #initialize progress bar
  if(show_progress) {
    message("Simulating tracks...")
    pb <- txtProgressBar(min = 0, max = nsteps + 2, initial = 0, style = 3)	  
  }
  
  while(length(rows_i) > 0){

    #calculate theta based on k (failed boundary attempts)
    theta_i <- c(theta[1], theta[2] * (1 + 0.1 * k^2))
    
    #operate on temporary object for ith window
    path_fwd_i <- crw(theta = theta_i, stepLen = stepLen, 
                      initPos = as.vector(sf::st_coordinates(init_i)),
                      initHeading, nsteps = length(rows_i))
    
    inPoly <- check_in_polygon(path_fwd_i, polyg_sf, EPSG)
    
    if(all(!inPoly)) {
      k <- k + 1 #counter
      next #repeat this iteration if all outside polygon
    }
    if(any(!inPoly)) rows_i <- rows_i[inPoly] #retain only rows inside
    
    k <- 0
    
    #update path.fwd
    path_fwd[rows_i , ] <- path_fwd_i[inPoly, ]
    
    
    #simulate track forward
    init_i <- sf::st_as_sf(path_fwd[max(rows_i), ], 
                           coords = c("x", "y"),
                           crs = EPSG)
    
    #smallest distance to boundary
    dist_i <- min(sf::st_distance(init_i, xl, sparse = TRUE), na.rm = TRUE)
    
    #calculate heading at end (start of next)
    initHeading <- vector_heading(path_fwd$x[max(rows_i) - 1:0],
                                  path_fwd$y[max(rows_i) - 1:0]) 
    

    #update progress bar
    if(show_progress){
      setTxtProgressBar(pb, max(rows_i))
      if(max(rows_i) > (nsteps + 1)) close(pb)		
    }

    #conservative estimate of the number of rows/steps to simulate
    #i.e., without reaching barrier
    nsteps_i <- (as.numeric(dist_i) %/% stepLen) + 1
    rows_i <- max(rows_i) + 1:nsteps_i 
    rows_i <- rows_i[rows_i < nsteps + 2]    
    
  } #end while
  
  if(show_progress) message("Done.")
  
  
  #convert to input coordinate system 
  #path_fwd_sp <- sp::SpatialPoints(path_fwd, proj4string = sp::CRS(projargs))
  #path_fwd_sp <- sp::spTransform(path_fwd_sp, CRSobj = projargs_in)
  
  #for backward compatibility, set output_class based on sp_out if given
  if(!missing(sp_out) & missing(output_class)){
     if(sp_out) output_class <- "sp"
     if(!sp_out) output_class <- "df"
  }
  
  
  #set output CRS to polygon CRS if output is spatial object; EPSG otherwise
  if(!is.na(sf::st_crs(polyg))) { 
    crs_out <- sf::st_crs(polyg) } else { crs_out <- sf::st_crs(EPSG) }

  #coerce to sf in output crs
  path_fwd_sf <- sf::st_as_sf(path_fwd, coords = c("x", "y"), 
                              crs = EPSG)
  path_fwd_sf <- sf::st_transform(path_fwd_sf, crs = crs_out)
  
  if(output_class == "sf") path_out <- path_fwd_sf
  
  if(output_class == "df") 
    path_out <- as.data.frame(sf::st_coordinates(path_fwd_sf))
  
  if(output_class == "matrix") 
    path_out <- sf::st_coordinates(path_fwd_sf)
  
  if(output_class == "sp")
    path_out <- sf::as_Spatial(path_fwd_sf)

  
  return(path_out)
} #end
 
#check if in polygon
check_in_polygon <- function(points, polygon, EPSG){
  points_sf <- sf::st_as_sf(points,
                            coords = c("x", "y"),
                            crs = EPSG)
  #identify points contains in any polygon
  inPoly <- apply(sf::st_contains(polygon, points_sf, sparse = FALSE), 2 , 
                  any)
  return(inPoly)
} 
 
