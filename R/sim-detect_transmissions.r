#' @title Simulate detection of transmitter signals in a receiver network
#'
#' @description Simulates detection of transmitter signals in a receiver network
#'   based on detection range curve (detection probability as a function of
#'   distance), location of transmitter, and location of receivers.
#'
#' @param trnsLoc A data frame with location (two numeric columns) and time
#'   (numeric or POSIXct column) of signal transmissions.\cr \emph{OR} \cr An
#'   object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}} containing
#'   \code{POINT} features (geometry column) and time (see \code{colNames}).
#'   (\code{\link[sp]{SpatialPointsDataFrame}} is also allowed.)
#'
#' @param recLoc A data frame with coordinates (two numeric columns) of receiver
#'   locations.\cr \emph{OR} \cr An object of class \code{\link[sf]{sf}} or
#'   \code{\link[sf]{sfc}} containing a \code{POINT} feature (geometry column)
#'   for each receiver. (\code{\link[sp]{SpatialPointsDataFrame}} is also
#'   allowed.)
#'
#' @param detRngFun A function that defines detection range curve; must accept a
#'   numeric vector of distances (in meters) and return a numeric vector of
#'   detection probabilities at each distance.
#'
#' @param trnsColNames A named list containing the names of columns in
#'   \code{trnsLoc} with coordinates of signal transmissions (defaults are
#'   \code{"x"} and \code{"y"}). Location column names are ignored if
#'   \code{trnsLoc} is a spatial object with a geometry column.
#'
#'
#' @param recColNames A named list containing the names of columns in
#'   \code{recLoc} with coordinates of receiver locations (defaults are
#'   \code{"x"} and \code{"y"}). Location column names are ignored if
#'   \code{recLoc} is a spatial object with a geometry column.
#'
#' @param CRS Defines the coordinate reference system (object of class
#'   \code{crs}) if crs is missing from inputs \code{trnsLoc} or \code{recLoc};
#'   ignored if input \code{trnsLoc} and \code{recLoc} are of class \code{sf},
#'   \code{sfc}, or \code{SpatialPointsDataFrame}). 
#'
#' @param sp_out Logical. If TRUE (default) then output is an \code{sf} object.
#'   If FALSE, then output is a \code{data.frame}.
#'
#' @param show_progress Logical. Progress bar and status messages will be shown
#'   if TRUE (default) and not shown if FALSE.
#'
#' @details Distances between each signal transmission and receiver are
#'   calculated using \code{\link[geodist]{geodist}} (\code{measure =
#'   "haversine"}) if input crs is geographic (i.e., longitude, latitude) and
#'   using simple Euclidean distances if input crs is Cartesian (e.g., UTM). If
#'   crs is missing, the an arbitrary Cartesian coordinate system with base unit
#'   of 1 meter is assumed. Computation time is fastest if coordinates in
#'   \code{path} are are in a Cartesian (projected) coordinate system and
#'   slowest if coordinates are in a geographic coordinate system.
#'
#' @details The probability of detecting each signal on each receiver is
#'   determined from the detection range curve. Detection of each signal on each
#'   receiver is determined stochastically by draws from a Bernoulli
#'   distribution with probability p (detection prob.).
#'
#'   This function was written to be used along with
#'   \code{\link{transmit_along_path}}.
#'
#' @return When \code{sp_out = TRUE}, an \code{sf} object containing one
#'   \code{POINT} feature with coordinates of each receiver location of each
#'   simulated detection and the the following columns: \cr
#'
#'   \item{trns_id}{Unique signal transmission ID.} \item{rec_id}{Unique
#'   receiver ID.} \item{trns_x}{Transmitter x coordinate at time of
#'   transmission.} \item{trns_y}{Transmitter y coordinate at time of
#'   transmission.} \item{time}{Elapsed time.}
#'
#'   \cr \emph{OR} \cr When \code{sp_out = FALSE}, a data.frame with columns
#'   containing coordinates of receiver locations of each simulation detection: 
#'   \item{rec_x}{Receiver x coordinate.} 
#'   \item{rec_y}{Receiver y coordinate.} 
#'   and the columns described above.
#'
#' @seealso \code{\link{transmit_along_path}} to simulate transmissions along a
#'   path (i.e., create \code{trnsLoc}).
#'
#' @author C. Holbrook (cholbrook@@usgs.gov)
#'
#' @examples
#' 
#' #Example 1 - data.frame input (make a simple path in polygon)
#' 
#' mypath <- crw_in_polygon(data.frame(x = c(0, 0, 1000, 1000),
#'                                     y = c(0, 1000, 1000, 0)), 
#'                                     stepLen = 100, nsteps = 50)
#'                                     
#' plot(mypath, type = "l", xlim=c(0,1000), ylim = c(0, 1000))
#' 
#' #add receivers
#' recs <- expand.grid(x = c(250, 750), y = c(250, 750))
#' points(recs, pch = 15, col = "blue")
#' 
#' #simulate tag transmissions
#' mytrns <- transmit_along_path(mypath, vel = 2.0, delayRng = c(60,180),
#'                               burstDur = 5.0)
#' points(sf::st_coordinates(mytrns), pch = 21) #add to plot
#' 
#' #Define detection range function (to pass as detRngFun)
#' # that returns detection probability for given distance
#' # assume logistic form of detection range curve where
#' #   dm = distance in meters
#' #   b = intercept and slope
#' pdrf <- function(dm, b=c(0.5, -1/120)){
#'   p <- 1/(1+exp(-(b[1]+b[2]*dm)))
#'   return(p)
#' }
#' pdrf(c(100,200,300,400,500)) #view detection probs. at some distances
#'
#' #simulate detection
#' mydtc <- detect_transmissions(trnsLoc = mytrns, 
#'                               recLoc = recs, 
#'                               detRngFun = pdrf)
#'                               
#' #view transmissions that were detected
#' sf::st_geometry(mydtc) <- "trns_geometry"
#' 
#' points(sf::st_coordinates(mydtc$trns_geometry), pch = 21, bg = "red")
#'
#' #link transmitter and receiver locations for each detection
#' segments(x0 = sf::st_coordinates(mydtc$trns_geometry)[,"X"], 
#'          y0 = sf::st_coordinates(mydtc$trns_geometry)[,"Y"],
#'          x1 = sf::st_coordinates(mydtc$rec_geometry)[,"X"], 
#'          y1 = sf::st_coordinates(mydtc$rec_geometry)[,"Y"],
#'          col = "red", lwd = 2)
#'
#' @export
detect_transmissions <- function(trnsLoc = NA , 
                                 recLoc = NA, 
                                 detRngFun = NA,
                                 trnsColNames = list(time = "time", 
                                                     x = "x",
                                                     y = "y"),
                                 recColNames = list(x = "x", 
                                                    y = "y"),
                                 CRS = NA, 
                                 sp_out = TRUE, 
                                 show_progress = TRUE){
	 
    # Check input class - trnsLoc
  if(!inherits(trnsLoc, c("data.frame", "sf", "sfc", "SpatialPointsDataFrame"))) 
    stop("Input 'trnsLoc' must be of class 'data.frame', 'sf', 'sfc', ",
         " or 'SpatialPointsDataFrame'.")

  # Check input class - recLoc
  if(!inherits(recLoc, c("data.frame", "sf", "sfc", "SpatialPointsDataFrame"))) 
    stop("Input 'recLoc' must be of class 'data.frame', 'sf', 'sfc', ",
         " or 'SpatialPointsDataFrame'.")
  
  # Get input CRS and use CRS arg if missing
  crs_in <- sf::st_crs(trnsLoc)
  if(is.na(crs_in)) crs_in <- sf::st_crs(CRS)
  
  
  # Check that sf geometry is POINT - trnsLoc
  if(inherits(trnsLoc, c("sf", "sfc"))) {
    if(!("POINT" %in% sf::st_geometry_type(trnsLoc)))
      stop("Input object 'trnsLoc' must contain geometry of type 'POINT' when ",
           "class is 'sf' or 'sfc'.")
    trnsLoc_sf <- trnsLoc
    
  } else if(inherits(trnsLoc, "data.frame")){
    #check for names
    xy_trns_col_names <- unname(unlist(trnsColNames[c("x","y")]))
    if(!all(xy_trns_col_names %in% names(trnsLoc))) {
      stop("Input data.frame 'trnsLoc' must have columns named ",
           "in input 'trnsColNames'.")
    }
    
    #rename cols x and y
    names(trnsLoc)[which(names(trnsLoc) %in% xy_trns_col_names)] <- c("x", "y")
    
    # Coerce to sf
    trnsLoc_sf <- sf::st_as_sf(trnsLoc, coords = c("x", "y"), crs = crs_in)
    
  }
  
  # Check that sf geometry is POINT - recLoc
  if(inherits(recLoc, c("sf", "sfc"))) {
    if(!("POINT" %in% sf::st_geometry_type(recLoc)))
      stop("Input object 'recLoc' must contain geometry of type 'POINT' when ",
           "class is 'sf' or 'sfc'.")
    recLoc_sf <- recLoc
    
  } else if(inherits(recLoc, "data.frame")){
    #check for names
    xy_rec_col_names <- unname(unlist(recColNames[c("x","y")]))
    if(!all(xy_rec_col_names %in% names(recLoc))) {
      stop("Input data.frame 'recLoc' must have columns named ",
           "in input 'recColNames'.")
    }
    
    #rename cols x and y
    names(recLoc)[which(names(recLoc) %in% xy_rec_col_names)] <- c("x", "y")
    
    # Coerce to sf
    recLoc_sf <- sf::st_as_sf(recLoc, coords = c("x", "y"), crs = crs_in)
    
  }  
  
  
  # Convert to sf_point if polyg is SpatialPointsDataFrame
  if(inherits(trnsLoc, "SpatialPointsDataFrame")){
    
    trnsLoc_sf <- sf::st_as_sf(trnsLoc, crs = crs_in)
    
  }
  
  # Convert to sf_point if polyg is SpatialPointsDataFrame
  if(inherits(recLoc, "SpatialPointsDataFrame")){
    
    recLoc_sf <- sf::st_as_sf(recLoc, crs = crs_in)
    
  }
  
  # Check time column names
  time_trns_col_names <- unname(unlist(trnsColNames[c("time")]))
  if(!all(time_trns_col_names %in% names(trnsLoc))) {
    stop("Input data.frame 'trnsLoc' must have columns named ",
         "in input 'trnsColNames'.")
  }
  
  # Rename time col
  names(trnsLoc)[which(names(trnsLoc) %in% xy_trns_col_names)] <- "time"  
  
  
  # Set CRS of recLoc to match trnsLoc
  rec_crs_in <- sf::st_crs(recLoc_sf)
  
  if(rec_crs_in != crs_in) recLoc_sf <- sf::st_transform(recLoc_sf, 
                                                         crs_in)
  
  
  #preallocate detection data frame
  dtc <- data.frame(
        trns_id = NA,
        rec_id = NA,
        rec_x = NA,
        rec_y = NA,
        trns_x = NA,
        trns_y = NA,
        time = NA,
    stringsAsFactors= FALSE)[0,]
	 
	 #loop through receivers (because should be much smaller than transmissions)
	 for(g in 1:nrow(recLoc_sf)){
		#initialize progress bar
	     if(g == 1 & show_progress){
	        pb <- txtProgressBar(min = 0, max = nrow(recLoc_sf), style = 3)
	     }
	  
	    #distance between gth receiver and each transmission
	   
	    distM.g <- sqrt((trnsLoc$x-recLoc$x[g])^2 +
                      (trnsLoc$y-recLoc$y[g])^2)
	    
	    if(isTRUE(sf::st_crs(trnsLoc_sf)$IsGeographic)) {
	      
	      distM.g <- geodist::geodist(sf::st_coordinates(trnsLoc_sf),
	                                  sf::st_coordinates(recLoc_sf[g,]),
	                                  measure = "haversine")    
	      
	    } else {
	      # Euclidean distance if Cartesian
	      distM.g <- sqrt((sf::st_coordinates(trnsLoc_sf)[, "X"] - 
	                         sf::st_coordinates(recLoc_sf)[g, "X"])^2 + 
	                      (sf::st_coordinates(trnsLoc_sf)[, "Y"] - 
	                         sf::st_coordinates(recLoc_sf)[g, "Y"])^2)
	    }
	    
	    # Probability of detection
      detP.g <- detRngFun(distM.g) 
        
      # Simulate detection
      succ.g <- as.logical(rbinom(length(detP.g), 1, detP.g)) 
	 
	    # Output detection data
      if(sum(succ.g) > 0){
  		  dtc.g <- data.frame(
  			  trns_id = which(succ.g),
  			  rec_id = g,
  			  rec_x = sf::st_coordinates(recLoc_sf)[g, "X"],
  			  rec_y = sf::st_coordinates(recLoc_sf)[g, "Y"],
  			  trns_x = sf::st_coordinates(trnsLoc_sf)[succ.g, "X"],
  			  trns_y = sf::st_coordinates(trnsLoc_sf)[succ.g, "Y"],
  			  time = trnsLoc_sf$time[succ.g])
  		 
  		  dtc <- rbind(dtc, dtc.g) #append
  	  } # end if
	 
		#update progress bar
    if(show_progress){
  		info <- sprintf("%d%% done", round(g/nrow(recLoc_sf)*100))
  		setTxtProgressBar(pb, g)
  		if(g==nrow(recLoc_sf)) close(pb)
    }
	 } #end g
  
  dtc <- dtc[order(dtc$time), ]
  
  # Convert to sf with receiver locations
  dtc_sf <- sf::st_as_sf(dtc, coords = c("rec_x", "rec_y"),
                         crs = crs_in)
  
  # Rename geometry column
  dtc_sf <- dplyr::rename(dtc_sf, rec_geometry = geometry)
  
  # Add transmission locations
  trns_geo <- sf::st_as_sf(dtc, coords = c("trns_x", "trns_y"), 
                              crs = crs_in)$geometry

  dtc_sf$trns_geometry <- trns_geo
  
  dtc_sf <- dplyr::select(dtc_sf, -c(trns_x, trns_y))

  if(sp_out) return(dtc_sf)
  
  return(dtc)
}
