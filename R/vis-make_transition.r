#' Create transition layer from spatial object.
#'
#' Create transition layer for [interpolate_path()] spatial object.
#'
#' @param poly A spatial polygon object of class
#'   [SpatialPolygonsDataFrame][SpatialPolygons] or a [sf::sf()][sf::sf] object
#'   with a geometry column of polygon and/or multipolygon objects.
#'
#' @param res two element vector that specifies the x and y dimension of output
#'   raster cells. Units are same as `poly` crs. May be calculated from desired
#'   resolution in meters using [scale_meters_to_degrees()].
#'
#' @param receiver_points Object containing coordinates of receiver locations.
#'   Must be of class `SpatialPointsDataFrame`, `SpatialPoints`, `sf`,
#'   `glatos_receivers`, or `glatos_detections`.
#'
#' @param epsg coordinate reference code that describes projection used for
#'   `res` and `buffer`.  Defaults to NAD83/Great Lakes and St. Lawrence Albers.
#'
#' @param buffer Buffer, in same units as `epsg`, that will be added to `poly`
#'   before rasterization.
#'
#' @details `make_transition` uses [jarasterize()] to convert a polygon
#'   shapefile into a raster layer and geo-corrected transition layer
#'   [interpolate_path()]. Raster cell values on land equal 0 cells in water
#'   equal 1. Output is a two-object list containing the raster layer and
#'   transition layer.  Both objects have the same (or slightly larger; within
#'   `res`) extents and geographic projection as input polygon.
#'
#' @details If receiver_points is provided, any receiver not in water is
#'   buffered by the distance from the receiver to the nearest water.  This
#'   allows all receivers to be coded as in water if the receiver is on land.
#'
#' @details Poly object is transformed into planer map projection specified by
#'   epsg argument for calculation of transition object if receiver_points is
#'   provided.  Output is projected to WGS84 (epsg 4326).
#'
#' @details output transition layer is corrected for projection distortions
#'   using `gdistance::geoCorrection`.  Adjacent cells are connected by 16
#'   directions and transition function returns 0 (land) for movements between
#'   land and water and 1 for all over-water movements.
#'
#' @details Note that this function underwent breaking changes between 0.7.3 and
#'   0.8.0 (uses `jasterize` instead of `gdalUtilities::gdal_rasterize` see
#'   NEWS). The functions previously named `make_transition2` and
#'   `make_transition3` have also been removed starting with 0.8.0.
#'
#' @return A list with two elements:
#' \describe{
#'    \item{transition}{a geo-corrected transition raster layer where land = 0
#'       and water = 1
#'   (see [`gdistance::Transition-class`])}
#'    \item{rast}{rasterized input layer of class `raster`}}
#'
#'
#' @author Todd Hayden, Chris Holbrook
#'
#' @examples
#'
#' # Example 1 - read from sf polygon object
#' # use example polygon for Great lakes
#'
#' # calculate resolution in degrees (from meters)
#' #  note this applies to cell at center, cell sizes will vary
#' res <- scale_meters_to_degrees(5000, sf = great_lakes_polygon)
#'
#' # make_transition layer
#' tst <- make_transition(great_lakes_polygon, res = res)
#'
#' \dontrun{
#' # plot raster layer (notice water = 1, land = 0)
#' raster::plot(tst$rast)
#'
#' # compare to polygon
#' plot(sf::st_geometry(great_lakes_polygon), add = TRUE)
#' }
#' 
#' # Example 2 - add 1 km buffer (same resolution)
#' 
#' # make_transition layer
#' tst2 <- make_transition(great_lakes_polygon, 
#'                        res = res,
#'                        buffer = 3000)
#'
#' \dontrun{
#' # plot raster layer (notice water = 1, land = 0)
#' raster::plot(tst2$rast)
#'
#' # compare to polygon
#' plot(sf::st_geometry(great_lakes_polygon), add = TRUE)
#' }
#'
#' # Example 3 - read from ESRI Shapefile and include receiver file
#' # to account for any receivers outside of great lakes polygon
#'
#' # path to polygon shapefile
#' poly <- system.file("extdata", "shoreline.zip", package = "glatos")
#' poly <- sf::st_read(paste0("/vsizip/", poly))
#'
#' # read in glatos receivers object
#' rec_file <- system.file("extdata", "sample_receivers.csv",
#'   package = "glatos")
#' recs <- read_glatos_receivers(rec_file)
#'
#' # change a coordinate to on-land to show impact...
#' recs[1, "deploy_lat"] <- recs[1, "deploy_lat"] + 4
#'
#' # make_transition layer (roughly 500 m res)
#' tst <- make_transition(poly, res = c(0.065, 0.046), receiver_points = recs)
#'
#' \dontrun{
#' # plot raster layer
#' # notice the huge circle rasterized as "water"  north of Lake Superior.
#' # This occurred because we had a "receiver" deployed at that locations
#' raster::plot(tst$rast)
#' points(recs$deploy_long, recs$deploy_lat, col = "red", pch = 20)
#'
#' # plot transition layer
#' raster::plot(raster::raster(tst$transition))
#' }
#' 
#' # Example 4- transition layer of Lake Huron only with receivers
#'
#' # transform to great lakes projection
#' poly <- sf::st_transform(great_lakes_polygon, crs = 3175)
#'
#' # set attribute-geometry relationship to constant.
#' # this avoids error when cropping
#' sf::st_agr(poly) <- "constant"
#'
#' # crop Great lakes polygon file
#' poly <- sf::st_crop(
#'   x = poly, xmin = 829242.55, ymin = 698928.27,
#'   xmax = 1270000.97, ymax = 1097196.15
#' )
#'
#' # read in glatos receivers object
#' rec_file <- system.file("extdata", "sample_receivers.csv",
#'   package = "glatos")
#' recs <- read_glatos_receivers(rec_file)
#'
#' # extract receivers in "HECWL" project
#' # all receiver stations except one is in Lake Huron
#' recs <- recs[recs$glatos_project == "HECWL", ]
#'
#' # remove two stations not in Lake Huron
#' recs <- recs[!recs$glatos_array %in% c("MAU", "LVD"), ]
#'
#' # convert recs to simple feature  object (sf)
#' recs <- sf::st_as_sf(recs, coords = c("deploy_long", "deploy_lat"),
#'   crs = 4326)
#'
#' # transform receivers to same projection as great lakes polygon
#' recs <- sf::st_transform(recs, crs = 3175)
#'
#' \dontrun{
#' # check by plotting
#' plot(sf::st_geometry(poly), col = NA)
#' plot(sf::st_geometry(recs), col = "red", add = TRUE)
#' } 
#' 
#' # create slightly higher resolution transition layer
#' #   (note that res in in meters here because crs is 3175)
#' tst1 <- make_transition(poly, res = 5000, receiver_points = recs)
#'
#' \dontrun{
#' # plot raster layer
#' raster::plot(tst1$rast)
#' 
#' plot(sf::st_geometry(recs),
#'   add = TRUE, col = "red", pch = 20
#' )
#'
#' # plot transition layer
#' raster::plot(raster::raster(tst1$transition))
#' }
#' 
#' @export
make_transition <- function(poly, 
                            res, 
                            receiver_points = NULL, 
                            epsg = 3175,
                            buffer = NULL) {
  
  if (inherits(poly, 
               c("SpatialPolygonsDataFrame", 
                 "SpatialPolygons", 
                 "sf")) == FALSE) {
    stop(paste0(
      "Supplied object for 'poly' argument is not class ",
      "SpatialPolygonsDataFrame, SpatialPolygons, or a sf polygon object"
    ), call. = FALSE)
  }

  if (inherits(poly, 
               c("SpatialPolygonsDataFrame", 
                 "SpatialPolygons")) == TRUE) {
    poly <- sf::st_as_sf(poly)
  }
  
  if(length(res) == 1) res <- rep(res, 2)
  
  # Convert to projected crs
  poly_proj <- sf::st_transform(poly, crs = epsg, agr = "constant")
  
  # combine elements
  poly_proj <- sf::st_union(poly_proj)
  

  # Buffer polygon if buffer is NULL (default) or > 0
  if(is.null(buffer) == FALSE) {
    
    if(buffer < 0) stop("Input `buffer` must be a positive number or NULL.")
    
    # calculate buffer around points not in polygon.
    poly_proj <- sf::st_buffer(poly_proj, buffer)
    
  }  
  

  # If receiver_points is specified
  if (is.null(receiver_points) == FALSE) {
    if (inherits(receiver_points, 
                 c("glatos_receivers", "glatos_detections"))) {
      receiver_points <- sf::st_as_sf(receiver_points,
                                      coords = c("deploy_long", "deploy_lat"),
                                      crs = sf::st_crs(poly), 
                                      agr = "identity"
      )
    }

    if(inherits(receiver_points, 
                 c("SpatialPoints", "SpatialPointsDataFrame"))) {
      receiver_points <- sf::st_as_sf(receiver_points)
    }

    if (inherits(receiver_points, c(
      "SpatialPointsDataFrame",
      "SpatialPoints", 
      "sf",
      "glatos_receivers",
      "glatos_detections"
    )) == FALSE) {
      stop("Supplied object for 'receiver_points' argument is not class ", 
           "SpatialPolygonsDataFrame, SpatialPolygons, sf polygon object, ",
           "or glatos_receiver, glatos_detections", 
           call. = FALSE)
    }

    # convert to GL projection
    recs_gl <- sf::st_transform(receiver_points, crs = epsg)


   # check if any receiver points are on land
    recs_in_poly  <- sf::st_intersects(sf::st_geometry(recs_gl),
                                       sf::st_geometry(poly_proj))
    
    recs_in_poly <- lengths(recs_in_poly) > 0
    
    # determine shortest distance from receiver to water polygon
    recs_gl$rec_water_dist <- 0
    recs_gl$rec_water_dist[!recs_in_poly] <- 
      apply(sf::st_distance(recs_gl[!recs_in_poly,], 
                            poly_proj),
            1,
            "min")


    # extract rec_water_dist > 0
    land <- recs_gl[recs_gl$rec_water_dist > 0, ]

    # calculate buffer around points not in polygon.
    land <- sf::st_buffer(land, land$rec_water_dist * 1.1)

    land <- sf::st_union(land)
    
    # union buffered points and land
    poly_proj <- sf::st_union(poly_proj, 
                              land)

  }
  
  # convert back to CRS 4326 (wgs84 lat/lon)
  poly <- sf::st_transform(poly_proj, crs = sf::st_crs(poly))


  # rasterize
  burned <- jarasterize(poly,
                        res = res)

  message("Making transition layer...")
  
  t0 <- Sys.time()

  # function for transition matrix
  tran <- function(x) {
    if (x[1] * x[2] == 0) {
      return(0)
    } else {
      return(1)
    }
  }

  # calculate distances
  tr1 <- gdistance::transition(burned,
    transitionFunction = tran,
    directions = 16
  )

  tr1 <- gdistance::geoCorrection(tr1, type = "c")

  
  et <- Sys.time() - t0
  
  message("Done (", round(et, 1)," ", units(et), ")")
          
  out <- list(transition = tr1, 
              rast = burned)
          
  return(out)
}

#' Get degree-scale equivalent of meter-scale distance on a spatial object
#'
#' Get degree-scale equivalent of meter-scale distance on an `sf` object.
#'
#' @param x Distance, in meters (or units of `epsg`), to be converted to degrees
#'   along each dimension of `sf`. May be a two element vector (denoting
#'   distance along x and y axes, respectively) or a single number (same
#'   distance along both axes).
#'
#' @param sf An `sf` spatial object.
#'
#' @param ref Reference point for scale. If `center` (default), then returned
#'   value is the distance (in degrees), along each dimension, at the center of
#'   `sf` that is equivalent to `x`. Other possible values are `min` and `max`.
#'
#' @param epsg Code denoting coordinate reference system in which calculations
#'   are made. Must be a Cartesian projection.
#'
#' @details A helper function to determine input resolution to
#'   [make_transition()].
#'
#' @returns A two-element vector with distance in x and y (longitude and
#'   latitude) dimensions, respectively.
#'
#' @examples
#'
#' # how many long, lat equal to 5000 m at center of great_lakes_polygon?
#' scale_meters_to_degrees(x = 5000,
#'                         sf = great_lakes_polygon)
#'
#' # how many long, lat equal to 5000 m at top of great_lakes_polygon?
#' scale_meters_to_degrees(x = 5000,
#'                         sf = great_lakes_polygon,
#'                         ref = "max")
#'
#' # how many long, lat equal to 5000 m at bottom of great_lakes_polygon?
#' scale_meters_to_degrees(x = 5000,
#'                         sf = great_lakes_polygon,
#'                         ref = "min")
#'
#' @export
scale_meters_to_degrees <- function(x,
                                    sf,
                                    ref = "center",
                                    epsg = 3175) {
  
  if(length(x) == 1) x <- rep(x, 2)
  
  # Convert to projected crs
  sf_proj <- sf::st_transform(sf, crs = epsg)
  
  if(inherits(sf_proj, "sf")) sf::st_agr(sf_proj) <- "constant"
  
  # combine elements
  sf_proj <- sf::st_union(sf_proj)
  
  sf_proj_bbox <- sf::st_bbox(sf_proj)
  
  if(ref == "center"){
    sf_proj_ref <- as.numeric(
      c(median(sf_proj_bbox[c("xmin", "xmax")]), 
        median(sf_proj_bbox[c("ymin", "ymax")])))
  } else if(ref == "min"){
    sf_proj_ref <- as.numeric(
      c(median(sf_proj_bbox[c("xmin", "xmax")]), 
               sf_proj_bbox["ymin"]))
  } else if(ref == "max"){
    sf_proj_ref <- as.numeric(
      c(median(sf_proj_bbox[c("xmin", "xmax")]), 
               sf_proj_bbox["ymax"]))
  } else { stop("Input values for 'ref' must be 'center', 'min', or 'max'.") }
  
  
  # calculate new point "res" m away in x and y dim
  ref_bbox_proj <- sf::st_bbox(
    c(xmin = sf_proj_ref[1] - 0.5 * x[1], 
      xmax = sf_proj_ref[1] + 0.5 * x[1],
      ymin = sf_proj_ref[2] - 0.5 * x[2], 
      ymax = sf_proj_ref[2] + 0.5 * x[2]),
    crs = sf::st_crs(epsg)
  )
  
  # convert back to WGS84 and calculate diffences in degrees lon, lat
  ref_inputcrs <- 
    sf::st_bbox(
      sf::st_transform(
        sf::st_as_sfc(ref_bbox_proj), crs = sf::st_crs(sf)))
  
  # resolution at center in degrees
  ref_input_units <- 
    as.numeric(
      c(diff(ref_inputcrs[c("xmin", "xmax")]), 
        diff(ref_inputcrs[c("ymin", "ymax")])))
 
  return(ref_input_units) 
}


#' Just another rasterizer
#' 
#' An 'all-touched-capable' rasterizer that depends only on `sf` and `raster`.
#' 
#' @param x An `sf` object (polygon, lines, or points).
#' 
#' @param res Resolution (raster cell size) in units of `x`'s `crs`. May be 
#' either a 2-element vector (for lon and lat, respectively) or a single value
#' (same value used for each dimension).
#' 
#' @param value The value assigned to cells matching `x`.
#'
#' @param bg_value The value assigned to cells not matching `x`.
#'
#' @param all_touched If `TRUE` (default), raster will return `value` for every
#'   cell touched by polygon (and `bg_value` for all others); otherwise, raster
#'   will return `value` for all cells whose center points are within the
#'   polygon.
#'   
#' @param silent If false (default), progress messages are not displayed.
#'
#' @returns A `raster::rasterLayer` object.
#' 
#' @examples
#' 
#' # Example 1. lon lat WGS input
#' 
#' poly1 <- great_lakes_polygon
#' 
#' plot(sf::st_geometry(poly1))
#' 
#' rast1 <- jarasterize(poly1, res = c(0.1, 0.05))
#' 
#' \dontrun{
#' # compare to polygon
#' x11(width = 12, height = 8)
#' raster::plot(rast1) 
#' plot(sf::st_geometry(poly1), add = TRUE)
#' }
#'  
#' # Example 2. projected input; 5 km cell size
#' 
#' poly2 <- sf::st_transform(poly1, crs = 3175)
#' 
#' rast2 <- jarasterize(poly2, res = 5000)
#' 
#' \dontrun{
#' # compare to polygon
#' x11(width = 12, height = 8)
#' raster::plot(rast2) 
#' plot(sf::st_geometry(poly2), add = TRUE)
#' }
#' 
#' # Example 3. projected input; 5 km cell size; all_touched = FALSE
#'
#' rast3 <- jarasterize(poly2, res = 5000, all_touched = FALSE)
#' 
#' \dontrun{
#' # compare to polygon
#' x11(width = 12, height = 8)
#' raster::plot(rast3) 
#' plot(sf::st_geometry(poly2), add = TRUE)
#' }
#' 
#' @export
jarasterize <- function(x, 
                        res,
                        value = 1,
                        bg_value = 0,
                        all_touched = TRUE,
                        silent = FALSE){
            
  ##  Declare global variables for NSE & R CMD check
  lat_id <- line_type <- lon_id <- rast_cell <- x1 <- x1cell <- x2 <- 
    x2cell <- x_cell <- y1 <- y1cell <- y2 <- y2cell <- y_cell <- NULL
  
  if(silent == FALSE) message("Rasterizing...")
  
  t0 <- Sys.time()
  
  # get input crs
  crs_in <- sf::st_crs(x)
  
  # make raster
  rast <- raster::raster(resolution = res, 
                         ext = raster::extent(sf::st_bbox(x)))
  
  # get unique values along each dimension (define edges of each raster cell)
  rast_extent <- raster::extent(rast)

  lon_vals <- seq(rast_extent@xmin, rast_extent@xmax, by = raster::res(rast)[1])
    
  lat_vals <- seq(rast_extent@ymin, rast_extent@ymax, by = raster::res(rast)[2])
  
    
  # If all_touched is false, check if each centerpoint is touched by the polygon
  if(all_touched == FALSE){
    
    # take about 11 secs for 1.4 million centerpoints
    centerpoints <- raster::coordinates(rast)
    
    centerpoints <- 
      sf::st_as_sf(
        as.data.frame(centerpoints),
        coords = c("x", "y"),
        crs = crs_in
      )
    
    # Preallocate raster cell values
    centerpoints$value <- rep(bg_value, length(centerpoints))
    
    cp_in_x <- sf::st_intersects(sf::st_geometry(centerpoints), 
                                 sf::st_geometry(x))
    
    centerpoints$value[lengths(cp_in_x) > 0] <- value

    # Set raster values
    rast <- raster::setValues(rast, centerpoints$value)
    
  } else {
  
    # if all_touched is true, find each raster cell edge touched by rast
    
    # range of edge lines along each dimension
    lon_rng <- range(lon_vals)
    lat_rng <- range(lat_vals)
    
    
    # Lines of longitude
    lon_lines <- expand.grid(x = lon_vals, 
                             y = lat_rng)
    
    # add id
    lon_lines$lon_id <- match(lon_lines$x, lon_vals)
    lon_lines$lat_id <- match(lon_lines$y, lat_vals)
  
    
    # omit crs until after intersection
    lon_lines <- sf::st_as_sf(lon_lines,
                              coords = c("x", "y"),
                              agr = "constant")
    
    # convert to multilinestring    
    lon_lines <- 
        lon_lines %>% 
        dplyr::group_by(lon_id) %>% 
        dplyr::summarize() %>%
        sf::st_cast("MULTILINESTRING") 
  
    
    # strip poly of crs
    # (st_intersects contains artifacts otherwise)
    sf::st_crs(x) <- NA

    # find segment of each line that intersects with x
    sf::st_agr(lon_lines) <- "constant"
    if(inherits(x, "sf")) sf::st_agr(x) <- "constant"
    lon_in_x <- sf::st_intersection(lon_lines, x)
    
    lon_touched <- lapply(sf::st_geometry(lon_in_x), 
                          function(x) {
                            # make line beginning and end same if point
                            if(sf::st_is(x, "POINT")) x <- c(x, x)
                            
                            coords_x <- sf::st_coordinates(x)[, c("X", "Y")]
                            
                            mat_x <- matrix(t(coords_x), ncol = 4, byrow = TRUE)
                            
                            colnames(mat_x) <- c("x1", "y1", "x2", "y2")
                            
                            return(data.frame(mat_x,
                                              line_type = "lon",
                                              segment = 1:nrow(mat_x))
                            )
                          })
    
    
    # reset crs
    sf::st_crs(lon_lines) <- crs_in
    sf::st_crs(lon_in_x) <- crs_in
    
    
    # Lines of latitude
    lat_lines <- expand.grid(x = lon_rng, 
                             y = lat_vals)
    
    # add id
    lat_lines$lon_id <- match(lat_lines$x, lon_vals)
    lat_lines$lat_id <- match(lat_lines$y, lat_vals)
    
    
    # omit crs until after intersection
    lat_lines <- sf::st_as_sf(lat_lines,
                              coords = c("x", "y"),
                              agr = "constant")
    
    # convert to multilinestring    
    lat_lines <- 
      lat_lines %>% 
      dplyr::group_by(lat_id) %>% 
      dplyr::summarize() %>%
      sf::st_cast("MULTILINESTRING") 
    
    
    # find segment of each line that intersects with x
    sf::st_agr(lat_lines) <- "constant"
    if(inherits(x, "sf")) sf::st_agr(x) <- "constant"
    lat_in_x <- sf::st_intersection(lat_lines, x)
    
    lat_touched <- lapply(sf::st_geometry(lat_in_x), 
                          function(x) {
                            # make line beginning and end same if point
                            if(sf::st_is(x, "POINT")) x <- c(x, x)
                            
                            coords_x <- sf::st_coordinates(x)[, c("X", "Y")]
                            
                            mat_x <- matrix(t(coords_x), ncol = 4, byrow = TRUE)
                            
                            colnames(mat_x) <- c("x1", "y1", "x2", "y2")
                            
                            return(data.frame(mat_x,
                                              line_type = "lat",
                                              segment = 1:nrow(mat_x))
                            )
                          })
    
    
    # reset crs
    sf::st_crs(lat_lines) <- crs_in
    sf::st_crs(lat_in_x) <- crs_in
    

    # reset crs
    sf::st_crs(x) <- crs_in
    
    
    #plot(sf::st_geometry(x))
    #plot(sf::st_geometry(lon_lines[lon_lines$lon_id == 23,]), col = "grey", add = TRUE)
    #plot(sf::st_geometry(lon_in_x[lon_in_x$lon_id == 23,]), col = "red", add = TRUE)
    
    #plot(sf::st_geometry(x))
    #plot(sf::st_geometry(lat_lines[lat_lines$lat_id == 32,]), col = "grey", add = TRUE)
    #plot(sf::st_geometry(lat_in_x[lat_in_x$lat_id == 32,]), col = "red", add = TRUE)
    
    
    # combine matched
    all_touched <- data.table::rbindlist(c(lon_touched, lat_touched))
    
    
    # find raster cell that contains each endpoint
    all_touched[,
                `:=`(x1cell = findInterval(x1, lon_vals, 
                                           rightmost.closed = TRUE),
                     x2cell = findInterval(x2, lon_vals, 
                                           rightmost.closed = TRUE),
                     y1cell = findInterval(y1, lat_vals, 
                                           rightmost.closed = TRUE),
                     y2cell = findInterval(y2, lat_vals, 
                                           rightmost.closed = TRUE))]
                     
    # "activate" cells on "both sides" of each edge
    # e.g., where polygon boundary crosses a "lon" edge, should active 
    #  raster cell on left and right of that edge.
      
    all_touched[line_type == "lon",
                x1cell := pmax(x1cell - 1, 1)]
    
    all_touched[line_type == "lat", 
                y1cell := pmax(y1cell - 1, 1)]

    #expand cell ranges

    cells_touched <- all_touched[, expand.grid(x_cell = x1cell:x2cell, 
                                               y_cell = y1cell:y2cell),
        by = 1:nrow(all_touched)]
    
    cells_touched <- unique(cells_touched[, c("x_cell", "y_cell")])
    
    # note that y is flipped here because raster counts from top down
    cells_touched[, rast_cell := 
                    raster::cellFromRowCol(rast, 
                                           row = nrow(rast) - y_cell + 1,
                                           col = x_cell)]
 
    # update raster values   
    val <- rep(0, length(rast))
    val[cells_touched$rast_cell] <- 1
   
    rast <- raster::setValues(rast, val)
    
    #x11(width = 12, height = 8)
    #raster::plot(rast)
    #plot(sf::st_geometry(x), add = TRUE)
    
  }
  
  et <- Sys.time() - t0
    
  if(silent == FALSE) message("Done (", round(et, 1)," ", units(et), ")")
  
  return(rast)
  
}

