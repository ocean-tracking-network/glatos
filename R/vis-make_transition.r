#' Create transition layer from spatial object.
#'
#' Create transition layer for [interpolate_path()] spatial object.
#'
#' @param poly A spatial polygon object of class
#'   [SpatialPolygonsDataFrame][SpatialPolygons][terra::SpatVector] or a [sf::sf()][sf::sf] object
#'   with a geometry column of polygon and/or multipolygon objects.
#'
#' @param res two element vector that specifies the x and y dimension of output
#'   raster cells. Units are same as `poly` crs. May be calculated from desired
#'   resolution in meters using [scale_meters_to_degrees()].
#'
#' @param receiver_points Object containing coordinates of receiver locations.
#'   Must be of class `SpatialPointsDataFrame`, `SpatialPoints`, `sf`, `SpatVector`
#'   `glatos_receivers`, or `glatos_detections`.
#'
#' @details `make_transition` uses [terra::rasterize] to convert a polygon
#'   shapefile into a raster layer and geo-corrected transition layer
#'   [interpolate_path()]. Raster cell values on land equal 0, cells in water
#'   equal 1. Output is a three-object list containing the raster layer, transition layer,
#'   and a data-frame of receiver points that do not fall inside of the spatial polygon. 
#'
#' @details If `receiver_points` is provided, any receiver not in water is identified and included in output.
#'
#' @details output transition layer is corrected for projection distortions
#'   using `gdistance::geoCorrection`.  Adjacent cells are connected by 16
#'   directions and transition function returns 0 (land) for movements between
#'   land and water and >0 for all over-water movements.
#'
#' @details Note that this function underwent breaking changes between 0.7.3 and
#'   0.8.0 (uses `terra::rasterize` instead of `gdalUtilities::gdal_rasterize` see
#'   NEWS).
#'
#' @return A list with three elements:
#' \describe{
#'    \item{transition}{a geo-corrected transition raster layer where land = 0
#'       and water = 1
#'   (see [`gdistance::Transition-class`])}
#'    \item{rast}{rasterized input layer of class `raster`}}
#'    \item{pip}{table of receivers not within polygon input layer}
#'
#' @author Todd Hayden, Chris Holbrook
#'
#' @examples
#'
#' # Example 1 - read from sf polygon object
#' # use example polygon for Great lakes
#'
#' # make_transition layer
#' tst <- make_transition(great_lakes_polygon, res = 0.1)
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
#'
#' \dontrun{
#' # plot raster layer (notice water = 1, land = 0)
#' raster::plot(tst$rast)
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
#'   package = "glatos"
#' )
#' recs <- read_glatos_receivers(rec_file)
#'
#' 
#' # make_transition layer (roughly 500 m res)
#' tst <- make_transition(poly, res = c(0.065, 0.046), receiver_points = recs)
#'
#' # list points outside of polygons
#' tst$pip
#' 
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
#'   package = "glatos"
#' )
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
#' recs <- sf::st_as_sf(recs,
#'   coords = c("deploy_long", "deploy_lat"),
#'   crs = 4326
#' )
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
                            receiver_points = NULL) {

    if (inherits(poly, c("SpatialPolygonsDataFrame", "SpatialPolygons", 
        "sf", "SpatVector")) == FALSE) {
        stop(paste0("Supplied object for 'poly' argument is not class ", 
            "SpatialPolygonsDataFrame, SpatialPolygons, a sf polygon object, or a terra SpatVector"), 
            call. = FALSE)
    }
  
    if (inherits(poly, c("SpatialPolygonsDataFrame", "SpatialPolygons", "sf")) == 
        TRUE) {
        poly <- terra::vect(poly)
    }
  
    if (length(res) == 1) 
      res <- rep(res, 2)

  # recevier points- class checks
  if (is.null(receiver_points) == FALSE) {
    
    if (inherits(receiver_points, c("SpatVector"))){
      og_crs <- terra::crs(receiver_points)
      receiver_points <- terra::project(receiver_points, terra::crs(poly))
    } 
    
    if (inherits(receiver_points, c("glatos_receivers", "glatos_detections"))) {
      receiver_points <- terra::vect(as.data.frame(receiver_points), geom = c("deploy_long", "deploy_lat"), crs = "epsg:4326")
      og_crs <- terra::crs(receiver_points)
      receiver_points <- terra::project(receiver_points, terra::crs(poly))
    }  
    
    if (inherits(receiver_points, c("SpatialPoints", "SpatialPointsDataFrame", "sf"))) {
      receiver_points <- terra::vect(receiver_points)
      og_crs <- terra::crs(receiver_points)
      receiver_points <- terra::project(receiver_points, terra::crs(poly))
    } 
        
    if (inherits(receiver_points, c("SpatialPointsDataFrame", 
                                    "SpatialPoints", "sf", "glatos_receivers", "glatos_detections", "SpatVector")) == 
          FALSE) {
      stop("Supplied object for 'receiver_points' argument is not class ", 
           "SpatialPolygonsDataFrame, SpatialPolygons, sf polygon object, ", 
           "terra SpatVector, glatos_receiver, or glatos_detections", call. = FALSE)
    }
    
    # identify any points on land
    message("Checking for receivers on land...")

    pip <- terra::relate(poly, receiver_points, "intersects", pairs = TRUE)
    
    if(nrow(pip > 0)){
      warning(paste0("Receiver_points were found outside of polygon! \n",
                     "Points outside of polygon will not be used in interpolation \n",
                     "See output for points."))
      land <- receiver_points[-(pip[, "id.y"]),]
      land <- project(land, og_crs)
      land <- cbind(as.data.table(terra::crds(land)), as.data.table(land))
      data.table::setnames(land, c("x", "y"), c("deploy_long", "deploy_lat"))
    }
  }

  # make empty raster
  rast_temp <- terra::rast(resolution = res, extent = terra::ext(poly), crs = terra::crs(poly))
  
  # make raster
  burned <- terra::rasterize(x = poly, y = rast_temp, field = 1, background = 0, touches = TRUE, update = TRUE)

  message("Making transition layer...")
  t0 <- Sys.time()
  tran <- function(x) {
    if (x[1] * x[2] == 0) {
      return(0)
    }
    else {
      return(1)
    }
  }

  tr1 <- gdistance::transition(raster::raster(burned), transitionFunction = tran, 
        directions = 16)
    tr1 <- gdistance::geoCorrection(tr1, type = "c")
    et <- Sys.time() - t0
  message("Done (", round(et, 1), " ", units(et), ")")

  if(nrow(land) == 0){
    out <- list(transition = tr1, rast = burned, pip = NA)
  } else {
    out <- list(transition = tr1, rast = burned, pip = land)
  }    
    
    return(out)
}
