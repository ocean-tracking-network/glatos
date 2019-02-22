##' Create transition layer from polygon shapefile
##'
##' Create transition layer for \link{interpolate_path} from polygon shapefile.
##'
##' @param poly A spatial polygon object of class
##'   \link[=SpatialPolygons]{SpatialPolygonsDataFrame} or a
##'   \link[sf:sf]{sf::sf()} object with a geometry column of polygon
##'   and/or multipolygon objects.
##' 
##' @param res two element vector that specifies the x and y dimension
##'   of output raster cells.  Units of res are same as input
##'   shapefile.
##'
##' @param receiver_points A SpatialPointsDataFrame object that
##'   contains coordinates of receivers  dataset or a "glatos_receivers" object.
##'
##' @param epsg coordinate reference code that describes projection
##'   used for map calculation and rasterization.  Defaults to
##'   NAD83/Great Lakes and St. Lawrence Albers.
##' 
##' @details \code{make_transition} uses \link[fasterize]{fasterize}
##'   to convert a polygon shapefile into a raster layer and
##'   geo-corrected transition layer \link{interpolate_path}.  Raster
##'   cell values on land equal 1 cells in water equal 0. Output is a
##'   two-object list containing the raster layer and transition
##'   layer.  Both objects have the same extents and geographic
##'   projection as input shapefile.
##'
##'  @details If receiver\_points is provided, any receiver not in water
##'   is buffered by the distance from the receiver to the nearest
##'   water.  This allows all receivers to be coded as in water if the
##'   receiver is on land.
##' 
##' @details Poly object is transformed into planer map projection
##'   specified by epsg argument for calculation of transition object
##'   if receiver_points is provided.  Output is projected to WGS84
##'   (epsg- 4326).
##'  
##' @details output transition layer is corrected for projection
##'   distortions using \code{gdistance::geoCorrection}.  Adjacent
##'   cells are connected by 16 directions and transition function
##'   returns 0 (land) for movements between land and water and 1 for
##'   all over-water movements.
##'
##' 
##' @return A list with two elements:
##' \describe{
##'    \item{transition}{a geo-corrected transition raster layer where land = 0
##'       and water=1
##'   (see \code{gdistance})}
##'    \item{rast}{rasterized input layer of class \code{raster}}}
##'   Additonally, rasterized version of input shapefile (*.tif extension) is written to computer
##'   at \code{output_dir} 
##' 
##'
##' @author Todd Hayden, Tom Binder, Chris Holbrook
##'
##' @examples
##' 
##' #Example 1 - read from SpatialPolygonsDataFrame
##' # use example polygon for Great lakes
##' 
##' library(sp) #for loading greatLakesPoly
##' library(raster) # for plotting rasters
##' 
##' #get polygon of the Great Lakes
##' data(greatLakesPoly) #glatos example data; a SpatialPolygonsDataFrame
##' 
##' # make_transition layer
##' tst <- make_transition3(greatLakesPoly, res = c(0.1, 0.1))
##' 
##' # plot raster layer
##' # notice land = 1, water = 0
##' plot(tst$rast)
##' 
##' #compare to polygon
##' plot(greatLakesPoly, add = TRUE)
##'
##' #Example 2 - read from ESRI Shapefile and include receiver file
##' # to account for any receivers outside of great lakes polygon
##' 
##' # path to polygon shapefile
##' poly <- system.file("extdata", "shoreline.zip", package = "glatos")
##' poly <- unzip(poly, exdir = tempdir())
##' poly <- sf::st_read(poly[grepl("*.shp", poly)])
##'
##' # read in glatos receivers object
##' rec_file <- system.file("extdata", "sample_receivers.csv", package="glatos")
##' recs <- read_glatos_receivers(rec_file)
##'
##' # change a coordinate to on-land to show impact...
##' recs[1, "deploy_lat"] <- recs[1,"deploy_lat"]+4
##' 
##' # make_transition layer
##' tst <- make_transition3(poly, res = c(0.1, 0.1), receiver_points = recs)
##'
##' # plot raster layer
##' # notice the huge circle rasterized as "water"  north of Lake Superior.
##' # This occurred because we had a "receiver" deployed at that locations
##' raster::plot(tst$rast)
##' points(recs$deploy_long, recs$deploy_lat, col = "red", pch = 20)
##' 
##' # plot transition layer
##' raster::plot(raster::raster(tst$transition))
##'
##' Example 3- transition layer of Lake Huron only with receivers
##' 
##' # read polygon shapefile
##' poly <- system.file("extdata", "shoreline.zip", package = "glatos")
##' poly <- unzip(poly, exdir = tempdir())
##' poly <- sf::st_read(poly[grepl("*.shp", poly)])
##' 
##' 
##' # transform to great lakes projection
##' poly <- sf::st_transform(poly, crs = 3175)
##'
##' # set attribute-geometry relationship to constant.
##' # this avoids error when cropping
##' sf::st_agr(poly) = "constant"
##'
##' # crop Great lakes polygon file
##' poly <- sf::st_crop(x = poly, xmin = 829242.55, ymin = 698928.27,
##'                               xmax = 1270000.97, ymax = 1097196.15)
##'
##' # read in glatos receivers object
##' rec_file <- system.file("extdata", "sample_receivers.csv", package="glatos")
##' recs <- read_glatos_receivers(rec_file)
##'
##' # extract receivers in "HECWL" project
##' # all receiver stations except one is in Lake Huron
##' recs <- recs[recs$glatos_project == "HECWL",]
##'
##' # remove two stations not in Lake Huron
##' recs <- recs[!recs$glatos_array %in% c("MAU","LVD"),]
##'
##' # convert recs to simple feature  object (sf)
##' recs <- sf::st_as_sf(recs, coords = c("deploy_long", "deploy_lat"), crs = 4326 )
##'
##' # transform to great lakes projection
##' recs <- sf::st_transform(recs, crs = 3175)
##'
##' # check by plotting
##' plot(sf::st_geometry(poly), col = NA)
##' plot(sf::st_geometry(recs), col = "red", add = TRUE)
##' 
##' # create slightly higher resolution transition layer
##' tst1 <- make_transition3(poly, res = c(0.01, 0.01), receiver_points = recs)
##'
##' # plot raster layer
##' raster::plot(tst1$rast)
##'  plot(sf::st_transform(sf::st_geometry(recs), crs = 4326), add = TRUE, col = "red", pch = 20)
##'
##' # plot transition layer
##' raster::plot(raster::raster(tst1$transition))
##'
##' 
##' @export

make_transition3 <- function (poly, res = c(0.1, 0.1), receiver_points = NULL, epsg = 3175){
  message("Making transition layer...")

  if(inherits(poly,  c("SpatialPolygonsDataFrame", "SpatialPolygons", "sf")) == FALSE) {
    stop(paste0("Supplied object for 'poly' argument is not class ",
                "SpatialPolygonsDataFrame, SpatialPolygons, or a sf polygon object"), call. = FALSE)}        

  if (inherits(poly, c("SpatialPolygonsDataFrame", "SpatialPolygons")) == TRUE){
    poly <- sf::st_as_sf(poly)
  }

  # If receiver_points is specified
  if(is.null(receiver_points) == FALSE){
    if(inherits(receiver_points, c("glatos_receivers", "glatos_detections"))){
      receiver_points <- sf::st_as_sf(receiver_points,
                                  coords = c("deploy_long", "deploy_lat"),
                                  crs = 4326, agr="identity")
    }

    if(inherits(receiver_points, c("SpatialPoints", "SpatialPointsDataFrame"))){
      receiver_points <- sf::st_as_sf(receiver_points)
    }

    if(inherits(receiver_points,  c("SpatialPolygonsDataFrame",
                                    "SpatialPolygons", "sf",
                                    "glatos_receivers",
                                    "glatos_detections")) == FALSE) {
      stop(paste0("Supplied object for 'receiver_points' argument is not class ", "SpatialPolygonsDataFrame, SpatialPolygons, sf polygon object, or glatos_receiver, glatos_detections"), call. = FALSE)
    }        
    
    # convert to GL projection
    poly_gl <- sf::st_transform(poly, crs = epsg)
    recs_gl <- sf::st_transform(receiver_points, crs = epsg) 

    # determine shortest distance from receiver to water polygon
    dist_rec <- units::drop_units(sf::st_distance(recs_gl, poly_gl))
    recs_gl$rec_water_dist <- apply(dist_rec, 1, "min")

    # extract rec_water_dist > 0
    land <- recs_gl[recs_gl$rec_water_dist > 0,]

    # calculate buffer around points not in polygon.
    land <- sf::st_buffer(land, land$rec_water_dist)

    # set attribute geometry relationship to constant
    sf::st_agr(poly_gl) = "constant"
    sf::st_agr(land) = "constant"

    # union buffered points and land
    poly_buff <- sf::st_union(poly_gl, land)
    
    # convert back to CRS 4236 (wgs84 lat/lon)
    poly <- sf::st_transform(poly_buff, crs = 4326)
  }
       
  # fasterize rasterize
  burned = fasterize::fasterize(sf = poly, raster = raster::raster(res = res, ext = raster::extent(poly)), field = NULL, fun = "last", background = 0, by = NULL)

  # function for transition matrix
  tran <- function(x) if (x[1] * x[2] == 0) {
    return(0)
  } else {
    return(1)
  }

  # calculate distances
  tr1 <- gdistance::transition(burned, transitionFunction = tran,
                                 directions = 16)
  
  tr1 <- gdistance::geoCorrection(tr1, type = "c")
  out <- list(transition = tr1, rast = burned)
  message("Done.")
  return(out)
}

