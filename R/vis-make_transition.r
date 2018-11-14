##' Create transition layer from polygon shapefile
##'
##' Create transition layer for \link{interpolate_path} from polygon shapefile.
##' 
##' @param in_file A SpatialPolygonsDataFrame object or a character string with file path to 
##'   polygon shapefile (with extension of *.shp).
##'
##' @param output character, name of output file with .tif extension
##'
##' @param output_dir character, directory where output file will be written. If NULL (default), 
##'   then files will be written to temporary directory that will be deleted after R session 
##'   is closed (see \link[=tempfile]{tempdir}).
##' 
##' @param res two element vector that specifies the x and y dimension
##'   of output raster cells.  Units of res are same as input
##'   shapefile.
##'
##' @param receiver_points A SpatialPointsDataFrame object that
##'   contains coordinates of receivers in detection dataset.
##'
##' 
##' @details \code{make_transition} uses
##'   \link[gdalUtils]{gdal_rasterize} to convert a polygon shapefile
##'   into a raster layer and geo-corrected transition layer
##'   \link{interpolate_path}.  Raster cell values on land equal value
##'   specified in "land" argument (default = 1000) and equals "water"
##'   argument (default = 1e-10) for water. Function also writes a geotiff
##'   file (*.tif) of the input shapefile to the ouput directory. Both
##'   raster layer and geotif output have the same extents and
##'   geographic projection as input shapefile.  Function requires
##'   that gdal is working on computer.  To determine if gdal is
##'   installed on your computer, see
##'   \link[gdalUtils]{gdal_rasterize}.
##'   
##' @details Returned objects will be projected in longlat WGS84
##'   (i.e., CRS("+init=epsg:4326"). If the input object is not in
##'   longlat WGS84 then transformation will be attempted and a
##'   warning will tell the user this was done.  Input shapefile must
##'   include an optional *.prj file that specifies the geographic projection.
##'
##' @details output transition layer is corrected for projection
##'   distortions using \code{gdistance::geoCorrection}.  Adjacent
##'   cells are connected by 16 directions and transition function
##'   returns 0 (land) for movements between land and water and 1 for
##'   all over-water movements.
##'
##' @details If receiver_points is provided, any receiver not in water
##'   is buffered by the distance from the receiver to the nearest
##'   water.  This allows all receivers to be coded as in water if the
##'   receiver is on land.
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
##' \dontrun{
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
##' tst <- make_transition(greatLakesPoly, res = c(0.1, 0.1))
##' 
##' # plot raster layer
##' # notice land = 1, water = 0
##' plot(tst$rast)
##' 
##' #compare to polygon
##' plot(greatLakesPoly, add = TRUE)
##'
##' # increase resolution and repeat if needed
##' 
##' #------------------------------------------
##' #Example 2 - read from ESRI Shapefile
##' # path to polygon shapefile
##' poly <- system.file("extdata", "shoreline.zip", package = "glatos")
##' poly <- unzip(poly, exdir = tempdir())
##'
##' # make_transition layer
##' tst <- make_transition(poly[grepl("*.shp", poly)], res = c(0.1, 0.1))
##'
##' # plot raster layer
##' # notice land = 0, water = 1
##' raster::plot(tst$rast)
##'
##' # plot transition layer
##' raster::plot(raster::raster(tst$transition))
##' 
##' # increase resolution- this may take some time...
##' tst1 <- make_transition(poly[grepl("*.shp", poly)], res = c(0.01, 0.01))
##'
##' # plot raster layer
##' raster::plot(tst1$rast)
##'
##' # plot transition layer
##' raster::plot(raster::raster(tst1$transition))
##' }
##' 
##' @export

make_transition <- function(in_file, output = "out.tif", output_dir = NULL,
                             res = c(0.1, 0.1), receiver_points = NULL) {
   
  gdalUtils::gdal_setInstallation()
  valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
  if (!valid_install) {
    stop("No GDAL installation found. Please install 'gdal' before continuing:\n\t-
see: www.gdal.org\n\t- https://trac.osgeo.org/osgeo4w/ (windows)\n")
  }

  if (inherits(in_file, "character")) {
    if (!file.exists(in_file)) 
      stop(paste0("Input file or folder '", in_file, "' not found."))
    if (!grepl("\\.shp$", in_file))
      stop(paste0("'in_file' must be a path to an ESRI shapefile (*.shp)"))
    if (!file.exists(in_file)) 
      stop(paste0("'in_file' directory '", in_dir, 
                  "' not found."))
    
    in_file <- rgdal::readOGR(in_file, verbose = FALSE)
  }
  if (!inherits(in_file, c("SpatialPolygonsDataFrame", "SpatialPolygons"))) 
    stop(paste0("'in_file' must be an object of class\n", "'SpatialPolygonsDataFrame or 'SpatialPolygons'"))

  default_proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  default_projection_transform <- function(default_proj, sp_obj){
    if(sp::proj4string(sp_obj) != default_proj){
      warning(paste0("Projection of input was not longlat WGS84, so conversion was attempted."), 
              call. = FALSE)
      out <- sp::spTransform(sp_obj, sp::CRS(default_proj))
      return(out)} else {
        return(sp_obj)}  
  }

  in_file <- default_projection_transform(default_proj = default_proj,
                                          sp_obj = in_file)
  
  if (inherits(receiver_points, c("SpatialPoints", "SpatialPointsDataFrame"))){

    receiver_points <- default_projection_transform(default_proj = default_proj,
                                                    sp_obj = receiver_points)
    
    # determine what records are not in polygon
    on_land <- receiver_points[is.na(over(receiver_points,
                                          as(in_file, "SpatialPolygons")))]

    # calculate radius from receiver to polygon for buffer
    dist_on_land <- geosphere::dist2Line(on_land, in_file,
                                         distfun = geosphere::distGeo)
    
    # create sp object
    on_land_corrected  <- SpatialPointsDataFrame(
      dist_on_land[, c("lon", "lat")],
      as.data.frame(dist_on_land[, c("distance", "ID")]),
      proj4string = CRS(default_proj))

    # do buffers
    on_land_corrected <- dismo::circles(on_land_corrected,
                                        on_land_corrected$distance)
    on_land_corrected <- spTransform(on_land_corrected@polygons, default_proj)

    # combine buffered points and original outline
    in_file <- bind(on_land_corrected, in_file)

  }
  
  temp_dir <- path.expand(file.path(tempdir()))
  rgdal::writeOGR(in_file, dsn = temp_dir, layer = "map_to_burn.shp",
                  driver = "ESRI Shapefile", overwrite_layer = TRUE)
  if (is.null(output_dir)) 
    output_dir <- temp_dir

  burned <- gdalUtils::gdal_rasterize(temp_dir,
                                      dst_filename =
                                        path.expand(file.path(output_dir, output)),
                                      burn = 1, tr = res, output_Raster = TRUE,
                                      at = TRUE)
  burned <- raster::raster(burned, layer = 1)
  tran <- function(x){if(x[1] * x[2] == 0){return(0)} else {return(1)}}

  tr1 <- gdistance::transition(burned, transitionFunction = tran, 
                               directions = 16)
  tr1 <- gdistance::geoCorrection(tr1, type = "c")
  out <- list(transition = tr1, rast = burned)
  return(out)

}
