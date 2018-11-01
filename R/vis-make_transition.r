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
##' @param water value coded as water in transition layer. Represents
##'   the "cost" associated with moving between raster cells coded as
##'   water.
##'
##' @param land value coded as land in transition layer. Represents
##'   the "cost" associated with impossible (for fish) over land
##'   movements.
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
##' @details default values for "land" and "water" arguments allow
##'   interpolation of fish movements over land when receiver is coded
##'   as on "land" in transition layer.  This often occurs for
##'   receivers in rivers when pixel size of transition layer is too
##'   large to distinguish between water and land.  Changing land
##'   argument to 0 and water to 1 will prevent any interpolation
##'   overland and result in an error if a receiver is on land.
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

make_transition <- function (in_file, output = "out.tif", output_dir = NULL,
                             res = c(0.1, 0.1), water = 1e-10, land = 1000){

  # convert from "cost" to "conductance"
#  water <- 1/water
#  land <- 1/land 
  
    gdalUtils::gdal_setInstallation()
    valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
    if (!valid_install) {
        stop("No GDAL installation found. Please install 'gdal' before continuing:\n\t- see: www.gdal.org\n\t- https://trac.osgeo.org/osgeo4w/ (windows)\n")
    }
    if (inherits(in_file, "character")) {
        if (!file.exists(in_file)) 
            stop(paste0("Input file or folder '", in_file, "' not found."))
        if (grepl("\\.shp$", in_file)) {
            in_dir <- dirname(in_file)
            if (!file.exists(in_dir)) 
                stop(paste0("'in_file' directory '", in_dir, 
                  "' not found."))
            in_layer <- basename(tools::file_path_sans_ext(basename(in_file)))
        }
        else {
            in_dir <- in_file
            in_layer <- rgdal::ogrListLayers(in_dir)[1]
        }
        in_shp <- rgdal::readOGR(in_dir, layer = in_layer, verbose = FALSE)
        if (!inherits(in_shp, "SpatialPolygonsDataFrame")) 
            stop(paste0("Input can only contain ", "polygon data."))
    }
    else if (inherits(in_file, "SpatialPolygonsDataFrame")) {
        in_shp <- in_file
        in_layer <- deparse(substitute(in_file))
    }
    else {
        stop(paste0("'in_file' must be either an object of class 'SpatialPolygonsDataFrame' or\n", 
            " path to an ESRI Shapefile."))
    }
    default_proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    if (sp::proj4string(in_shp) != default_proj) {
        warning(paste0("Projection of input was not longlat WGS84, so conversion was attempted."), 
            call. = FALSE)
        in_shp <- sp::spTransform(in_shp, sp::CRS(default_proj))
    }
    temp_dir <- path.expand(file.path(tempdir(), in_layer))
    rgdal::writeOGR(in_shp, dsn = temp_dir, layer = in_layer, 
        driver = "ESRI Shapefile", overwrite_layer = TRUE)
    if (is.null(output_dir)) 
        output_dir <- temp_dir
    burned <- gdalUtils::gdal_rasterize(temp_dir, dst_filename = path.expand(file.path(output_dir, 
        output)), burn = water, tr = res, output_Raster = TRUE)
    burned <- raster::raster(burned, layer = 1)
    burned[burned == 0] <- land
    
    tran <- function(x) {
        if (x[1] == land | x[2] == land) {
            return(land)
        }
        else {
            return(water)
        }
    }
    tr1 <- gdistance::transition(burned, transitionFunction = tran, 
        directions = 16)
    tr1 <- gdistance::geoCorrection(tr1, type = "c")
    out <- list(transition = tr1, rast = burned)
    return(out)
}

