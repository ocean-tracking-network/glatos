##' Create transition layer from polygon shapefile
##'
##' Create transition layer for \code{interpolatePath} from polygon shapefile.
##' 
##' @param in_file character, file path to polygon shapefile
##' @param res two element vector that specifies the x and y dimension
##'   of output raster cells.  In example, units are degrees.
##' 
##' @details \code{make_transition} uses gdalUtils::gdal_rasterize to
##'   convert a polygon shapefile into transition layer (see
##'   \code{gdistance} where raster cell values on land = 0 and water
##'   = 1. Output raster is in same projection as input shapefile and
##'   has same extents as input shapefile.  Function requires that
##'   gdal is working on computer.  To determine if gdal is installed
##'   on your computer, see gdalUtils::gdal_rasterize.
##'
##' @details output transition layer is corrected for projection
##'   distortions using \code{gdistance::geoCorrection}.  Adjacent
##'   cells are connected by 16 directions and transition function
##'   returns 0 (land) for movements between land and water and 1 for
##'   all over-water movements.
##' 
##' @return returns geo-corrected transition raster layer where land = 0 and water =
##'   1 (see \code{gdistance})
##' @author Todd Hayden, Tom Binder, Chris Holbrook
##' @examples
##'
##' # path to polygon shapefile
##' in_file <- system.file("extdata", "glshoreline_mod.zip", package = "glatos")
##' gl <- unzip(in_file, "glshoreline_mod.shp", )
##'
##' # make_transition layer
##' tst <- make_transition(gl, res = c(0.1, 0.1))
##'
##' # 
##'
##' 
##' # make transition layer
##' # make_transition(in_file)
##' @export


make_transition <- function(in_file, res = c(0.1, 0.1)){
  out <-tempfile(fileext = ".tif")
  burned <- gdalUtils::gdal_rasterize(in_file, dst_filename= out, burn = 1,
                                      tr = res, output_Raster = TRUE)
  unlink(out)
  burned <- raster::raster(burned)
  tran <- function(x){if(x[1] * x[2] == 0){return(0)} else {return(1)}}
  tr1 <- gdistance::transition(burned, transitionFunction = tran, directions = 16)
  tr1 <- gdistance::geoCorrection(tr1, type="c")
  return(tr1)
}
