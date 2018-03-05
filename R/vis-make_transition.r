##' Create transition layer from polygon shapefile
##'
##' Create transition layer for \link{interpolate_path} from polygon shapefile.
##' 
##' @param in_file character, file path to polygon shapefile (with
##'   extension of *.shp)
##'
##' @param output character, name of output file with .tif extension
##'
##' @param output_dir character, directory where output file will be written
##' 
##' @param res two element vector that specifies the x and y dimension
##'   of output raster cells.  Units of res are same as input
##'   shapefile.
##' 
##' @details \code{make_transition} uses
##'   \link[gdalUtils]{gdal_rasterize} to convert a polygon shapefile
##'   into a raster layer, and geo-corrected transition layer
##'   \link{interpolate_path}.  Raster cell values on land = 0 and
##'   water = 1. Function also writes a geotiff file (*.tif) of the
##'   input shapefile to the ouput directory. Both raster layer and
##'   geotif output have the same extents and geographic projection as
##'   input shapefile.  Function requires that gdal is working on
##'   computer.  To determine if gdal is installed on your computer,
##'   see \link[gdalUtils]{gdal_rasterize}.
##'
##' @details output transition layer is corrected for projection
##'   distortions using \code{gdistance::geoCorrection}.  Adjacent
##'   cells are connected by 16 directions and transition function
##'   returns 0 (land) for movements between land and water and 1 for
##'   all over-water movements.
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


make_transition <- function(in_file, output = "out.tif",
                            output_dir = getwd(), res = c(0.1, 0.1)){
  burned <- gdalUtils::gdal_rasterize(path.expand(in_file),
                                      dst_filename = path.expand(file.path(output_dir, output)),
                                      burn = 1,
                                      tr = res,
                                      output_Raster = TRUE)

  burned <- raster::raster(burned, layer = 1)

  tran <- function(x){if(x[1] * x[2] == 0){return(0)} else {return(1)}}
  tr1 <- gdistance::transition(burned, transitionFunction = tran, directions = 16)
  tr1 <- gdistance::geoCorrection(tr1, type="c")
  out <- list(transition = tr1, rast = burned)
  return(out)
}
