##' Create transition layer from polygon shapefile
##'
##' Create transition layer for \code{interpolatePath} from polygon shapefile.
##' 
##' @param in_file character, file path to polygon shapefile (with
##'   extension of *.shp)
##'
##' @param output character, name of output file for rasterized \code{in_file}
##'
##' @param output_dir character, directory that output file will be written
##' 
##' @param res two element vector that specifies the x and y dimension
##'   of output raster cells.  Units of res are same as input
##'   shapefile.
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
##' @return A list with two elements:
##' \describe{
##'    \item{transition}{a geo-corrected transition raster layer where land = 0
##'       and water=1
##'   (see \code{gdistance})}
##'    \item{rast}{rasterized input layer of class \code{raster}}}
##'   Additonally, rasterized version of input shapefile is written to computer
##'   at \code{output_dir} 
##' 
##'
##' @author Todd Hayden, Tom Binder, Chris Holbrook
##'
##' @examples
##'
##' # path to polygon shapefile
##' poly <- system.file("extdata", "shoreline.zip", package = "glatos")
##' poly <- unzip(poly, exdir = tempdir())
##'
##' # make_transition layer
##' tst <- make_transition(poly[grepl("*.shp", poly)], res = c(0.1, 0.1))
##'
##' # plot raster layer
##' # notice land = 1, water = 0
##' raster::plot(tst$rast)
##'
##' # increase resolution
##' tst1 <- make_transition(poly[grepl("*.shp", poly)], res = c(0.01, 0.01))
##' raster::plot(tst1$rast)
##' 
##' @export


make_transition <- function(in_file = ".", output = "out.tif",
                            output_dir = ".", res = c(0.1, 0.1)){

  out <- tempfile(fileext = paste0(".", (tools::file_ext(output)) ))

  burned <- gdalUtils::gdal_rasterize(in_file, dst_filename = out,
                                      burn = 1, tr = res, output_Raster = TRUE)
  file.copy(out, file.path(output_dir, output), overwrite = TRUE)
  burned <-  raster::raster(file.path(output_dir, output))
  unlink(out)
  tran <- function(x){if(x[1] * x[2] == 0){return(0)} else {return(1)}}
  tr1 <- gdistance::transition(burned, transitionFunction = tran,
                               directions = 16)
  tr1 <- gdistance::geoCorrection(tr1, type="c")
  out <- list(transition = tr1, rast = burned)
  return(out)
}
