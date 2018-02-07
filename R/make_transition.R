
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param in_file
##' @details result is in same projection as input, must have working gdal on computer, see instructions for gdalUtils::gdal_rasterize, produces layer with land = 0, water = 1.  Need argument for tr 
##' @return returns transition raster layer (gdistance)
##' @author Todd Hayden, Tom Binder, Chris Holbrook
##' @examples
##'
##' # path to polygon shapefile
##' in_file <- "/home/thayden/Documents/R_workshop/coastline_poly_modified/glshoreline_mod.shp"
##'
##' # make transition layer
##' make_transition(in_file)
##' @export


make_transition <- function(in_file){
  # gdalUtils::gdal_rasterize function is WAY faster than
  # raster::rasterize and seems to do a much better job keep in mind
  # that "at" argument can be used to change default method for
  # rasterizing when line crosses partial cell.  including at in the
  # call turns this on- default is not include this.  Setting at == TRUE
  # does nothing.

  foo <- gdalUtils::gdal_rasterize(in_file, out_file, burn = 1, tr = c(0.1, 0.1),
                                   output_Raster = TRUE)
  foo <- raster(foo)
  tran <- function(x){if(x[1] * x[2] == 0){return(0)} else {return(1)}}
  tr1 <- gdistance::transition(tst, transitionFunction = tran, directions = 16)
  tr1C <- gdistance::geoCorrection(tr1, type="c")
  return(tr1C)
}



