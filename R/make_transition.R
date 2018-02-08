##' Create transition layer from polygon shapefile
##'
##' Create transition layer for \code{interpolatePath} from polygon shapefile.
##'
##' 
##' @param in_file character, file path to polygon shapefile
##' @param out_file character, file path to output raster
##' @param raster_out logical, default is false
##' 
##' @details
##'
##'
##' result is in same projection as input, must have working gdal on computer, see instructions for gdalUtils::gdal_rasterize, produces layer with land = 0, water = 1.  Need argument for tr 
##' @return returns transition raster layer (gdistance)
##' @author Todd Hayden, Tom Binder, Chris Holbrook
##' @examples
##'
##' # path to polygon shapefile
##' in_file <- "/home/thayden/Documents/R_workshop/coastline_poly_modified/glshoreline_mod.shp"
##' 
##' 
##' # make transition layer
##' make_transition(in_file)
##' @export


make_transition <- function(in_file, raster_out = FALSE){
  # gdalUtils::gdal_rasterize function is WAY faster than
  # raster::rasterize and seems to do a much better job keep in mind
  # that "at" argument can be used to change default method for
  # rasterizing when line crosses partial cell.  including at in the
  # call turns this on- default is not include this.  Setting at == TRUE
  # does nothing.
  out <-tempfile(fileext = ".tif")
    
  foo <- gdalUtils::gdal_rasterize(in_file, dst_filename= out, burn = 1, tr = c(0.1, 0.1),
                                   output_Raster = TRUE)
  unlink(out)
  
  foo <- raster(foo)
  tran <- function(x){if(x[1] * x[2] == 0){return(0)} else {return(1)}}
  tr1 <- gdistance::transition(tst, transitionFunction = tran, directions = 16)
  tr1C <- gdistance::geoCorrection(tr1, type="c")
  return(tr1C)
}



