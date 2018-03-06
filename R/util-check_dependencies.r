##' Check system dependencies necessary for package 'glatos' 
##'
##' Checks to make sure external dependencies required by functions in
##' 'glatos' are installed and available to R.
##'
##' 
##' @details Function returns a list of tests for all external
##'   dependencies needed for all functions in 'glatos'.  
##'   
##' 
##' @return A list:
##' 
##' @author Todd Hayden, Tom Binder, Chris Holbrook
##'
##' @export

check_dependencies <- function(){

  # check for gdal
  # need other checks added...
  gdal_setInstallation()
  getOption("gdalUtils_gdalPath")
     valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
  out <- list(gdal = valid_install)
  return(out)

 } 
