#' @title 
#' Read data from a GLATOS detection file
#' 
#' @description
#' Read data from a GLATOS detection (csv) file and return a data.table of  
#' class \code{glatos_detections}.
#'
#' @param det_file A character string with path and name of detection file in 
#'  standard GLATOS format (*.csv). If only file name is given, then the 
#'  file must be located in the working directory.
#'  
#' @param version An optional character string with 
#'  the glatos file version number. If NULL (default value) then version 
#'  will be determined by evaluating workbook structure. The only allowed 
#'  values are \code{NULL} and \code{"1.3"}. Any other values will trigger 
#'  an error.
#'  
#' @details
#' For speed, data are loaded using the \code{fread} function in the 
#' \code{data.table} package and timestamps are coerced to POSIXct usign the 
#' \code{fastPOSIXct} function in the \code{fasttime} package. All times must be
#' in UTC timezone per GLATOS standard.
#' 
#' @return A data.table object of class \code{glatos_detections}:
#'
#' @author C. Holbrook (cholbrook@usgs.gov) 
#'
#' @examples
#' #get path to example detection file
#' det_file <- system.file("extdata", 
#'   "NEED_TO_ADD.csv",package="glatos")
#' det <- read_glatos_detections(det_file)
#'
#' @export
read_glatos_detections <- function(det_file, version=NULL) {

  #Read detection file-------------------------------------------------------
  
  #Get version-specific workbook specifications
  data(detection_specs)
  
  
  #Identify detection file version
  ##TODO: expand version matching to use column names in each sheet
  id_det_version <- function(det_file){
    det_col_names <- names(data.table::fread(det_file, nrows = 0))
    if(all(detection_specs$v1.3$name == det_col_names)) { 
      return("1.3") 
    } else {
      stop("Detection file version could not be identified.")
    }
  }
  
  if(is.null(version)) {
    version <- id_det_version(det_file)
  } else if (!(paste0("v",version) %in% names(detection_specs))) {
    stop(paste0("Detection file version ",version," is not supported."))
  }
 
  #-Detections v1.3----------------------------------------------------------------  
  if (version == "1.3") {

    #read data
    det <- data.table::fread(det_file, sep = ",")
    
    #coerce timestamps to POSIXct; note that with fastPOSIXct raw
    #  timestamp must be in UTC; and tz argument sets the tzone attr only
    det[ , detection_timestamp_utc := 
             fasttime::fastPOSIXct(detection_timestamp_utc, tz = "UTC")]
    det[ , utc_release_date_time := 
             fasttime::fastPOSIXct(utc_release_date_time, tz = "UTC")]
    
    #coerce dates to date
    det[ , glatos_caught_date := 
        as.Date(glatos_caught_date)]
  }
  #-end v1.3----------------------------------------------------------------
  
  #TO DO: cerce to glatos_detections (e.g., as_glatos_detections) instead of 
  class(det) <- c("glatos_detections",class(det))
  
  return(det)
}
