#' @title 
#' Read data from a GLATOS detection file
#' 
#' @description Read data from a standard GLATOS detection (csv) file and return
#' a data.frame of class \code{glatos_detections}.
#'
#' @param det_file A character string with path and name of detection file in 
#'  standard GLATOS format (*.csv). If only file name is given, then the 
#'  file must be located in the working directory.
#'  
#' @param version An optional character string with the glatos file version
#'   number. If NULL (default value) then version will be determined by
#'   evaluating file structure. The only allowed values currently are
#'   \code{NULL} and \code{"1.3"}. Any other values will trigger an error.
#'  
#' @details
#' Data are loaded using the \code{fread} function in the 
#' \code{data.table} package and timestamps are coerced to POSIXct usign the 
#' \code{fastPOSIXct} function in the \code{fasttime} package. All times must be
#' in UTC timezone per GLATOS standard.
#' 
#' @return A data.frame of class \code{glatos_detections}:
#'
#' @author C. Holbrook (cholbrook@usgs.gov) 
#'
#' @examples
#' #get path to example detection file
#' det_file <- system.file("extdata", 
#'   "walleye_detections.zip", package = "glatos")
#' det_file <- unzip(det_file, "walleye_detections.csv")
#' det <- read_glatos_detections(det_file)
#'
#' @export
read_glatos_detections <- function(det_file, version = NULL) {

  #Read detection file-------------------------------------------------------
  
  #Get version-specific file specifications
  data(glatos_detection_schema)
  
  
  #Identify detection file version
  id_det_version <- function(det_file){
    det_col_names <- names(data.table::fread(det_file, nrows = 0))
    if(all(glatos_detection_schema$v1.3$name == det_col_names)) { 
      return("1.3") 
    } else {
      stop("Detection file version could not be identified.")
    }
  }
  
  if(is.null(version)) {
    version <- id_det_version(det_file)
  } else if (!(paste0("v",version) %in% names(glatos_detection_schema))) {
    stop(paste0("Detection file version ",version," is not supported."))
  }
 
  #-Detections v1.3----------------------------------------------------------------  
  if (version == "1.3") {

    col_classes <- glatos_detection_schema[["v1.3"]]$type
    timestamp_cols <- which(col_classes == "POSIXct")
    date_cols <- which(col_classes == "Date")
    col_classes[c(timestamp_cols, date_cols)] <- "character"
    
    #read data
    dtc <- data.table::fread(det_file, sep = ",", colClasses = col_classes)
    
    #coerce timestamps to POSIXct; note that with fastPOSIXct raw
    #  timestamp must be in UTC; and tz argument sets the tzone attr only
    for (j in timestamp_cols) data.table::set(dtc, j = j, 
                      value = fasttime::fastPOSIXct(dtc[[j]], tz = "UTC"))
    #coerce dates to date
    for (j in date_cols) {
      data.table::set(dtc, j = j, value = ifelse(dtc[[j]] == "", NA, dtc[[j]]))
      data.table::set(dtc, j = j, value = as.Date(dtc[[j]]))
    }
  }
  #-end v1.3----------------------------------------------------------------

  #assign class 
  dtc <- glatos_detections(dtc)
  
  return(dtc)
}
