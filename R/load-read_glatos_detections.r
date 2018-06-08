#' @title 
#' Read data from a GLATOS detection file
#' 
#' @description Read data from a standard GLATOS detection (csv) file and return
#' a data.frame of class \code{glatos_detections}.
#'
#' @param det_file A character string with path and name of detection file in
#'   standard GLATOS format (*.csv). If only file name is given, then the file
#'   must be located in the working directory. File must be a standard GLATOS
#'   file (e.g., \emph{xxxxx_detectionsWithLocs_yyyymmdd_hhmmss.csv}) submitted
#'   via GLATOSWeb Data Portal \url{http://glatos.glos.us}.
#'  
#' @param version An optional character string with the glatos file version
#'   number. If NULL (default value) then version will be determined by
#'   evaluating file structure. The only allowed values currently are
#'   \code{NULL} and \code{"1.3"}. Any other values will trigger an error.
#'  
#' @details
#' Data are loaded using \link[data.table]{fread} and timestamps are coerced to
#' POSIXct using \link[fasttime]{fastPOSIXct}. All times must be in UTC timezone
#' per GLATOS standard.
#' 
#' @details Column \code{animal_id} is considered a required column by many
#'   other functions in this package, so it will be created if any records are
#'   \code{NULL}. When created, it will be constructed from
#'   \code{transmitter_codespace} and \code{transmitter_id}, separated by '-'.
#' 
#' @return A data.frame of class \code{glatos_detections}.
#'
#' @author C. Holbrook \email{cholbrook@usgs.gov}
#'
#' @examples
#' #get path to example detection file
#' det_file <- system.file("extdata", "walleye_detections.csv",
#'                          package = "glatos")
#'                          
#' #note that code above is needed to find the example file
#' #for real glatos data, use something like below
#' #det_file <- "c:/path_to_file/HECWL_detectionsWithLocs_20150321_132242.csv"           
#'                          
#' det <- read_glatos_detections(det_file)
#'
#' @export
read_glatos_detections <- function(det_file, version = NULL) {

  #Read detection file-------------------------------------------------------
  
  #see version-specific file specifications
  #internal data object; i.e., in R/sysdata.r
  
  
  #Identify detection file version
  id_det_version <- function(det_file){
    det_col_names <- names(data.table::fread(det_file, nrows = 0))
    if(all(glatos:::glatos_detection_schema$v1.3$name %in% det_col_names)) { 
      return("1.3") 
    } else {
      stop("Detection file version could not be identified.")
    }
  }
  
  if(is.null(version)) {
    version <- id_det_version(det_file)
  } else if (!(paste0("v",version) %in% 
      names(glatos:::glatos_detection_schema))) {
    stop(paste0("Detection file version ", version," is not supported."))
  }
 
  #-Detections v1.3----------------------------------------------------------------  
  if (version == "1.3") {

    col_classes <- glatos:::glatos_detection_schema[["v1.3"]]$type
    timestamp_cols <- which(col_classes == "POSIXct")
    date_cols <- which(col_classes == "Date")
    col_classes[c(timestamp_cols, date_cols)] <- "character"
    
    #read data
    dtc <- data.table::fread(det_file, sep = ",", colClasses = col_classes,
                             na.strings = c("", "NA"))
    
    #coerce timestamps to POSIXct; note that with fastPOSIXct raw
    #  timestamp must be in UTC; and tz argument sets the tzone attr only
    for (j in timestamp_cols) data.table::set(dtc, 
                                  j = glatos_detection_schema[["v1.3"]]$name[j], 
                      value = fasttime::fastPOSIXct(
                           dtc[[glatos_detection_schema[["v1.3"]]$name[j]]], 
                                tz = "UTC"))
    #coerce dates to date
    for (j in date_cols) {
      data.table::set(dtc, j = glatos_detection_schema[["v1.3"]]$name[j], 
        value = ifelse(dtc[[glatos_detection_schema[["v1.3"]]$name[j]]] == "", 
                       NA, 
                       dtc[[glatos_detection_schema[["v1.3"]]$name[j]]]))
      data.table::set(dtc, j = glatos_detection_schema[["v1.3"]]$name[j], 
        value = as.Date(dtc[[glatos_detection_schema[["v1.3"]]$name[j]]]))
    }
  }
  #-end v1.3----------------------------------------------------------------

  #create animal_id if missing
  anid_na <- is.na(dtc$animal_id)
  if(any(anid_na)){
    dtc$animal_id[anid_na] <- with(dtc, 
      paste0(transmitter_codespace, "-", transmitter_id))
    warning(paste0("Some or all values of required column 'animal_id' were ", 
      "missing so they were created from 'transmitter_codespace' and ",
      "'transmitter_id'.)"))
  }
  
  #assign class 
  dtc <- glatos:::glatos_detections(dtc)
  
  return(dtc)
}
