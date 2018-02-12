#' @title 
#' Read data from a GLATOS receiver location file
#' 
#' @description
#' Read data from a standard GLATOS receiver location (csv) file and return a 
#' data.frame of class \code{glatos_receivers}.
#'
#' @param rec_file A character string with path and name of receiver location
#'   file in standard GLATOS format (*.csv). If only file name is given, then
#'   the file must be located in the working directory.
#'  
#' @param version An optional character string with the glatos file version
#'   number. If NULL (default value) then version will be determined by
#'   evaluating file structure. The only allowed values currently are
#'   \code{NULL} and \code{"1.0"}. Any other values will trigger an error.
#'  
#' @details 
#' Data are loaded using the \code{fread} function in the
#' \code{data.table} package and timestamps are coerced to POSIXct using the
#' \code{fastPOSIXct} function in the \code{fasttime} package. All timestamps
#' must be in UTC timezone per GLATOS standard.
#' 
#' @return A data.frame of class \code{glatos_receivers}:
#'
#' @author C. Holbrook (cholbrook@usgs.gov) 
#'
#' @examples
#' #get path to example receiver_locations file
#' rec_file <- system.file("extdata", 
#'   "receiver_locations_2011.csv", package = "glatos")
#' rcv <- read_glatos_receivers(rec_file)
#'
#' @export
read_glatos_receivers <- function(rec_file, version = NULL) {

  #Read source file-------------------------------------------------------
  
  #see version-specific file specifications
  #internal data object; i.e., in R/sysdata.r
  
  
  #Identify file version
  id_file_version <- function(rec_file){
    col_names <- names(data.table::fread(rec_file, nrows = 0))
    if(all(glatos_receivers_schema$v1.0$name == col_names)) { 
      return("1.0") 
    } else {
      stop("Receiver location file version could not be identified.")
    }
  }
  
  if(is.null(version)) {
    version <- id_file_version(rec_file)
  } else if (!(paste0("v",version) %in% 
               names(glatos_receivers_schema))) {
    stop(paste0("Receiver locations file version ", version,
                " is not supported."))
  }
 
  #-v1.0----------------------------------------------------------------  
  if (version == "1.0") {

    col_classes <- glatos_receivers_schema[["v1.0"]]$type
    timestamp_cols <- which(col_classes == "POSIXct")
    date_cols <- which(col_classes == "Date")
    col_classes[c(timestamp_cols, date_cols)] <- "character"
    
    #read data
    rec <- data.table::fread(rec_file, sep = ",", colClasses = col_classes)
    
    #coerce timestamps to POSIXct; note that with fastPOSIXct raw
    #  timestamp must be in UTC; and tz argument sets the tzone attr only
    for (j in timestamp_cols) set(rec, j = j, 
                      value = fasttime::fastPOSIXct(rec[[j]], tz = "UTC"))
    #coerce dates to date
    for (j in date_cols) {
      set(rec, j = j, value = ifelse(rec[[j]] == "", NA, rec[[j]]))
      set(rec, j = j, value = as.Date(rec[[j]]))
    }
  }
  #-end v1.0----------------------------------------------------------------
  
  #assign class 
  rec <- glatos_receivers(rec)
  
  return(rec)
}
