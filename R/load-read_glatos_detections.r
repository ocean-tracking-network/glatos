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
#' @param det_version An optional character string with the glatos file version
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
#' @author C. Holbrook \email{cholbrook@usgs.gov} and T. Hayden
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
#' #return data.table object
#' det <- read_glatos_detections(det_file, data.table = TRUE)
#' 
#' #return a subset of rows containing specific values in specific columns
#' det <- read_glatos_detections(det_file, 
#'          select_rows = list(relase_location = "Tattabawassee", 
#'                             glatos_array = c("TTB", "SGR", "SBO")))
#'                             
#'
#' @export


read_glatos_detections <- function(det_file, det_version = NULL, 
  select_rows = NULL, select_type = c("any", "all"), 
  data.table = FALSE, ...) {

 
  #Read detection file-------------------------------------------------------
  
  #see version-specific file specifications
  #internal data object; i.e., in R/sysdata.r
  
  
  #Identify detection file version
  id_det_version <- function(det_file){
    det_col_names <- names(data.table::fread(det_file, nrows = 0))
    if(all(glatos:::glatos_detection_schema$v1.3$name == det_col_names)) { 
      return("1.3") 
    } else {
      stop("Detection file version could not be identified.")
    }
  }
  
  if(is.null(det_version)) {
    det_version <- id_det_version(det_file)
  } else if (!(paste0("v", det_version) %in% 
      names(glatos:::glatos_detection_schema))) {
    stop(paste0("Detection file version ", det_version," is not supported."))
  }

 #-Detections v1.3----------------------------------------------------------------  
  if (det_version == "1.3") {

    col_classes <- glatos:::glatos_detection_schema[["v1.3"]]$type
    timestamp_cols <- which(col_classes == "POSIXct")
    date_cols <- which(col_classes == "Date")
    col_classes[c(timestamp_cols, date_cols)] <- "character"

    #read data
    
    if(is.null(select_rows)){
      dtc <- data.table::fread(det_file, sep = ",", colClasses = col_classes,
                             na.strings = c("", "NA"), ...)
    } else {

      #check row_select_type
      if(!missing(select_type)) { select_type <- match.arg(select_type) }
      else { select_type <- select_type[1] }
      
      #TODO: check column validity in pattern (i.e., names(pattern))
      
      #get column names and indices
      col_names <- names(data.table::fread(det_file, nrows = 0))
      cols_to_match <- match(names(select_rows), col_names)
      
      for(i in 1:length(cols_to_match)){
        #make the pattern part of regex for ith column
        pattern_i <- paste0(select_rows[[i]], collapse = "|")
        grep_string_i <- paste0("^([^,]+,){", cols_to_match[i] - 1, "}(", 
          pattern_i, "),")
        
        #if select rows matching any condition in pattern ("OR"-type match)
        if(select_type == "any"){
          if(i == 1) { 
            grep_string <- grep_string_i 
          } else {
            grep_string <- paste0(grep_string, "|", grep_string_i)
          }
          sys_cmd <- paste("grep -E", shQuote(grep_string), shQuote(det_file))
        } else if(row_select_type == "all") {
          if(i == 1) { 
            sys_cmd <- paste("grep -E", shQuote(grep_string_i), shQuote(det_file))        
          } else {
            sys_cmd <- paste(sys_cmd, "| grep -E", shQuote(grep_string_i))
          }      
        }
      }
    
      if(!"select" %in% names(list(...))){
         cols_to_load <- seq_len(length(col_names))
       } else {
         cols_to_load <- match(select, col_names) 
       }
      
      dtc <- data.table::fread(sys_cmd, ...)
      names(dtc) <- col_names[cols_to_load]
    }
  
    # Coerce timestamp and date columns
    sub_cols <- glatos:::glatos_detection_schema[["v1.3"]]$type[match(
      names(dtc), glatos:::glatos_detection_schema[["v1.3"]]$name)]
    timestamp_cols <- which(sub_cols  == "POSIXct")
    date_cols <- which(sub_cols == "Date")

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

##   # create animal_id if missing but only if animal_id, transmitter_codespace, and
##   # transmitter_id are in dataset...



## cols_present <- all(is.element(c("animal_id", "transmitter_codespace",
##                                  "transmitter_id"), names(dtc)))
  
##   if(cols_present & any(is.na(dtc$animal_id))){
##     anid_na <- is.na(dtc$animal_id)
##     dtc$animal_id[anid_na] <- with(dtc,
##       paste0(transmitter_codespace, "-", transmitter_id))
##     warning(paste0("Some or all values of required column 'animal_id' were ", 
##       "missing so they were created from 'transmitter_codespace' and ",
##       "'transmitter_id'."))
##   }
##   if(!is.element(c("animal_id"), names(dtc))){
##       warning(paste0("Missing 'animal_id' column.  Revise subset to include ",
##                      "'animal_id' or include 'transmitter_codespace' and 'transmitter_id' ",
##                      "in subset to automatically create 'animal_id'"))
##   }
 
  #assign class 
  dtc <- glatos:::glatos_detections(dtc, data.table)

  return(dtc)
}
