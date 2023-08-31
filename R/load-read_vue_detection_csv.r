#' Read detection data exported from Innovasea VUE software
#'
#' Read detection data exported from Innovasea VUE software
#'
#' @param src A character string with path and name of a CSV file produced
#'   containing detection data exported from Innovasea VUE software. If only
#'   file name is given, then the file must be located in the working directory.
#'
#' @param show_progress Optional argument passed to \code{data.table::fread}'s
#'   \code{showProgress}.
#'
#' @details Reading is done via \code{\link[data.table]{fread}}.
#'
#' @details All timestamp columns are assumed to be in UTC.
#'
#' @return A data.frame of class \code{vue_detections}.
#'
#'
#' @author C. Holbrook (cholbrook@@usgs.gov)
#'
#' @examples
#' csv_file <- system.file("extdata",
#'   "VR2W_109924_20110718_1.csv", package="glatos")
#'
#' vue_det <- read_vue_detection_csv(csv_file)
#'
#' @export
read_vue_detection_csv <- function(src, 
                          show_progress = FALSE){
  
  #Check if exists
  if(!file.exists(src)){
    warning("File not found: ", src)
    return()
  }
  
  #Check if looks like VUE export format
  src_header <- data.table::fread(file = src, nrows = 1L, header = FALSE)
  
  vue_detection_cols <- c(
      `Date and Time (UTC)` = "POSIXct",
      Receiver = "character", 
      Transmitter = "character", 
      `Transmitter Name` = "character", 
      `Transmitter Serial` = "character", 
      `Sensor Value` = "numeric", 
      `Sensor Unit` = "character", 
      `Station Name` = "character", 
      Latitude = "numeric", 
      Longitude = "numeric")
  
  missing_cols <- setdiff(names(vue_detection_cols), 
                          src_header[1, ])
  
  if(length(missing_cols) > 0) {
    stop("Input file does not appear to be in VUE Export format.\n\ ",
         "The following columns are missing: \n  ",
         paste(missing_cols, collapse = "\n  "))
  }
  

  #Read each list element separately
  vue_detections <- data.table::fread(
                  file = src, 
                  sep = ",", 
                  na.strings = "",
                  colClasses = vue_detection_cols,
                  header = TRUE,
                  fill = TRUE,
                  showProgress = show_progress)
  
  #Assign class
  vue_detections <- structure(vue_detections, 
                    class = c("vue_detections", 
                              class(vue_detections)))
  
  return(vue_detections)
}  
  