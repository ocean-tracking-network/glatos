#' @title 
#' Summarize GLATOS detections by animal_id, glatos_array, or both.
#' 
#' @description
#' Summarize GLATOS detections by animal_id, glatos_array, or both.
#'
#' @param dtc A GLATOS detection data frame.
#'  
#' @param by_col Character string or vector containing names of columns
#'   in dtc to group the count by. Result is  
#'
#' @details
#' TBD
#' 
#' @return A data frame with columns listed in by_col and count of detections
#'   in dtc corresponding to each unique by_col combination.
#'
#' @author C. Holbrook (cholbrook@usgs.gov) 
#'
#' @examples
#' #get path to example detection file
#' det_file <- system.file("extdata", 
#'   "walleye_detections.zip",package="glatos")
#' det_file <- unzip(det_file, "walleye_detections.csv")
#' det <- read_glatos_detections(det_file)
#' summarize_by_fish(det)
#' summarize_by_fish(det, by = "transmitter_id")
#' summarize_by_fish(det, by = c("release_location", "sex", "glatos_array"))
#'
#' @export
summarize_by_fish <- function(dtc, by_col = c("animal_id","glatos_array")) {

  dtc <- data.table(dtc)
  tbl <- dtc[, .N, by = by_col]
  return(data.frame(tbl))
}

