#' @title 
#' Breakdown and summarize GLATOS detections by individual and location.
#' 
#' @description
#' Breakdown and summarize GLATOS detections by individual and location.
#'
#' @param dtc A GLATOS detection data frame (class \code{glatos_detections}).
#'  
#' @param by_col A list with two elements. The first element, named 'individual'
#'   is a vector of character strings containing names of columns in dtc that
#'   together identify each unique animal or group of animals. The second
#'   element, named 'location', is a vector of character strings containing
#'   names of columns in dtc that together identify each unique location or
#'   group of locations.
#'
#' @details
#' TBD
#' 
#' @return A list of two data frames. The first named 'individuals' and the 
#'  second named 'locations'. Each data frame one row per unique combination
#'  of 'location' or 'individual' variables and then the count of records, 
#'  count of unique oppposing groups, and min and max detection timestamps.
#'
#' @author C. Holbrook (cholbrook@usgs.gov) 
#'
#' @examples
#' #get path to example detection file
#' det_file <- system.file("extdata", 
#'   "walleye_detections.zip",package="glatos")
#' det_file <- unzip(det_file, "walleye_detections.csv")
#' det <- read_glatos_detections(det_file)
#' bd <- breakdown(det)
#' bd <- breakdown(det, by_col = list(individual = c("release_location"),
#'                 location = c("glatos_array", "station_no")))
#' bd <- breakdown(det, by_col = list(individual = NULL,
#'                 location = c("glatos_array", "station_no")))
#'                 
#' @export
breakdown <- function(dtc, by_col = list(individual = c("animal_id"),
                              location = c("glatos_array", "station_no"))) {

  dtc <- data.table(dtc)
  tbl_ind <- dtc[, .(det_count = .N, 
                 loc_count = nrow(unique(.SD[,by_col$location, with = FALSE])),
                 first_det = min(detection_timestamp_utc),
                 last_detc = max(detection_timestamp_utc)
                      ), 
                      by = eval(by_col$individual)]
  setkeyv(tbl_loc, by_col$animal_id) 
  tbl_loc <- dtc[, .(ind_count = nrow(unique(.SD[,by_col$individual, with = FALSE])),
                     det_count = .N, 
                     first_det = min(detection_timestamp_utc),
                     last_det = max(detection_timestamp_utc)
                     ), 
                 by = eval(by_col$location)]
  setkeyv(tbl_loc, by_col$location)
  
  return(list(individual = data.frame(tbl_ind),
              location = data.frame(tbl_loc)))
}

