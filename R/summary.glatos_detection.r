#' Summarize detections by animal and location
#'
#' Summarize first and last detection timestamps and number of detections 
#' of each animal at each location.
#'
#' @param det A data frame containing detection data with two columns 
#'   described below and one column containing a location grouping variable 
#'   specified by \code{location_col}. 
#'   The following two columns must appear in \code{det}: 
#'   \itemize{
#'     \item \code{animal_id} A character string with the name of the column 
#' 		 containing the individual animal identifier.
#'	   \item \code{detection_timestamp_utc} is a character string with the name 
#'	   of the column containing datetime stamps for the detections (MUST be of 
#'	   class 'POSIXct').
#'   }   
#' 
#' @param location_col A character string with the name of the column 
#'   containing the location grouping variable (e.g., "glatos_array") in 
#'   quotes.
#'
#' @param as_matrix A logical indicating if output should be formatted as
#'   a matrix (TRUE) or data frame (FALSE; default).   
#' @param location_vals A character vector with names of all possible values
#'   in \code{location_col}. If \code{NULL} then all unique values in 
#'   \code{det[ , "location_col"]} will be used.
#' 
#' @details The list of unique locations is pasted as a space-separated string. 
#'
#' @return 
#'   \itemize{
#'   \item{If \code{as_matrix = FALSE} (default): A data frame containing four
#'   columns, the animal identifier, the total number of detections for each
#'   animal, the total number of locations at which each animal was detected,
#'   and a space-separated character string containing a list of all unique
#'   locations each fish was detected.}
#'   \item{if \code{as_matrix = TRUE}: A list of three data frames where
#'   the first column in each row contains \code{animal_id} and the remaining 
#'   columns contain data (number of detections, first detection timestamp, 
#'   and last detection timestamp, respectively).}
#'   }
#'
#' @author T. R. Binder and C. Holbrook
#'
#' @examples
#' 
#' #get path to example detection file
#' det_file <- system.file("extdata", "walleye_detections.csv",
#'                          package = "glatos")
#' det <- read_glatos_detections(det_file)
#' 
#' #basic summary
#' ds <- summary(det)
#' 
#' #matrix summary
#' dsm <- summary(det, as_matrix = TRUE)
#' 
#' #specify all possible glatos_arrays from receiver file
#' #get example receiver data
#' 
#' rec_file <- system.file("extdata", "sample_receivers.csv",
#'                          package = "glatos")
#' rec <- read_glatos_receivers(rec_file) 
#' 
#' dsm2 <- summary(det, as_matrix = TRUE, 
#'                 location_vals = sort(unique(rec$glatos_array)))
#' 
#' @export

summary.glatos_detections <- function(det, location_col = "glatos_array",
                                      as_matrix = FALSE, location_vals = NULL){
  
  #coerce to data.table
  dtc <- data.table::as.data.table(det)
  
  #check that required columns exist
  missing_cols <- setdiff(c("animal_id", "detection_timestamp_utc"), 
                          names(dtc))
  if(length(missing_cols) > 0){
    stop(paste0("The following required columns are missing:\n",
                paste(missing_cols, collapse = ", "), "."))
  }
  
  #check that location_col exists
  if(!(location_col %in% names(dtc))){
    stop(paste0("Column ", location_col, " is missing.\n",
      "Double check input argument 'location_col'."))
  }  
  
  #check that detection_timestamp_utc is POSIXct
  if(!(inherits(dtc$detection_timestamp_utc, "POSIXct"))){
    stop("Column 'detection_timestamp_utc' must be of class POSIXct.")
  }  
  
  if(!as_matrix){
    #summarize
    det_sum <- dtc[ , list(first_det = min(detection_timestamp_utc), 
                           last_det  = max(detection_timestamp_utc),
                           num_dets = .N,
                           num_locs = length(unique(.SD[[location_col]])),
                           locations = paste(sort(unique(.SD[[location_col]])), 
                                        collapse = " ")),
                      by = animal_id]
  }
  
  if(as_matrix){
    #get unique values of locs (if NULL) and animals
    if(is.null(location_vals)) location_vals <- 
                                 sort(unique(det[[location_col]]))
    animal_vals <- sort(unique(dtc$animal_id))
    
    #identify row (animal) and column (location) for each record in matrix
    dtc[ , `:=`(tag_row = match(dtc$animal_id, animal_vals),
                loc_col = match(dtc[[location_col]], location_vals))]
    
    #first detection, last, and number of detections for each tag-row combo
    index_sum <- dtc[ , list(first_det = min(detection_timestamp_utc),
                             last_det = max(detection_timestamp_utc),
                             num_dets = .N), 
                  by = c("tag_row", "loc_col")]
    
    #make matrices for storing summaries
    fdtm <- as.data.frame(matrix(NA, nrow = length(animal_vals), 
                                      ncol = length(location_vals)))
    fdtm[, ] <- lapply(fdtm[, ], function(x) as.POSIXct(NA, tz = "UTC"))
    colnames(fdtm) <- location_vals
    #add animal_ids as first row
    fdtm <- data.frame(animal_id = animal_vals, fdtm)
    
    ldtm <- fdtm #make copy for last detection
    ndm <- fdtm #number of detections
    ndm[, 2:ncol(ndm)] <- 0
    
    #add one to column index
    index_sum[ , loc_col := loc_col + 1] 
    
    #insert values
    fdtm[as.matrix(index_sum[ , 1:2])] <- index_sum$first_det
    ldtm[as.matrix(index_sum[ , 1:2])] <- index_sum$last_det    
    ndm[as.matrix(index_sum[ , 1:2])] <- index_sum$num_dets    
    
    #summarize
    det_sum <- list(
      num_dets = ndm,
      first_det = fdtm,
      last_det = ldtm)
  }

  return(det_sum)
}
