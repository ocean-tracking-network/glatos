#' Summarize detections by animal and location
#'
#' Summarize first and last detection timestamps and number of detections 
#' of each animal at each receiver location or each receiver location, or 
#' both.
#'
#' @param det A data frame containing detection data with the two columns 
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
#' @param receiver_locs A data frame containing receiver data with the three 
#'   columns 
#'   described below and one column containing the location grouping variable 
#'   specified by \code{location_col}. 
#'   The following three columns must appear in \code{receiver_locs}: 
#'   \itemize{
#'     \item \code{deploy_lat} Latitude of receiver deployment in decimal 
#'      degrees, NAD83.
#'     \item \code{deploy_lon} Longitude of receiver deployment in decimal 
#'      degrees, NAD83.
#'   }   
#'   
#' @param animals A character string with the unique values of "animal_id" 
#'   or a data frame containing the column named 'animal_id'.
#'
#' @param type A character string indicating the primary variable to be 
#'   summarized. Possible values are \code{"animal"} (default), 
#'   \code{"location"}, and \code{"both"}.
#' 
#' @details If \code{receiver_locs} is specified, then \code{mean_lat} and 
#'   \code{mean_lon} in output will be calculated from those data; otherwise, 
#'   they will be calculated from \code{det}.
#'
#' @return 
#'  If \code{type = "animal"} (default): A data frame containing six
#'   columns:
#'   \itemize{
#'     \item{\code{animal_id}: described above.}
#'     \item{\code{num_locs}: number of locations.}
#'     \item{\code{num_dets}: number of detections.}
#'     \item{\code{first_det}: first detection timestamp.}
#'     \item{\code{last_det}: last detections timestamp.}
#'     \item{\code{locations}: space-delimited character string with 
#'       locations detected.}
#'   }
#'  If \code{type = "location"} (default): A data frame containing six
#'   columns:
#'   \itemize{
#'     \item{\code{LOCATION_COL}: defined by \code{location_col}.}
#'     \item{\code{num_fish}: number of unique animals detected.}
#'     \item{\code{num_dets}: number of detections.}
#'     \item{\code{first_det}: first detection timestamp.}
#'     \item{\code{last_det}: last detections timestamp.}
#'     \item{\code{mean_lat}: mean latitude of receivers at this location.}
#'     \item{\code{mean_lon}: mean longitude of receivers at this location.}
#'   }
#'  If \code{type = "both"} (default): A data frame containing six
#'   columns:
#'   \itemize{
#'     \item{\code{animal_id}: described above.}
#'     \item{\code{LOCATION_COL}: defined by \code{location_col}.}
#'     \item{\code{num_dets}: number of detections.}
#'     \item{\code{first_det}: first detection timestamp.}
#'     \item{\code{last_det}: last detections timestamp.}
#'     \item{\code{mean_lat}: mean latitude of receivers at this location.}
#'     \item{\code{mean_lon}: mean longitude of receivers at this location.}
#'   }
#'   
#'
#' @author T. R. Binder and C. Holbrook
#'
#' @examples
#' 
#' #get path to example detection file
#'  det_file <- system.file("extdata", "walleye_detections.csv",
#'    package = "glatos")
#'  det <- read_glatos_detections(det_file)
#'  
#'  #basic summary for each animal
#'  ds <- summarize_detections(det)
#'  
#'  #basic summary for each location
#'  ds <- summarize_detections(det, type = "location")
#'  
#'  #basic summary for each animal-location combination
#'  ds <- summarize_detections(det, type = "both")
#'  
#'  
#'  #add receivers
#'  #get example receiver data
#'  
#'  rec_file <- system.file("extdata", "sample_receivers.csv",
#'    package = "glatos")
#'  rec <- read_glatos_receivers(rec_file) 
#'  
#'  ds <- summarize_detections(det, receiver_locs = rec, type = "location")
#'  
#'  
#'  #add animals
#'  #get example animal data from walleye workbook
#'  wb_file <- system.file("extdata", "walleye_workbook.xlsm",
#'    package = "glatos")
#'  wb <- read_glatos_workbook(wb_file) 
#'  
#'  ds <- summarize_detections(det, animals = wb$animals, type = "animal")
#'  ds <- summarize_detections(det, receiver_locs = rec, animals = wb$animals, 
#'    type = "both")
#' 
#' @export

summarize_detections <- function(det, location_col = "glatos_array", 
                                 receiver_locs = NULL, animals = NULL, 
                                 type = "animal"){
  
  #coerce to data.table
  dtc <- data.table::as.data.table(det)
  
  #check 'type'
  if(!(type %in% c("animal", "location", "both"))) stop(paste0("invalid 'type'", 
     " argument; must be 'animal', 'location', or 'both'."))
  
  #check that required columns exist in detectsions
  missing_cols <- setdiff(c("animal_id", "detection_timestamp_utc"), names(dtc))
  if(length(missing_cols) > 0){
    stop(paste0("The following required columns are missing:\n",
                paste(missing_cols, collapse = ", "), "."))
  }
  
  #check that location_col exists in detections
  if(!(location_col %in% names(dtc))){
    stop(paste0("Column ", location_col, " is missing in 'det'.\n",
      "Double check input argument 'location_col'."))
  }  
  
  #check that detection_timestamp_utc is POSIXct
  if(!(inherits(dtc$detection_timestamp_utc, "POSIXct"))){
    stop("Column 'detection_timestamp_utc' in 'dtc' must be of class POSIXct.")
  }  

  if(!is.null(receiver_locs)){
    
    #check that location_col exists in receiver locations
    if(!(location_col %in% names(receiver_locs))){
      stop(paste0("Column ", location_col, " is missing in 'receiver_locs'.\n",
        "Double check input argument 'location_col'."))
    } 
    rcv <- data.table::as.data.table(receiver_locs)
    
    #get mean receiver locations from receiver_locs
    mean_locs <- rcv[ , list(mean_lat = mean(deploy_lat), 
                            mean_lon = mean(deploy_long)), 
                            by = location_col]
  } else {
    #get mean receiver locations from dtc
    mean_locs <- dtc[ , list(mean_lat = mean(deploy_lat), 
      mean_lon = mean(deploy_long)), 
      by = location_col]    
  }
  
  if(!is.null(animals)){
    
    #read animal_id vector from data frame if passed as data frame
    if(is.data.frame(animals) & "animal_id" %in% names(animals)) {
      animals <- sort(unique(animals$animal_id))
    }
  } else { animals <- sort(unique(dtc$animal_id)) }
    
  if(type == "location"){
    #summarize fish detections
    loc_summary <- dtc[ , list(num_fish = length(unique(.SD$animal_id)), 
                                 num_dets = .N,
                                 first_det = min(detection_timestamp_utc),
                                 last_det = max(detection_timestamp_utc)), 
                                 by = location_col]
    
    #add mean locations
    loc_summary <- merge(loc_summary, mean_locs, by = location_col, all.y = T)
    
    loc_summary[ is.na(num_fish), `:=`(num_fish = 0, num_dets = 0)]
    
    data.table::setkeyv(loc_summary, location_col)
    
    det_sum <- as.data.frame(loc_summary)
  } 

  if(type == "animal"){
    #summarize fish detections
    anim_summary <- dtc[ , list(num_locs = length(unique(.SD[[location_col]])), 
                                num_dets = .N,
      first_det = min(detection_timestamp_utc),
      last_det = max(detection_timestamp_utc),
      locations = paste(sort(unique(.SD[[location_col]])), collapse = " ")),
      by = animal_id]
    
    #add animals not detected
    anim_summary <- merge(anim_summary, 
       data.table::data.table(animal_id = animals), by = "animal_id", all.y = TRUE)
    
    anim_summary[ is.na(num_locs), `:=`(num_locs = 0, num_dets = 0)]
    
    data.table::setkey(anim_summary, "animal_id")

    det_sum <- as.data.frame(anim_summary)    
  }     
    
  if(type == "both"){
    #summarize fish detections
    both_summary <- dtc[ , list(num_dets = .N, 
                            first_det = min(detection_timestamp_utc),
                            last_det = max(detection_timestamp_utc)), 
                            by = c("animal_id", location_col)]
    
    #add animal-location combinations not present in dtc
    combos <- data.table::as.data.table(expand.grid(animals, 
                                                    mean_locs[[location_col]]))
    names(combos) <- c("animal_id", location_col)
    both_summary <- merge(both_summary, combos, by = c("animal_id", 
                                              location_col), all.y = TRUE)
  
    #add mean locations
    both_summary <- merge(both_summary, mean_locs, by = location_col, all.y = T)
    
    both_summary[ is.na(num_dets), `:=`(num_dets = 0)]
    
    both_summary <- both_summary[ , c(2, 1, 3:ncol(both_summary)), with = FALSE]    
    data.table::setkeyv(both_summary, c("animal_id", location_col))

    det_sum <- as.data.frame(both_summary)
  }   

  return(det_sum)
}
