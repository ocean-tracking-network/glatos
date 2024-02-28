#' Summarize detections by animal, location, or both
#'
#' Calculate number of fish detected, number of detections, first and last 
#' detection timestamps, and/or mean location of receivers or groups, 
#' depending on specific type of summary requested.
#'
#'@param det A \code{glatos_detections} object (e.g., produced by
#'  \link{read_glatos_detections}).
#'  
#'  \emph{OR} a data frame containing detection
#'  data with four columns described
#'  below and one column containing a location grouping variable, whose name is
#'  specified by \code{location_col} (see below). 
#'  
#'  The following four columns must appear in \code{det}, 
#'  except \code{deploy_lat} and \code{deploy_lon} are not needed 
#'    if \code{receiver_locs} is specified: 
#'  \describe{ 
#'  \item{\code{animal_id}}{Individual animal
#'  identifier; character.} 
#'  \item{\code{detection_timestamp_utc}}{ Timestamps for
#'  the detections (MUST be of class 'POSIXct').}
#'	\item{\code{deploy_lat}}{Latitude of receiver deployment in decimal 
#'  degrees, NAD83.}
#'	\item{\code{deploy_long}}{Longitude of receiver deployment in decimal 
#'  degrees, NAD83.}
#'  }
#'  
#' @param location_col A character string indicating the column name in
#'   \code{det} (and \code{receiver_locs} if specified) that will be used as the
#'   location grouping variable (e.g. "glatos_array"), in quotes.
#'   
#' @param receiver_locs An optional data frame containing receiver data with the
#'   two columns ('deploy_lat', 'deploy_long') described below and one column
#'   containing a location grouping variable, whose name is specified by
#'   \code{location_col} (see above). 
#'   The following two columns must appear in \code{receiver_locs}: 
#'   \itemize{
#'     \item \code{deploy_lat} Latitude of receiver deployment in decimal 
#'      degrees, NAD83.
#'     \item \code{deploy_long} Longitude of receiver deployment in decimal 
#'      degrees, NAD83.
#'   }   
#'   
#' @param animals A character vector with values of 'animal_id' that will be
#'   included in summary. This allows (1) animals \emph{not} detected (i.e.,
#'   not present in \code{det}) to be included in the summary and/or (2)
#'   unwanted animals in \code{det} to be excluded from the summary.
#'
#' @param summ_type A character string indicating the primary focus of 
#'   the summary. Possible values are \code{"animal"} (default), 
#'   \code{"location"}, and \code{"both"}. See Details below.
#' 
#' @details Input argument \code{summ_type} determines which of three possible
#'   summaries is conducted. If \code{summ_type = "animal"} (default), the
#'   output summary includes the following for each unique value of
#'   \code{animal_id}: number of unique locations (defined by unique values of
#'   \code{location_col}), total number of detections across all locations,
#'   timestamp of first and last detection across all locations, and a
#'   space-delimited string showing all locations where each animal was
#'   detected. If \code{summ_type = "location"}, the output summary includes the
#'   following for each unique value of \code{location_col}: number of animals
#'   (defined by unique values of \code{animal_id}), total number of detections
#'   across all animals, timestamp of first and last detection across all
#'   animals, mean latitude and longitude of each location group, and a
#'   space-delimited string of each unique animal that was detected. If
#'   \code{summ_type = "both"}, the output summary includes the following for
#'   each unique combination of \code{location_col} and \code{animal_id}: total
#'   number of detections, timestamp of first and last detection, and mean
#'   latitude and longitude.
#' 
#' @details If \code{receiver_locs = NULL} (default), then mean latitude and 
#'   longitude of each location (\code{mean_lat} and \code{mean_lon} in 
#'   output data frame) will be calculated from data in \code{det}. Therefore, 
#'   mean locations in the output summary may not represent the mean among
#'   all receiver stations in a particular group if detections did not occur 
#'   on all receivers in each group. However, when actual receiver locations 
#'   are specified by \code{receiver_locs}, then \code{mean_lat} and 
#'   \code{mean_lon} will be calculated from \code{receiver_locs}. Also, if mean
#'   location is not desired or suitable, then \code{receiver_locs} can 
#'   be used to pass a single user-specified \code{deploy_lat} and 
#'   \code{deploy_long} for each unique value of \code{location_col}, whose 
#'   values would then represent \code{mean_lat} and \code{mean_lon} in 
#'   the output summary. 
#'   
#' @return 
#'  If \code{summ_type = "animal"} (default): A data frame, data.table, or 
#'  tibble containing six columns:
#'   \itemize{
#'     \item{\code{animal_id}: described above.}
#'     \item{\code{num_locs}: number of locations.}
#'     \item{\code{num_dets}: number of detections.}
#'     \item{\code{first_det}: first detection timestamp.}
#'     \item{\code{last_det}: last detections timestamp.}
#'     \item{\code{locations}: character string with 
#'       locations detected, separated by spaces.}
#'   }
#'  If \code{summ_type = "location"}: A data frame, data.table, or
#'  tibble containing eight columns:
#'   \itemize{
#'     \item{\code{LOCATION_COL}: defined by \code{location_col}.}
#'     \item{\code{num_fish}: number of unique animals detected.}
#'     \item{\code{num_dets}: number of detections.}
#'     \item{\code{first_det}: first detection timestamp.}
#'     \item{\code{last_det}: last detections timestamp.}
#'     \item{\code{mean_lat}: mean latitude of receivers at this location.}
#'     \item{\code{mean_lon}: mean longitude of receivers at this location.}
#'     \item{\code{animals}: character string with animal_ids detected,
#'     separated by spaces.}
#'   }
#'  If \code{summ_type = "both"}: A data frame, data.table, or tibble
#'  containing seven columns:
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
#'  #Basic summaries
#'  
#'  # by animal
#'  ds <- summarize_detections(det)
#'  
#'  # by location 
#'  ds <- summarize_detections(det, summ_type = "location")
#'  
#'  # by animal and location
#'  ds <- summarize_detections(det, summ_type = "both")
#'  
#'  
#'  #Include user-defined location_col
#'  
#'  # by animal
#'  det$some_place <- ifelse(grepl("^S", det$glatos_array), "s", "not_s")
#'  
#'  ds <- summarize_detections(det, location_col = "some_place")
#'  
#'  # by location 
#'  ds <- summarize_detections(det, location_col = "some_place", 
#'                             summ_type = "location")
#'  
#'  # by animal and location
#'  ds <- summarize_detections(det, location_col = "some_place", 
#'                             summ_type = "both")  
#'                             
#'                             
#'  #Include locations where no animals detected
#'  
#'  #get example receiver data
#'  rec_file <- system.file("extdata", "sample_receivers.csv",
#'    package = "glatos")
#'  rec <- read_glatos_receivers(rec_file) 
#'  
#'  ds <- summarize_detections(det, receiver_locs = rec, summ_type = "location")
#'  
#'  
#'  #Include animals that were not detected
#'  #get example animal data from walleye workbook
#'  wb_file <- system.file("extdata", "walleye_workbook.xlsm",
#'    package = "glatos")
#'  wb <- read_glatos_workbook(wb_file) 
#'  
#'  ds <- summarize_detections(det, animals = wb$animals, summ_type = "animal")
#'  
#'  #Include by animals and locations that were not detected
#'  ds <- summarize_detections(det, receiver_locs = rec, animals = wb$animals, 
#'    summ_type = "both")
#' 
#' @export



summarize_detections <- function(det, location_col = "glatos_array", 
                                 receiver_locs = NULL, animals = NULL, 
                                 summ_type = "animal"){
  
  #coerce to data.table
  dtc <- data.table::as.data.table(det)
  
  #check 'summ_type'
  if(!(summ_type %in% c("animal", "location", "both"))) stop(paste0("invalid",
     "summary type ('summ_type'); must be 'animal', 'location', or 'both'."))
  
  #check that required columns exist in detections
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
    
  if(summ_type == "location"){
    #summarize fish detections
    loc_summary <- dtc[ , list(num_fish = length(unique(.SD$animal_id)), 
                               num_dets = .N,
                               first_det = min(detection_timestamp_utc),
                               last_det = max(detection_timestamp_utc),
                               animals = paste(sort(unique(.SD[["animal_id"]])),
                                               collapse = " ")), 
                               by = location_col]
    
    #add mean locations
    loc_summary <- merge(loc_summary, mean_locs, by = location_col, all.y = T)
    
    loc_summary[ is.na(num_fish), `:=`(num_fish = 0, num_dets = 0)]
    
    #reorder columns
    data.table::setcolorder(loc_summary, c(setdiff(names(loc_summary), "animals"), 
                               "animals"))
    
    data.table::setkeyv(loc_summary, location_col)
    
    det_sum <- loc_summary
  } 

  if(summ_type == "animal"){
    #summarize fish detections
    anim_summary <- dtc[ , list(num_locs = data.table::uniqueN(location_col),
                                num_dets = .N,
      first_det = min(detection_timestamp_utc),
      last_det = max(detection_timestamp_utc),
      locations = paste(sort(unique(dtc[[location_col]])), collapse = " ")),
      by = animal_id]
    
    #add animals not detected
    anim_summary <- merge(anim_summary, 
       data.table::data.table(animal_id = animals), by = "animal_id", all.y = TRUE)
    
    anim_summary[ is.na(num_locs), `:=`(num_locs = 0, num_dets = 0)]
    
    data.table::setkey(anim_summary, "animal_id")

    det_sum <- anim_summary   
  }     
    
  if(summ_type == "both"){
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

    det_sum <- both_summary
  }   

  #return data.table if input class data.table
  if(inherits(det, "data.table")) return(det_sum)
  
  #return tibble if input class tibble
  if(inherits(det, "tbl")) return(tibble::as_tibble(det_sum))  
  
  return(as.data.frame(det_sum))
}
