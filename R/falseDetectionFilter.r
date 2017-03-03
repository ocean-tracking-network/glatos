#' False detection filter
#'
#' Identify possible false detections based on "short interval" criteria
#'
#' @param detections A data frame containing detection data (e.g., from the 
#'   standard GLATOS detection export file '*_detectionsWithLocs.csv'). 
#'   Must contain the column \code{min_lag} or equivalent (specified by 
#'   \code{minLagCol}). \code{min_lag} is loosely based on the the 
#'   "short interval" method described by Pincock (2012). In this case 
#'   (GLATOS), it is defined for each detection as the shortest interval 
#'   (in seconds) between either the previous or next detection (whichever 
#'   is closest) of the same transmitter on the same receiver.
#'   
#' @param tf A numeric scalar indicating the time threshold (in seconds; e.g., 
#'   Pincock's (2012) "short interval") for identifying possible false 
#'   detections.
#'   
#' @param verbose  Append intermediately calculated columns to the dataframe 
#'   for debugging purposes
#'
#' @details Detections are identified as potentially false when 
#'   \code{min_lag > tf}.
#' @details 
#' A new column (\code{passedFilter}), indicating if each record (row) passed 
#' the filter, is added to the input data frame. This function was written
#' specifically with GLATOS standard detection export in mind, so it requires
#' \code{min_lag} or equivalent. 
#' @details  
#' A common rule of thumb for choosing tf for VEMCO PPM encoded transmitters 
#'   is 30 times the nominal delay (e.g., 3600 s for a transmitter with a 
#'   120 s nominal delay) - see Pincock (2012).
#'
#' @return A data frame consisting of \code{detections} with an additional 
#'   column 'passedFilter' indicating if each detection did (TRUE) or did not (FALSE) 
#'   pass the criteria.
#'
#' @author T. R. Binder
#'
#' @references
#'   Pincock, D.G., 2012. False detections: what they are and how to remove them 
#'     from detection data. Vemco Division, Amirix Systems Inc., Halifax, 
#'     Nova Scotia.
#'     \cr \url{http://www.vemco.com/pdf/false_detections.pdf}
#' @references
#'   Simpfendorfer, C.A., Huveneers, C., Steckenreuter, A., Tattersall, K., 
#'     Hoenner, X., Harcourt, R. and Heupel, M.R., 2015. Ghosts in the data: 
#'     false detections in VEMCO pulse position modulation acoustic telemetry 
#'     monitoring equipment. Animal Biotelemetry, 3(1), p.55.
#'     \cr \url{https://animalbiotelemetry.biomedcentral.com/articles/10.1186/s40317-015-0094-z}
#'
#' @examples
#' data("walleye_detections") #example data
#' 
#' head(walleye_detections)
#'
#' dtx <- falseDetectionFilter(walleye_detections, 3600)
#' head(dtx)
#' 
#' @export

require(lubridate)
require(dplyr)

falseDetectionFilter <- function(detections, tf=3600, colnames=list(
              locationCol="glatos_array",
              animalCol="animal_id",
              timestampCol="detection_timestamp_utc",
              stationCol='station_no',
              latCol="deploy_lat",
              longCol="deploy_long"),
              verbose=FALSE
              ){
  
  # Check that the specified columns appear in the detections dataframe
  # Fail w/ helpful error otherwise.
  colCheck = unlist(colnames %in% names(detections))
  
  if (! all(colCheck)) {
    missingCols <- colnames[!colCheck]
    stop(paste0("Detections dataframe is missing the following required fields:",
                paste0("\n       ", names(missingCols),": '", missingCols ,"' not found in dataframe", collapse="\n")), 
         call.=FALSE)
    }
  
  out <- detections %>%
    group_by_(colnames$animalCol, colnames$stationCol) %>%  # TODO: still compares end of one to start of next. Do we need group_by if we aren't calcing by group?
    arrange_(colnames$animalCol, colnames$timestampCol) %>%  # Arrange by animal and time.
    mutate_(
      this.date = colnames$timestampCol
    ) %>% 
    mutate(   # Snappy way to peek ahead and behind
      last.date = lag(this.date),
      next.date = lead(this.date)
    ) %>%
    mutate( # Less-snappy way to run a bunch of conditional logic on what's ahead and behind.
      last.diff = ifelse(animal_id == lag(animal_id) & station_no == lag(station_no), suppressWarnings(difftime(this.date, last.date, units="secs")), NA),
      next.diff = ifelse(animal_id == lead(animal_id) & station_no == lead(station_no), suppressWarnings(difftime(next.date, this.date, units="secs")), NA)
    ) %>%  # now need to make the first/last detection calc not-NA and not-true, so for these we'll evaluate only if other side within timebounds.
           # TODO: get smarter about handling NAs as acceptable values of last.diff and next.diff
    replace_na(list(last.diff=tf+as.difftime(1, units="secs"), next.diff=tf+as.difftime(1,units="secs"))
    ) %>% # Now can run the filter step to evaluate
    mutate( 
            # Filter out the duplicated detection events due to double-loading .vrls?
            # NB: This is probably not the place for this extra-step.
            # Manually manipulated this value based on contents of min_lag in source data.
      calc_min_lag = ifelse(pmin(last.diff, next.diff) < 55, pmax(last.diff, next.diff), pmin(last.diff, next.diff)),
      
      # Diagnostic printing: ensuring we aren't comparing different stations or animals as we traverse the list.
      # Couldn't get an exact match to min_lag, something going on that I'm not catching. Sometimes it's max-lag, apparently.
      my_next_station = lead(station_no),
      my_last_station = lag(station_no),
      my_next_animal = lead(animal_id),
      my_last_animal = lag(animal_id),
      
      # Finally, check filter against min_lag
      passedFilter = calc_min_lag <= tf
    )
  
    if (!verbose){
      # TODO: trim out the columns other than passedFilter

    }
  
    return(out)
}