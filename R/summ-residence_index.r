#' Generate the residence index from a set of detections
#'
#' This residence index tool will take condensed detection event data (from
#' \code{\link{detection_events}} and caculate the residence index for each
#' location. The information passed to the function is
#' what is used to calculate the residence index, make sure you are only passing
#' the data you want taken into consideration for the residence index (i.e.
#' species, stations, tags, etc.).
#'
#' @references 
#' Kessel, S.T., Hussey, N.E., Crawford, R.E., Yurkowski, D.J., O'Neill, C.V.
#' and Fisk, A.T., 2016. Distinct patterns of Arctic cod (\emph{Boreogadus
#' saida}) presence and absence in a shallow high Arctic embayment, revealed
#' across open-water and ice-covered periods through acoustic telemetry. Polar
#' Biology, 39(6), pp.1057-1068.
#' \url{https://www.researchgate.net/publication/279269147}
#'
#' @param detections A data.frame from the \code{\link{detection_events}}
#'   function.
#'   
#' @param calculation_method A character string with the calculation method
#'   using one of the following: \code{kessel}, \code{time_interval}, 
#'   \code{timedelta}, \code{aggregate_with_overlap}, or 
#'   \code{aggregate_no_overlap}.
#'   
#' @param locations An optional data frame that identifies all unique 
#' locations where RI will be calculated. Three columns required:
#' \describe{
#'   \item{location}{Character string with unique location identifier.}
#'   \item{mean_longitude}{Location longitude (for mapping).}
#'   \item{mean_latitude}{Location latitude (for mapping).}
#' }
#' 
#' If \code{locations = NULL} (default value) then RI will only be 
#' calculated at locations present in \code{detections$location}.
#'   
#' @param group_col Optional character string (can be multiple) that identifies 
#' additional grouping variables for RI calculations. The default value 
#' (\code{group_col = "animal_id"}) will calculate and return RI for each 
#'  animal at each location (i.e., for each unique combination of 
#'  \code{location} and \code{animal_id}. If \code{group_col = NULL} then 
#'  RI will be calculated by location only (will not account for animal or 
#'  any other variable).
#'   
#' @param time_interval_size Character string with size of the time interval 
#' used when \code{calculation_method = "time_interval"}. This is passed to 
#' \link[base]{seq.Date}'s \code{by} argument, so must meet the requirements of 
#' that argument for that function (e.g., "1 day", "4 hours", etc.). Default is 
#' \code{"1 day"}.
#' 
#' @param groupwise_total Logical that determines how the denominator is 
#' calculated in RI. If FALSE (default) then the denominator represents the 
#' total number of time intervals or time (depending on calculation method) 
#' among all records. Otherwise (if FALSE), the denominator 
#' represents the total number of time intervals or time within each 
#' group level (e.g., for each animal if \code{group_col = "animal_id"}.
#'
#' @details The \strong{kessel} method converts both the \code{first_detection}
#' and \code{last_detection} columns into a date with no hours, minutes, or
#' seconds. Next it creates a list of the unique days where a detection was
#' seen. The size of the list is returned as the total number of days as an
#' integer. This calculation is used to determine the total number of distinct
#' days (T) and the total number of distinct days per location (S). Possible
#' rounding error may occur as a detection on 2016-01-01 23:59:59 and a
#' detection on 2016-01-02 00:00:01 would be counted as two days when it is
#' really 2-3 seconds.
#'
#' \deqn{ RI = S/T}
#' \deqn{ RI = Residence Index}
#' \deqn{S = Distinct number of days detected at the location}
#' \deqn{T = Distinct number of days detected at any location}
#'
#' @details The \strong{time_interval} calculation method determines the
#'   number of time intervals (size determined by \code{time_interval_size}
#'   argument) in which detections occurred at each \code{location} and as a 
#'   fraction of the number of time intervals in which detections occurred 
#'   among all sites. For each location, residency index (RI) is calculated:
#'
#' \deqn{ RI = L/T}
#' \deqn{ RI = Residence Index}
#' \deqn{L = Distinct number of time intervals in which detection observed at 
#'           this location}
#' \deqn{T = Distinct number of time intervals in which detection observed at 
#'           any location}
#'           
#'   For consistency with other \code{calculation_method}s, 
#'   the L and T are not reported, but are converted cumulative time covered in 
#'   days and reported in columns \code{days_detected} and \code{total_days}.
#' 
#'
#' @details The \strong{timedelta} calculation method determines the first
#' detection and the last detection of all detections. The time difference is
#' then taken as the values to be used in calculating the residence index. The
#' timedelta for each station is divided by the timedelta of the array to
#' determine the residence index.
#'
#' \deqn{
#' RI = Delta S/Delta T}
#' 
#' \deqn{RI = Residence Index}
#' 
#' \deqn{Delta S = Last detection time at the location - First detection time at the location}
#' 
#' \deqn{Delta T = Last detection time at any location - First detection time at any location}
#' 
#'
#' @details
#' The \strong{aggregate_with_overlap} calculation method takes the length of time of each
#' detection and sums them together. A total is returned. The sum for each location
#' is then divided by the sum among all locations to determine the residence index.
#'
#' \deqn{
#' RI = AwOS/AwOT}
#' 
#' \deqn{RI = Residence Index}
#' 
#' \deqn{AwOS = Sum of length of time of each detection at the location}
#' 
#' \deqn{AwOT = Sum of length of time of each detection among all locations}
#' 
#'
#' @details 
#' The \strong{aggregate_no_overlap} calculation method takes the length of time of each
#' detection and sums them together. However, any overlap in time between one or
#' more detections is excluded from the sum. For example, if the first detection
#' is from \code{2016-01-01 01:02:43} to \code{2016-01-01 01:10:12} and the second
#' detection is from \code{2016-01-01 01:09:01 }to \code{2016-01-01 01:12:43}, then the
#' sum of those two detections would be 10 minutes. A total is returned once all
#' detections of been added without overlap. The sum for each location is then
#' divided by the sum among all locations to determine the residence index.
#'
#' \deqn{
#' RI = AnOS/AnOT}
#' 
#' \deqn{RI = Residence Index}
#' 
#' \deqn{AnOS = Sum of length of time of each detection at the location, excluding any overlap}
#' 
#' \deqn{AnOT = Sum of length of time of each detection among all locations, excluding any overlap}
#' 
#' 
#' @examples
#' #get path to example detection file
#' det_file <- system.file("extdata", "walleye_detections.csv",
#'                          package = "glatos")
#' det <- read_glatos_detections(det_file)
#' detection_events <- glatos::detection_events(det)
#' rik_data <- glatos::residence_index(detection_events, calculation_method = 'kessel')
#' rit_data <- glatos::residence_index(detection_events, calculation_method = 'timedelta')
#' riawo_data <- glatos::residence_index(detection_events, calculation_method = 'aggregate_with_overlap')
#' riano_data <- glatos::residence_index(detection_events, calculation_method = 'aggregate_no_overlap')
#'
#' @return A data.frame of days_detected, residency_index, location,
#' mean_latitude, mean_longitude
#' 
#' 
#' @author A. Nunes, \email{anunes@dal.ca}
#'
#' @importFrom dplyr count distinct select
#' @export
residence_index <- function(detections, calculation_method='kessel', 
  locations = NULL, group_col = "animal_id", time_interval_size = "1 day", 
  groupwise_total = FALSE) {
  
  if(is.na(group_col)) group_col <- NULL #set to NULL if NA

  #get locations from detections if not given
  if(is.null(locations)){
    locs <- dplyr::select(detections, location, mean_latitude, mean_longitude)
  }
  if(!is.null(locations) & inherits(locations, "data.frame")){
    
    # Check that the required columns appear in the detections data frame
    req_cols <- c("location", "mean_latitude", "mean_longitude")
    missingCols <- setdiff(req_cols, names(locations))
    if (length(missingCols) > 0){
      stop(paste0("det is missing the following ",
        "column(s):\n", paste0("       '", missingCols, "'", collapse="\n")), 
        call. = FALSE)
    }
    
    locs <- dplyr::select(locations, location, mean_latitude, mean_longitude)
    
  }
  
  locs <- dplyr::distinct(locs)    

  #summarize lat and lon for each unique location
  locs <- dplyr::group_by(locs, location)
  locs <- dplyr::summarise(locs, 
            mean_latitude = mean(mean_latitude, na.rm = TRUE),
            mean_longitude = mean(mean_longitude, na.rm = TRUE))
    
  #insert 0 for missing group levels (e.g., non-detection at a location)
  
  #all possible combinations of locations and grp_by columns
  if(!is.null(group_col)){
  group_levels <- merge(data.frame(location = unique(locs$location), 
                        stringsAsFactors = FALSE), 
                      dplyr::distinct(dplyr::select(detections, group_col)))
  } else {
    group_levels <- data.frame(location = unique(locs$location), 
                      stringsAsFactors = FALSE)
  }
  
  #summarize RI for each group
  
  #numerator
  group_cols <- c("location", group_col)
  
  detections <- dplyr::group_by(detections, .dots = group_cols)
  
  ri <- dplyr::do(detections, 
                    data.frame(days_detected = get_days(., calculation_method, 
                                                        time_interval_size)))   
  
  #add missing combinations (non-detection)
  ri <- dplyr::left_join(group_levels, ri, by = group_cols)
  
  ri <- dplyr::mutate(ri, 
          days_detected = ifelse(is.na(days_detected), 0, days_detected))
  
  #divisor
  #set grouping for divisor, calculate total days
  if(groupwise_total == FALSE | is.null(group_col)) {
    detections <- dplyr::ungroup(detections)
    ri$total_days <- get_days(detections, calculation_method, 
                              time_interval_size)
  } else{
    detections <- dplyr::group_by(detections, .dots = group_col)
    ri <- dplyr::left_join(ri,
                           dplyr::do(detections, 
                             data.frame(total_days = 
                                          get_days(., calculation_method, 
                                                   time_interval_size))),
                           by = group_col)
    detections <- dplyr::ungroup(detections)
  }
    
  #calculate RI
  ri <- dplyr::mutate(ri, 
      residency_index = as.double(days_detected) / total_days)

  #add lat and lon
  ri <- dplyr::left_join(ri, locs, by = "location")
  
  out_cols <- c(group_col, c("days_detected", "total_days", "residency_index", 
    "location", "mean_latitude", "mean_longitude"))
  ri <- data.frame(ri)[, out_cols]

  return(ri)
}


#' The function below takes a detection events data frame and determines the 
#' number of time bins in which detections were observed and returns the 
#' cumulative time covered by all bins, in days. Interval (bin) size is 
#' determined by the 'time_interval_size' argument. 
#'
#' For each event (row in detection events data frame), the function 
#' sequences from first_detection to last_detection by time_interval_size, then 
#' counts the number of unique intervals.
#'
#'
#' @param detections - data frame from detection_events (condensed = TRUE)
#' 
#' @param time_interval_size time increment string as in seq.Date 'by' argument
#'
interval_count <- function(detections, time_interval_size) {
  
  #get unique bins in each detection event
  detections <- dplyr::rowwise(detections)
  ints <- dplyr::do(detections, data.frame(
    int = seq(lubridate::floor_date(.$first_detection, time_interval_size), 
              lubridate::floor_date(.$last_detection, time_interval_size),
          by = time_interval_size)))
  
  intcount <- dplyr::n_distinct(ints)
  
  #fraction of day covered by one interval 
  day_fraction <- diff(as.numeric(seq(as.POSIXct("2000-01-01 00:00"), 
                                      by = time_interval_size, 
                                      length.out = 2))) / 86400.0
  
  #cumulative days (not necessarily contiguous)
  day_count <- intcount * day_fraction
  
  return(day_count)
}


#' The function below takes a Pandas DataFrame and determines the number of days
#' any detections were seen on the array.
#'
#' The function converts both the first_detection and last_detection columns
#' into a date with no hours, minutes, or seconds. Next it creates a list of the
#' unique days where a detection was seen. The size of the list is returned as
#' the total number of days as an integer.
#'
#' *** NOTE **** Possible rounding error may occur as a detection on 2016-01-01
#' 23:59:59 and a detection on 2016-01-02 00:00:01 would be counted as days when
#' it is really 2-3 seconds.
#'
#'
#' @param Detections - data frame pulled from the compressed detections CSV
#'
#' @importFrom dplyr distinct mutate select bind_rows
total_days_count <- function(detections) {
  startdays <- distinct(select(mutate(detections, days = as.Date(first_detection)), days))
  enddays <- distinct(select(mutate(detections, days = as.Date(last_detection)), days))
  days <- bind_rows(startdays,enddays)
  daycount <- as.double(dplyr::count(dplyr::distinct(select(days,days ))))
  return(daycount)
}



#' The function below determines the total days difference.
#'
#' The difference is determined by the minimal first_detection of every
#' detection and the maximum last_detection of every detection. Both are
#' converted into a datetime then subtracted to get a timedelta. The timedelta
#' is converted to seconds and divided by the number of seconds in a day
#' (86400). The function returns a floating point number of days (i.e.
#' 503.76834).
#'
#' @param Detections - data frame pulled from the compressed detections CSV
#'
total_diff_days <- function(detections) {
  first <- detections$first_detection[which.min(detections$first_detection)]
  last <- detections$first_detection[which.max(detections$first_detection)]
  total <- as.double(difftime(last, first, units="secs"))/86400.0
  return(total)
}


#' The function below aggregates timedelta of first_detection and last_detection
#' of each detection into a final timedelta then returns a float of the number
#' of days. If the first_detection and last_detection are the same, a timedelta
#' of one second is assumed.
#' 
#' @param Detections -data frame pulled from the compressed detections CSV
#'
#' @importFrom dplyr mutate
aggregate_total_with_overlap <- function(detections) {
  detections <- mutate(detections, timedelta = as.double(difftime(as.Date(last_detection),as.Date(first_detection), units="secs")))
  detections <- mutate(detections, timedelta = dplyr::recode(detections$timedelta, `0` = 1))
  total <- as.double(sum(detections$timedelta))/86400.0
  return(total)
}


#' The function below aggregates timedelta of first_detection and
#' last_detection, excluding overlap between detections. Any overlap between two
#' detections is converted to a new detection using the earlier first_detection
#' and the latest last_detection. If the first_detection and last_detection are
#' the same, a timedelta of one second is assumed.
#'
#' @param Detections - data frame pulled from the compressed detections CSV
#'
#' @importFrom lubridate interval int_overlaps
#' @importFrom dplyr mutate
aggregate_total_no_overlap <- function(detections) {
  total <- 0.0
  detections <- arrange(detections, first_detection)
  detcount <- as.integer(dplyr::count(detections))
  detections <- mutate(detections, interval = interval(first_detection,last_detection))
  detections <- mutate(detections, timedelta = as.double(difftime(as.Date(last_detection),as.Date(first_detection), units="secs")))
  detections <- mutate(detections, timedelta = dplyr::recode(detections$timedelta, `0` = 1))

  next_block <- 2
  start_block <- 1
  end_block <- 1
  while(next_block <= detcount) {
    # if it overlaps
    if(next_block < detcount && int_overlaps(dplyr::nth(detections$interval, end_block), dplyr::nth(detections$interval, next_block))) {
      if(dplyr::nth(detections$interval, next_block) >= dplyr::nth(detections$interval, end_block)) {
        end_block <- next_block
      }

      if(end_block == detcount) {
        tdiff <- as.double(difftime(dplyr::nth(detections$last_detection,end_block), dplyr::nth(detections$first_detection, start_block), units="secs"))

        if(tdiff == 0.0) {
          tdiff <- 1
        }
        start_block <- next_block
        end_block <- next_block + 1
        total <- total + as.double(tdiff)
      }
    } else {
      #if it doesn't overlap
      tdiff <- 0.0

      # Generate the time difference between the start of the start block and the end of the end block
      tdiff <- as.double(difftime(dplyr::nth(detections$last_detection,end_block), dplyr::nth(detections$first_detection, start_block), units="secs"))

      start_block <- next_block
      end_block <- next_block
      total <- total + as.double(tdiff)
    }
    next_block <- next_block + 1
  }
  total <- total/86400.0
  return(total)
}


#' Determines which calculation method to use for the residency index.
#'
#' Wrapper method for the calulation methods above.
#'
#' @param dets - data frame pulled from the detection events
#' @param calculation_method - determines which method above will be used to count total time and location time
get_days <- function(dets, calculation_method='kessel', 
                     time_interval_size = "1 day") {
  days <- 0
  if (calculation_method == 'aggregate_with_overlap') {
    days <- glatos:::aggregate_total_with_overlap(dets)
  } else if(calculation_method == 'aggregate_no_overlap') {
    days <- glatos:::aggregate_total_no_overlap(dets)
  } else if(calculation_method == 'timedelta') {
    days <- glatos:::total_diff_days(dets)
  } else if(calculation_method == 'kessel'){
    days <- glatos:::total_days_count(dets)
  } else if(calculation_method == 'time_interval'){
    days <- glatos:::interval_count(dets, time_interval_size)
  } else {
      stop("Unsupported 'calculated_method'.")
  }
  return(days)
}



#' Uses plotly to place the caluclated residence index on a map.
#'
#' @details This function uses plotly to place the caluclated RI on a map.
#'
#' @param df A data.frame from the \code{residence_index} containing
#' days_detected, residency_index, location, mean_latitude,
#' mean_longitude
#'
#' @param title A string for the title of the plot
#'
#' @return A plotly plot_geo object
#'
#' @importFrom magrittr "%>%"
#' @import plotly
#' @export
ri_plot <- function(df, title="Residence Index") {
  lat_range <- c(df$mean_latitude[which.min(df$mean_latitude)], df$mean_latitude[which.max(df$mean_latitude)])
  lon_range <- c(df$mean_longitude[which.min(df$mean_longitude)], df$mean_longitude[which.max(df$mean_longitude)])
  m <- list(colorbar = list(title = "Residence Index"))

  g <- list(
    showland = TRUE,
    landcolor = plotly::toRGB("gray"),
    subunitcolor = plotly::toRGB("white"),
    countrycolor = plotly::toRGB("white"),
    showlakes = TRUE,
    lakecolor = plotly::toRGB("white"),
    showsubunits = TRUE,
    showcountries = TRUE,
    resolution = 50,
    projection = list(
      type = 'conic conformal',
      rotation = list(lon = -100)
    ),
    lonaxis = list(
      showgrid = TRUE,
      gridwidth = 0.5,
      range = lon_range,
      dtick = 1
    ),
    lataxis = list(
      showgrid = TRUE,
      gridwidth = 0.5,
      range = lat_range,
      dtick = 1
    )
  )

  p <- plot_geo(df, lat = ~mean_latitude, lon = ~mean_longitude, color = ~residency_index) %>%
    add_markers(size = ~((df$residency_index * 5) + 1),
                text = ~paste(df$location, ":",df$residency_index), hoverinfo = "text"
    ) %>%
    layout(title = title, geo = g)
  show(p)
  return(p)
}

