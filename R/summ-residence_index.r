#' The function below takes a Pandas DataFrame and determines the number of days any
#' detections were seen on the array.
#'
#' The function converts both the first_detection and last_detection columns into a date with no hours, minutes,
#' or seconds. Next it creates a list of the unique days where a detection was seen. The size of the
#' list is returned as the total number of days as an integer.
#'
#' *** NOTE ****
#' Possible rounding error may occur as a detection on 2016-01-01 23:59:59 and a detection on
#' 2016-01-02 00:00:01 would be counted as days when it is really 2-3 seconds.
#'
#'
#' @param Detections - data frame pulled from the compressed detections CSV
require(dplyr)
total_days_count <- function(detections) {
  startdays <- distinct(select(mutate(detections, days = as.Date(first_detection)), days))
  enddays <- distinct(select(mutate(detections, days = as.Date(last_detection)), days))
  days <- bind_rows(startdays,enddays)
  daycount <- as.double(dplyr::count(dplyr::distinct(select(days,days ))))
  return(daycount)
}


#' The function below determines the total days difference.
#'
#' The difference is determined by the minimal first_detection of every detection and the maximum last_detection of every detection.
#' Both are converted into a datetime then subtracted to get a timedelta. The timedelta
#' is converted to seconds and divided by the number of seconds in a day (86400). The function
#' returns a floating point number of days (i.e. 503.76834).
#'
#' @param Detections - data frame pulled from the compressed detections CSV
require(dplyr)
total_diff_days <- function(detections) {
  first <- detections$first_detection[which.min(detections$first_detection)]
  last <- detections$first_detection[which.max(detections$first_detection)]
  total <- as.double(difftime(last, first, units="secs"))/86400.0
  return(total)
}


#' The function below aggregates timedelta of first_detection and last_detection of each detection into
#' a final timedelta then returns a float of the number of days. If the first_detection and last_detection
#' are the same, a timedelta of one second is assumed.
#'
#' @param Detections -data frame pulled from the compressed detections CSV
require(lubridate)
require(dplyr)
aggregate_total_with_overlap <- function(detections) {
  detections <- mutate(detections, timedelta = as.double(difftime(as.Date(last_detection),as.Date(first_detection), units="secs")))
  detections <- mutate(detections, timedelta = dplyr::recode(detections$timedelta, `0` = 1))
  total <- as.double(sum(detections$timedelta))/86400.0
  return(total)
}


#' The function below aggregates timedelta of first_detection and last_detection, excluding overlap between
#' detections. Any overlap between two detections is converted to a new detection using the earlier
#' first_detection and the latest last_detection. If the first_detection and last_detection are the same, a timedelta of one
#' second is assumed.
#'
#' @param Detections - data frame pulled from the compressed detections CSV
require(lubridate)
require(dplyr)
aggregate_total_no_overlap <- function(detections) 
  

#' Determines which calculation method to use for the residency index.
#'
#' Wrapper method for the calulation methods above.
#'
#' @param dets - data frame pulled from the detection events
#' @param calculation_method - determines which method above will be used to count total time and location time
get_days <- function(dets, calculation_method='kessel') {
  days <- 0
  if (calculation_method == 'aggregate_with_overlap') {
    days <- glatos:::aggregate_total_with_overlap(dets)
  } else if(calculation_method == 'aggregate_no_overlap') {
    days <- glatos:::aggregate_total_no_overlap(dets)
  } else if(calculation_method == 'timedelta') {
    days <- glatos:::total_diff_days(dets)
  } else {
    days <- glatos:::total_days_count(dets)
  }
  return(days)
}


#' This residence index tool will take a compressed or uncompressed detection
#' file and caculate the residency index for each station/receiver in the
#' detections. The information passed to the function is what is used to
#' calculate the residence index, make sure you are only passing the data you
#' want taken into consideration for the residence index (i.e. species,
#' stations, tags, etc.).
#'
#' Kessel et al. Paper - https://www.researchgate.net/publication/279269147
#'
#' @param detections A data.frame from the \code{detection_events} function from
#' the \pkg{glatos} package
#'
#' @param calculation_method A character string with the calculation method
#' using one of the following:
#' \itemize{
#'   \item kessel
#'   \item timedelta
#'   \item aggregate_with_overlap
#'   \item aggregate_no_overlap
#' }
#'
#'
#' @details \strong{Kessel Residence Index Calculation}
#' The Kessel method converts both the startdate and enddate columns into a
#' date with no hours, minutes, or seconds. Next it creates a list of the
#' unique days where a detection was seen. The size of the list is returned
#' as the total number of days as an integer. This calculation is used to
#' determine the total number of distinct days (T) and the total number of
#' distinct days per station (S).
#' Possible rounding error may occur as a detection on 2016-01-01 23:59:59
#' and a detection on 2016-01-02 00:00:01 would be counted as two days
#' when it is really 2-3 seconds.
#'
#' \deqn{
#' RI = S/T
#' RI = Residence Index
#' S = Distinct number of days detected at the station
#' T = Distinct number of days detected anywhere on the array
#' }
#'
#' @details \strong{Timedelta Residence Index Calculation}
#' The Timedelta calculation method determines the first detection and the
#' last detection of all detections. The time difference is then taken as
#' the values to be used in calculating the residence index. The timedelta
#' for each station is divided by the timedelta of the array to determine
#' the residence index.
#'
#' \deqn{
#' RI = Delta S/Delta T
#' RI = Residence Index
#' Delta S = Last detection time at a station - First detection time at the station
#' Delta T = Last detection time on an array - First detection time on the array
#' }
#'
#' @details \strong{Aggregate With Overlap Residence Index Calculation}
#' The Aggregate With Overlap calculation method takes the length of time of each
#' detection and sums them together. A total is returned. The sum for each station
#' is then divided by the sum of the array to determine the residence index.
#'
#' \deqn{
#' RI = AwOS/AwOT
#' RI = Residence Index
#' AwOS = Sum of length of time of each detection at the station
#' AwOT = Sum of length of time of each detection on the array
#' }
#'
#' @details \strong{Aggregate No Overlap Residence Index Calculation}
#' The Aggregate No Overlap calculation method takes the length of time of each
#' detection and sums them together. However, any overlap in time between one or
#' more detections is excluded from the sum. For example, if the first detection
#' is from __2016-01-01 01:02:43__ to __2016-01-01 01:10:12__ and the second
#' detection is from __2016-01-01 01:09:01__ to __2016-01-01 01:12:43__, then the
#' sum of those two detections would be 10 minutes. A total is returned once all
#' detections of been added without overlap. The sum for each station is then
#' divided by the sum of the array to determine the residence index.
#'
#' \deqn{
#' RI = AnOS/AnOT
#' RI = Residence Index
#' AnOS = Sum of length of time of each detection at the station, excluding any overlap
#' AnOT = Sum of length of time of each detection on the array, excluding any overlap
#' }
#'
#' @return A data.frame of days_detected, residency_index, location,
#' mean_latitude, mean_longitude
#'
#' @author A. Nunes, \email{anunes@dal.ca}
#'
#' @examples
#' data <- glatos::read_glatos_detections('./inst/extdata/walleye_detections.csv')
#' cdata <- glatos::detection_events(data)
#' rik_data <- glatos::residence_index(cdata, calculation_method = 'kessel')
#' rit_data <- glatos::residence_index(cdata, calculation_method = 'timedelta')
#' riawo_data <- glatos::residence_index(cdata, calculation_method = 'aggregate_with_overlap')
#' riano_data <- glatos::residence_index(cdata, calculation_method = 'aggregate_no_overlap')
#'
#' @importFrom dplyr count distinct 
#' @export
residence_index <- function(detections, calculation_method='kessel') {
  
  total_days = get_days(detections, calculation_method)
  
  ri <- data.frame('days_detected'=numeric(),'residency_index'=numeric(), 'location'=character())
  
  locations <- distinct(select(detections, location))
  for (index in 1:as.integer(dplyr::count(locations))) {
    stn <- as.character(slice(locations, index)$location)
    stn_data <- filter(detections, location == stn)
    total_stn_days <- glatos:::get_days(stn_data, calculation_method)
    res_index = as.double(total_stn_days)/total_days
    row <- data.frame('days_detected'=total_stn_days,'residency_index'=res_index, 'location'=stn)
    ri <- rbind(ri, row)
  }
  
  locations <- unique(detections[,c('location','mean_latitude','mean_longitude')])
  rownames(locations) <- 1:nrow(locations)
  locations$location <- as.character(locations$location)
  ri$location <- as.character(ri$location)
  ri <- dplyr::left_join(ri, locations, by = "location")
  return(ri)
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
#' @import magrittr
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