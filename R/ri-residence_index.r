#' Generate residence index from detection events
#'
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
