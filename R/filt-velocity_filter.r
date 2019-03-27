#' Veloctiy filter
#'
#' Identify any detections where the lead, lag, or both velocites are greater than a 
#' maximum allowable velocity
#' 
#' @param detections A \code{glatos_detections} object (e.g., produced by
#'   \link{read_glatos_detections} or \link{read_otn_detections}). 
#'
#' @param maximum_velocity A numerical value expressed in m/s (i.e. 10 would be 10 m/s).
#' 
#' @param filter_criteria A string indicating whether to apply the filter to the lead, 
#' lag, or btohe velocites using the respective strings \code{lead}, \code{lag}, or 
#' \code{both} (default).
#' 
#' @param drop_lead_and_lag_rows A boolean to either drop (\code{TRUE}) or keep (\code{FALSE}) the
#' extra columns needed to calculate the velocity.
#'   
#' @details The function groups by unique animal and orders the detections chronologically.
#' It then proceeds to determine the lead and lag station, time difference in seconds,
#' and geolocation for each detection. Using the determined lead and lag values it uses
#' \code{geosphere} to calculate the lead and lag diffence and then the velocity. A filter
#' column is then determined to be a 1 or 0 dpending on if the calculated velocity is
#' greater than the \code{maximum_velocity}.
#' 
#' @examples 
#' #' #get path to example detection file
#' det_file <- system.file("extdata", "walleye_detections.csv",
#'                          package = "glatos")
#' det <- read_glatos_detections(det_file)
#'
#' det <- velocity_filter(det)
#' head(det)
#' 
#' 
#' @importFrom dplyr group_by mutate rowwise select
#' @importFrom magrittr "%>%"
#' @importFrom geosphere distVincentySphere
#'
#' @export

velocity_filter  <- function(detections, maximum_velocity = 10, filter_criteria = 'both', drop_lead_and_lag_rows = TRUE) {
    detections <- detections %>%
      group_by(animal_id) %>%
      mutate(lead_station = lead(station, order_by = detection_timestamp_utc),
             lag_station = lag(station, order_by = detection_timestamp_utc),
             lead_time_diff = (lead(detection_timestamp_utc, order_by = detection_timestamp_utc) - detection_timestamp_utc),
             lag_time_diff =  (detection_timestamp_utc-lag(detection_timestamp_utc, order_by = detection_timestamp_utc)),
             lead_lat =  lead(deploy_lat, order_by = detection_timestamp_utc),
             lead_long =  lead(deploy_long, order_by = detection_timestamp_utc),
             lag_lat =  lag(deploy_lat, order_by = detection_timestamp_utc),
             lag_long =  lag(deploy_long, order_by = detection_timestamp_utc)
      ) %>%
      rowwise() %>%
      mutate(
        lead_dist_m = distVincentySphere(c(deploy_long, deploy_lat), c(lead_long, lead_lat)),
        lag_dist_m = distVincentySphere(c(deploy_long, deploy_lat), c(lag_long, lag_lat)),
        lead_velocity = (lead_dist_m/lead_time_diff),
        lag_velocity = (lag_dist_m/lag_time_diff)
      )

    if(filter_criteria == 'both') {
      filtered_detections <- detections %>%
        mutate(passed_filter=if_else(((lead_velocity > maximum_velocity & is.finite(lead_velocity)) & (lag_velocity > maximum_velocity & is.finite(lag_velocity))), 0, 1))
    } else if(filter_criteria == 'lead'){
      filtered_detections <- detections %>%
        mutate(passed_filter=if_else((lead_velocity > maximum_velocity & is.finite(lead_velocity)), 0, 1))
    } else if(filter_criteria == 'lag') {
      filtered_detections <- detections %>%
        mutate(passed_filter=if_else((lag_velocity > maximum_velocity & is.finite(lag_velocity)), 0, 1))
    } else {
      stop("Unsupported 'filter_criteria'. Please use 'lead', 'lag', or 'both'.")
    }

    nr <- nrow(filtered_detections)
    message(paste0("The filter identified ",
                   nr - sum(filtered_detections$passed_filter), " (",
                   round((nr - sum(filtered_detections$passed_filter))/
                           nr*100, 2), "%) of ", nr,
                   " detections as having a velocity of > ",maximum_velocity," m/s on ",filter_criteria,"."))

    if(drop_lead_and_lag_rows) {
      filtered_detections <- filtered_detections %>% select(-c(lead_station, lag_station, lead_time_diff, lag_time_diff, lead_lat, lead_long, lag_lat, lag_long ,lead_dist_m, lag_dist_m, lead_velocity, lag_velocity))
    }
    return(data.frame(filtered_detections))
}
