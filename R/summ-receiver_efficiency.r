#' Calculates a returns a list of each station and the REI (defined here)
#'
#' @description  The receiver efficiency index is number between 0 and 1 indicating the amount of relative activity at each receiver compared to the
#' entire set of receivers, regardless of positioning. The function takes a set detections and a deployment history of the receivers to
#' create a context for the detections. Both the amount of unique tags and number of species are taken into consideration in the calculation.
#'
#' \emph{(Ellis, R., Flaherty-Walia, K., Collins, A., Bickford, J., Walters Burnsed, Lowerre-Barbieri S. 2018. Acoustic telemetry array evolution: from species- and project-specific designs to large-scale, multispecies, cooperative networks,
#'  <https://doi.org/10.1016/j.fishres.2018.09.015>)}
#'
#' REI() takes two arguments. The first is a dataframe of detections the detection timstamp, the station identifier, the species, and the
#' tag identifier. The next is a dataframe of deployments for each station. The station name should match the stations in the detections.
#' The deployments need to include a deployment date and recovery date.
#'
#' \deqn{
#' REI = (Tr/Ta) x (Sr/Sa) x (DDr/DDa) x (Da/Dr)
#' }
#' \itemize{
#' \item{Tr = The number of tags detected on the receievr}
#' \item{Ta = The number of tags detected across all receivers}
#' \item{Sr = The number of species detected on the receiver}
#' \item{Sa = The number of species detected across all receivers}
#' \item{DDa = The number of unique days with detections across all receivers}
#' \item{DDr = The number of unique days with detections on the receiver}
#' \item{Da = The number of days the array was active}
#' \item{Dr = The number of days the receiver was active}
#' }
#'
#'
#' @param detections a glatos detections class data table
#'
#' @param deployments a glatos receivers class data table
#'
#' @return a list of receivers with lat and long and the receiver efficiency index
#'
#' @author Alex Nunes  \email{anunes@dal.ca}
#'
#' @examples
#' \dontrun{
#' det_file <- system.file("extdata", "hfx_detections.csv",
#'   package = "glatos"
#' )
#'
#' dep_file <- system.file("extdata", "hfx_deployments.csv",
#'   package = "glatos"
#' )
#'
#' hfx_deployments <- glatos::read_otn_deployments(dep_file)
#' dets <- glatos::read_otn_detections(det_file)
#'
#' hfx_receiver_efficiency_index <- glatos::REI(dets, hfx_deployments)
#' }
#' @importFrom dplyr group_by mutate coalesce filter summarise
#'
#' @export

REI <- function(detections, deployments) {
  ##  Declare global variables for NSE & R CMD check
  recover_date_time <- last_download <- station <- days_deployed <- deploy_lat <-
    deploy_long <- animal_id <- common_name_e <- detection_timestamp_utc <- NULL

  # Check for proper columns
  required_deployment_columns <- c("station", "deploy_date_time", "recover_date_time")
  required_detection_columns <- c("station", "common_name_e", "animal_id", "detection_timestamp_utc")

  if (all(required_deployment_columns %in% colnames(deployments)) & all(required_detection_columns %in% colnames(detections))) {
    if ("last_download" %in% colnames(deployments)) {
      deployments <- deployments %>%
        dplyr::mutate(recover_date_time = coalesce(recover_date_time, last_download))
      deployments <- deployments %>% filter(!is.na(last_download) | !is.na(recover_date_time))
    }



    # Make sure all dates are a POSIXct type
    if (!inherits(deployments$deploy_date_time, "POSIXct")) {
      deployments$deploy_date_time <- as.POSIXct(deployments$deploy_date_time, format = "%Y-%m-%d %H:%M:%S")
    }

    if (!inherits(deployments$recover_date_time, "POSIXct")) {
      deployments$recover_date_time <- as.POSIXct(deployments$recover_date_time, format = "%Y-%m-%d %H:%M:%S")
    }

    if (!inherits(detections$detection_timestamp_utc, "POSIXct")) {
      detections$detection_timestamp_utc <- as.POSIXct(detections$detection_timestamp_utc, format = "%Y-%m-%d %H:%M:%S")
    }

    # Get the total number of days the array/line was active
    array_days_active <- as.integer(max(na.omit(deployments$recover_date_time)) - min(na.omit(deployments$deploy_date_time)))

    # Calculate each receivers total days deployed
    deployments$days_deployed <- round(difftime(deployments$recover_date_time, deployments$deploy_date_time, units = "days"), 0)
    deployments <- deployments[, c("station", "days_deployed")]

    deployments <- group_by(deployments, station) %>% summarise(
      receiver_days_active = as.numeric(sum(days_deployed))
    )
    deployments <- na.omit(deployments)

    # Exclude all detections that are not registered with receivers in the deployments
    detections <- subset(detections, detections$station %in% deployments$station)

    # Calculate array counts
    array_unique_tags <- length(unique(detections$animal_id))
    array_unique_species <- length(unique(detections$common_name_e))
    days_with_detections <- length(unique(as.Date(detections$detection_timestamp_utc)))


    # Loop through each station in the detections and Calculate REI for each station
    station_stats <- group_by(detections, station) %>% summarise(
      latitude = mean(deploy_lat),
      longitude = mean(deploy_long),
      receiver_unique_tags = length(unique(animal_id)),
      receiver_unique_species = length(unique(common_name_e)),
      receiver_days_with_detections = length(unique(as.Date(detection_timestamp_utc)))
    )

    station_reis <- merge(station_stats, deployments, by = "station", all.x = TRUE)

    station_reis$rei <- (station_reis$receiver_unique_tags / array_unique_tags) * (station_reis$receiver_unique_species / array_unique_species) * (station_reis$receiver_days_with_detections / days_with_detections) * (array_days_active / station_reis$receiver_days_active)

    # Normalize REIs to value from 0 to 1
    station_reis$rei <- station_reis$rei / sum(station_reis$rei)

    # Cleanup and return the station REI's
    station_reis <- station_reis[, c("station", "latitude", "longitude", "rei")]

    return(station_reis)
  } else {
    message("Please make sure detections and deployments have the correct columns.")
  }
}
