#' Calculates receiver efficiency index for each receiver station
#'
#' @description  The receiver efficiency index is number between 0 and 1 indicating the amount of relative activity at each receiver compared to the entire set of receivers, regardless of positioning. The function takes a set detections and a deployment history of the receivers to create a context for the detections. Both the amount of unique tags and number of species are taken into consideration in the calculation.
#'

#' See:
#' \emph{(Ellis, R., Flaherty-Walia, K., Collins, A., Bickford, J., Walters Burnsed, Lowerre-Barbieri S. 2018. Acoustic telemetry array evolution: from species- and project-specific designs to large-scale, multispecies, cooperative networks,
#'  <https://doi.org/10.1016/j.fishres.2018.09.015>)}
#'
#' REI() takes two arguments and is calculated as:

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

#' @param detections A `glatos_detections` object (e.g., produced by [as_glatos_detections]) containing required columns **plus** columns for station (unique name representing the detection location; character) and species name (common_name_e; character)
#'
#' *OR* a data frame with the following columns whose names and types are specified below:
#' \describe{
#' \item{`animal_id`}{Individual unique animal identifier; character.}
#' \item{`detection_timestamp_utc`}{Detection timestamp in UTC; MUST be of class POSIXct.}
#' \item{`deploy_lat`}{Receiver deployment latitude for detection in decimal degrees (NAD83); numeric.}
#' \item{`deploy_long`}{Receiver deployment longitude for detection in decimal degrees (NAD83); numeric.}
#' \item{`ins_serial_no`}{Receiver serial number; character.}
#' \item{`station`}{unique name representing the detection location; character.}
#' \item{`common_name_e`}{species identifier; character.}
#' }
#'
#' @param receivers A `glatos_receivers` object (e.g., produced by [as_glatos_receivers]) containing the required columns **plus** a column for station (grouping column representing location of detection; character)
#'
#' *OR* a data frame with the following columns whose names and types are specified below:
#' \describe{
#' \item{`station`}{unique name representing the detection location; character.}
#' \item{`deploy_date_time`}{Receiver deployment timestamp; MUST be of class POSIXct.}
#' \item{`recover_date_time`}{Receiver recovery timestamp; MUST be of class POSIXct.}
#' \item{`deploy_lat`}{Receiver deployment latitude in decimal degrees (NAD83); numeric.}
#' \item{`deploy_long`}{Receiver deployment longitude in decimal degrees (NAD83); numeric.}
#' \item{`ins_serial_no`}{Receiver serial number; character.}
#' }
#'
#' **NOTE** Values of `station` in `receivers` and `detections` object should be the same.  Only matching `stations` in `receivers` and `detections` will be returned in output!

#' @return a data frame of receivers with deployment latitude and deployment longitude and receiver efficiency index
#'
#' @author Alex Nunes  \email{anunes@dal.ca}
#'
#' @examples

#' # load exmple detections file
#' dtc_file <- system.file("extdata", "walleye_detections.csv", package = "glatos")
#' dtc <- read_glatos_detections(dtc_file)

#' # load example receiver deployment/recovery file
#' recs <- system.file("extdata", "sample_receivers.csv", package = "glatos")
#' recs <- read_glatos_receivers(recs)

#' REI(dtc, recs)
#' @export

REI <- function(detections, deployments) {
  ##  Declare global variables for NSE & R CMD check
  recover_date_time <- last_download <- station <- days_deployed <- deploy_lat <-
    deploy_long <- animal_id <- common_name_e <- detection_timestamp_utc <- NULL

  # coerce to glatos_receivers class if not
  # req cols = deploy_lat, deploy_long, deploy_date_time, recover_date_time, ins_serial_no
  if (!inherits(deployments, "glatos_receivers")) {
    deployments <- as_glatos_receivers(deployments)
  }

  # coerce to glatos_detections if not
  if (!inherits(detections, "glatos_detections")) {
    detections <- as_glatos_detections(detections)
  }

  # Check for proper columns that are not required in glatos_receivers object
  required_deployment_columns <- c(
    "station"
  )

  # check for columns that are not required in glatos_detections object
  required_detection_columns <- c(
    "station",
    "common_name_e"
  )

  # determine if any required columns are missing in the detections object
  missingCols_dtc <- setdiff(
    required_detection_columns,
    names(detections)
  )

  if (length(missingCols_dtc) > 0) {
    stop(
      paste0(
        "Detections dataframe is missing the following ",
        "column(s):\n",
        paste0("       '", missingCols_dtc, "'", collapse = "\n")
      ),
      call. = FALSE
    )
  }

  # determine if any required columns are missing in the receivers object
  missingCols <- setdiff(
    required_deployment_columns,
    names(deployments)
  )

  if (length(missingCols) > 0) {
    stop(
      paste0(
        "Detections dataframe is missing the following ",
        "column(s):\n",
        paste0("       '", missingCols, "'", collapse = "\n")
      ),
      call. = FALSE
    )
  }

  if ("last_download" %in% colnames(deployments)) {
    deployments <- deployments |>
      dplyr::mutate(
        recover_date_time = dplyr::coalesce(recover_date_time, last_download)
      )
    deployments <- deployments %>%
      filter(!is.na(last_download) | !is.na(recover_date_time))
  }

  # Check that deploy timestamp is of class 'POSIXct' in deployments
  if (!("POSIXct" %in% class(deployments$deploy_date_time))) {
    stop(
      paste0(
        "Column 'deploy_date_time' in the deployments data frame",
        "must be of class 'POSIXct'."
      ),
      call. = FALSE
    )
  }

  # Check that recover timestamp is of class 'POSIXct' in deployments
  if (!("POSIXct" %in% class(deployments$recover_date_time))) {
    stop(
      paste0(
        "Column 'recover_date_time' in the deployments dataframe",
        "must be of class 'POSIXct'."
      ),
      call. = FALSE
    )
  }

  # Check that detection_timestamp timestamp is of class 'POSIXct' in detections
  if (!("POSIXct" %in% class(detections$detection_timestamp_utc))) {
    stop(
      paste0(
        "Column 'detection_timestamp_utc' in the detections dataframe",
        "must be of class 'POSIXct'."
      ),
      call. = FALSE
    )
  }

  # Get the total number of days the array/line was active
  array_days_active <- as.integer(
    max(na.omit(deployments$recover_date_time)) -
      min(na.omit(deployments$deploy_date_time))
  )

  # Calculate each receivers total days deployed
  deployments$days_deployed <- round(
    difftime(
      deployments$recover_date_time,
      deployments$deploy_date_time,
      units = "days"
    ),
    0
  )
  deployments <- deployments[, c("station", "days_deployed")]

  deployments <- dplyr::group_by(deployments, station) |>
    dplyr::summarise(
      receiver_days_active = as.numeric(sum(days_deployed))
    )
  deployments <- na.omit(deployments)

  # Exclude all detections that are not registered with receivers in the deployments
  detections <- subset(
    detections,
    detections$station %in% deployments$station
  )

  # Calculate array counts
  array_unique_tags <- length(unique(detections$animal_id))
  array_unique_species <- length(unique(detections$common_name_e))
  days_with_detections <- length(unique(as.Date(
    detections$detection_timestamp_utc
  )))

  # Loop through each station in the detections and Calculate REI for each station
  station_stats <- dplyr::group_by(detections, station) |>
    dplyr::summarise(
      latitude = mean(deploy_lat),
      longitude = mean(deploy_long),
      receiver_unique_tags = length(unique(animal_id)),
      receiver_unique_species = length(unique(common_name_e)),
      receiver_days_with_detections = length(unique(as.Date(
        detection_timestamp_utc
      )))
    )

  station_reis <- merge(
    station_stats,
    deployments,
    by = "station",
    all.x = TRUE
  )

  station_reis$rei <- (station_reis$receiver_unique_tags /
    array_unique_tags) *
    (station_reis$receiver_unique_species / array_unique_species) *
    (station_reis$receiver_days_with_detections / days_with_detections) *
    (array_days_active / station_reis$receiver_days_active)

  # Normalize REIs to value from 0 to 1
  station_reis$rei <- station_reis$rei / sum(station_reis$rei)

  # Cleanup and return the station REI's
  station_reis <- station_reis[, c("station", "latitude", "longitude", "rei")]

  return(station_reis)
}
