#' Classify discrete events in detection data
#'
#' Reduce detection data into discrete detection events, defined by movement
#' between receivers (or receiver groups, depending on location), or sequential
#' detections at the same location that are separated by a user-defined
#' threshold period of time.
#'
#' @param det A `glatos_detections` object (e.g., produced by
#'   [read_glatos_detections]).
#'
#'   *OR* a data frame containing detection data with four columns
#'   described below and one column containing a location grouping variable,
#'   whose name is specified by `location_col` (see below).
#'
#'   The following four columns must appear in `det`:
#'   \describe{
#'   \item{`animal_id`}{Individual animal identifier; character.}
#' 	 \item{`detection_timestamp_utc`}{Detection timestamps; MUST be of class
#'   POSIXct.}
#' 	 \item{`deploy_lat`}{Latitude of receiver deployment in decimal
#'      degrees, NAD83.}
#' 	 \item{`deploy_long`}{Longitude of receiver deployment in decimal
#'     degrees, NAD83.}
#'   }
#'
#' @param location_col A character string indicating the column name in
#'   `det` that will be used as the location grouping variable (e.g.
#'   "glatos_array"), in quotes.
#'
#' @param time_sep Amount of time (in seconds) that must pass between
#'   sequential detections on the same receiver (or group of receivers,
#'   depending on specified location) before that detection is considered to
#'   belong to a new detection event. The default value `Inf`, will not
#'   define events based on elapsed time (only when location changes).
#'
#' @param condense A logical indicating if the result should be a condensed
#'   data frame (`condense = TRUE`; default value) with one event per row,
#'   or the input data frame with new event data columns added
#'   `condense = TRUE`.
#'
#' @details mean_latitude and mean_longitude columns in the output dataframe are
#'   the mean GPS locations for the detections comprising that detection event.
#'   For example, if the a fish was detected at 3 receiver stations in a
#'   glatos_array and glatos_array was selected as the location, then GPS
#'   location for that event will be the mean of the latitude and longitude
#'   for those three receiver stations (weighted based on the number of
#'   detections that occurred on each station).
#'
#' @return A data.table or tibble object (if input is either type; output
#' class to match input) or data.frame otherwise. Structure depends on
#' value of `condense` argument: \cr
#'
#' If `condense = TRUE`, a data.frame, data.table, or tibble with the
#' following columns:
#'  \item{event}{Unique event identifier.}
#'  \item{individual}{Unique 'animal_id'.}
#'  \item{location}{Unique 'location'.}
#' 	\item{mean_latitude}{Mean latitude of detections comprising each event.}
#' 	\item{mean_longitude}{Mean longitude of detections comprising each event.}
#'  \item{first_detection}{The time of the first detection in a given detection
#'    event.}
#'  \item{last_detection}{The time of the last detection in a given detection
#'    event.}
#'  \item{num_detections}{The total number of detection that comprised a given
#' 	  detection event.}
#'  \item{res_time_sec}{The elapsed time in seconds between the first and last
#' 		detection in a given event.}
#'
#' 	If `condense = FALSE`, a data.frame, data.table, or tibble matching the
#' 	input data frame `det` with the following columns added:
#'  \item{time_diff}{Lagged time difference in seconds between successive
#'    detections of each animal_id.}
#'  \item{arrive}{Flag (0 or 1) representing the first detection in each
#'    event.}
#'  \item{depart}{Flag (0 or 1) representing the last detection in each
#'    event.}
#'  \item{event}{Integer representing the event number.}
#'
#' @author T. R. Binder, T. A. Hayden, C. M. Holbrook
#'
#' @examples
#'
#' # get path to example detection file
#' det_file <- system.file("extdata", "walleye_detections.csv",
#'   package = "glatos"
#' )
#' det <- read_glatos_detections(det_file)
#'
#' filt0 <- detection_events(det) # no time filter
#'
#' # 7-day filter
#' filt_7d <- detection_events(det, time_sep = 604800)
#'
#' # 7-day filter but return do not condense result
#' filt_7d <- detection_events(det, time_sep = 604800, condense = FALSE)
#'
#' @export

detection_events <- function(
    det,
    location_col = "glatos_array",
    time_sep = Inf,
    condense = TRUE) {
  # Make detections data frame a data.table object for processing speed
  detections <- data.table::as.data.table(det)

  # Check time_sep is numeric
  if (is.character(time_sep)) {
    time_sep <- as.numeric(time_sep)
    if (all(is.na(time_sep))) stop("`time_sep` argument should be numeric.")
  }

  # Check value of condense
  if (!is.logical(condense)) {
    stop(
      "input argument 'condense' must be either TRUE or FALSE (unquoted)."
    )
  }

  # Check that the specified columns appear in the detections dataframe
  missingCols <- setdiff(
    c(
      "animal_id",
      "detection_timestamp_utc",
      "deploy_lat",
      "deploy_long"
    ),
    names(detections)
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

  # Check that timestamp is of class 'POSIXct'
  if (!("POSIXct" %in% class(detections$detection_timestamp_utc))) {
    stop(
      paste0(
        "Column 'detection_timestamp_utc' in the detections dataframe",
        "must be of class 'POSIXct'."
      ),
      call. = FALSE
    )
  }

  # Set variables to NULL that will appear in data.table calls
  #  to avoid R CMD check NOTES
  animal_id <- detection_timestamp_utc <- time_diff <- arrive <-
    depart <- event <- deploy_lat <- deploy_long <- NULL

  # Change name of location variable column for convenience
  data.table::setnames(detections, location_col, "location_col")

  # Sort detections by transmitter id and then by detection timestamp.
  data.table::setkey(detections, animal_id, detection_timestamp_utc)

  # Add a column with time between a given detection and the detection
  detections[,
    time_diff := c(NA, diff(as.numeric(detection_timestamp_utc))),
    by = c("animal_id", "location_col")
  ]

  # Flag as arrival if location not equal to previous or time_diff > time_sep
  detections[,
    arrive := as.numeric(
      (location_col != data.table::shift(location_col, fill = TRUE)) |
        (time_diff > time_sep)
    ),
    by = animal_id
  ]

  # Flag as departure if location not equal to next or time_diff > time_sep
  detections[,
    depart := as.numeric(
      (location_col !=
        data.table::shift(location_col, fill = TRUE, type = "lead")) |
        (data.table::shift(time_diff, fill = TRUE, type = "lead") > time_sep)
    ),
    by = animal_id
  ]

  # Add unique event number (among all fish, not within each fish)
  detections[, event := cumsum(arrive)]

  # Summarize the event data using data.table.
  Results <- detections[,
    .(
      animal_id = animal_id[1],
      location = location_col[1],
      mean_latitude = mean(deploy_lat, na.rm = T),
      mean_longitude = mean(deploy_long, na.rm = T),
      first_detection = detection_timestamp_utc[1],
      last_detection = detection_timestamp_utc[.N],
      num_detections = .N,
      res_time_sec = diff(range(as.numeric(detection_timestamp_utc)))
    ),
    by = event
  ]

  # function to match output to input
  out_class <- function(xout, xin) {
    # return data.table if input class data.table
    if (inherits(xin, "data.table")) {
      return(xout)
    }

    # return tibble if input class tibble
    if (inherits(xin, "tbl")) {
      return(dplyr::as_tibble(xout))
    }

    return(as.data.frame(xout))
  }

  # Return conditional on 'condense'
  if (condense) {
    # Returns dataframe containing summarized detection event data
    message(paste0(
      "The event filter distilled ",
      nrow(detections),
      " detections down to ",
      nrow(Results),
      " distinct detection events."
    ))

    return(out_class(Results, det))
  } else {
    # Returns input dataframe with new columns
    message(paste0(
      "The event filter identified ",
      max(detections$event, na.rm = TRUE),
      " distinct events in ",
      nrow(detections),
      " detections."
    ))

    # Rename location variable column back to original
    data.table::setnames(detections, "location_col", location_col)

    return(out_class(detections, det))
  }
}
