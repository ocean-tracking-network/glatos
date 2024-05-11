#' Calculate 'min_lag' for identifying potential false positive detections
#'
#' Calculate minimum time interval (min_lag) between successive detections and
#' add to detection data set for identifying potential false detections.
#'
#' @param det A `glatos_detections` object (e.g., produced by
#'   [read_glatos_detections]).
#'
#'   *OR* a data frame containing detection
#'   data with the following columns:
#'   \describe{
#'   \item{detection_timestamp_utc}{Detection timestamps; MUST be of class
#'   POSIXct.}
#'   \item{transmitter_codespace}{A character string with transmitter code space
#'   (e.g., "A69-1061" for Vemco PPM coding").}
#'   \item{transmitter_id}{A character string with transmitter ID code (e.g.,
#'   "1363" for Vemco PPM coding").}
#'   \item{receiver_sn}{A character vector with unique receiver serial number.}
#'   }
#'
#' @details `min_lag` is loosely based on the the "short interval"
#'   described by Pincock (2012) and replicates the `min_lag` column in the
#'   standard glatos detection export file. In this case (GLATOS),
#'   `min_lag` is defined for each detection as the shortest interval (in
#'   seconds) between either the previous or next detection (whichever is
#'   closest) of the same transmitter code (defined here as combination of
#'   transmitter_codespace and transmitter_id) on the same receiver.
#'
#' @details A new column (`min_lag`) is added to the input
#'     dataframe that represents the time (in seconds) between the
#'     current detection and the next detection (either before or
#'     after) of the same transmitter on the same receiver. This
#'     function replicates the 'min_lag' column included in the
#'     standard glatos export.
#'
#' @return A column `min_lag` (defined above) is added to input object.
#'
#' @seealso [false_detections()]
#'
#' @author Chris Holbrook, Todd Hayden, Angela Dini
#'
#' @references
#'   Pincock, D.G., 2012. False detections: what they are and how to remove them
#'     from detection data. Vemco Division, Amirix Systems Inc., Halifax,
#'     Nova Scotia.
#'     \cr <http://www.vemco.com/pdf/false_detections.pdf>
#'
#' @examples
#'
#' # load example detection file
#' det_file <- system.file("extdata", "walleye_detections.csv",
#'   package = "glatos"
#' )
#' det <- read_glatos_detections(det_file)
#'
#' # rename existing min_lag column
#' colnames(det)[colnames(det) == "min_lag"] <- "min_lag.x"
#'
#' # calculate min_lag
#' det <- min_lag(det)
#'
#' head(det)
#'
#' @export

min_lag <- function(det) {
  ##  Declare global variables for NSE & R CMD check
  ord <- transmitter_codespace <- transmitter_id <- receiver_sn <-
    detection_timestamp_utc <- NULL
  
  # coerce to data.table
  dtc <- data.table::as.data.table(det)

  dtc[, ord := 1:.N] # original order

  data.table::setkey(
    dtc, transmitter_codespace, transmitter_id, receiver_sn,
    detection_timestamp_utc
  )

  # calculate min_lag
  dtc[, min_lag := pmin(diff(c(NA, as.numeric(detection_timestamp_utc))),
    diff(c(as.numeric(detection_timestamp_utc), NA)),
    na.rm = TRUE
  ),
  by = c("transmitter_codespace", "transmitter_id", "receiver_sn")
  ]

  # return original order
  setkey(dtc, ord)

  # drop unwanted columns
  drop_cols <- "ord"
  dtc <- dtc[, !drop_cols, with = FALSE]

  # return data.table if input class data.table
  if (inherits(det, "data.table")) {
    return(dtc)
  }

  # return tibble if input class tibble
  if (inherits(det, "tbl")) {
    return(tibble::as_tibble(dtc))
  }

  return(as.data.frame(dtc))
}
