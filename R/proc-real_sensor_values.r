#' Add 'real'-scale sensor values to glatos detetections
#'
#' Get transmitter sensor (e.g., depth, temperature) conversion parameters
#' (e.g., intercept, slope) from a Vemco transmitter specification object (e.g.,
#' from [read_vemco_tag_specs], calculate 'real'-scale values
#' (e.g., depth in meters), and add real values to detection data in a new
#' column.
#'
#' @param det A `glatos_detections` object (e.g., produced by
#'   [read_glatos_detections]).
#'
#'   *OR* A data frame containing detection
#'   data with the following columns:
#'   \describe{
#'   \item{transmitter_codespace}{A character string with transmitter code space
#'   (e.g., "A69-1061" for Vemco PPM coding").}
#'   \item{transmitter_id}{A character string with transmitter ID code (e.g.,
#'   "1363" for Vemco PPM coding").}
#'   \item{sensor_value}{A numeric sensor measurement (e.g., an integer for
#'   'raw' Vemco sensor tags).}
#'   \item{sensor_unit}{A character string with `sensor_value` units
#'   (e.g., "ADC" for 'raw' Vemco sensor tag detections). }
#'   }
#'
#' @param tag_specs An object produced by [read_vemco_tag_specs].
#'
#'   *OR* A data frame containing transmitter specification data with the
#'   following columns:
#'   \describe{
#'   \item{code_space}{A character string with transmitter code space (e.g.,
#'   "A69-1061" for Vemco PPM coding").}
#'   \item{id_code}{A character string with transmitter ID code (e.g., "1363"
#'   for Vemco PPM coding").}
#'   \item{sensor_type}{A numeric sensor measurement (e.g., an integer for 'raw'
#'   Vemco sensor tags).}
#'   \item{sensor_range}{A numeric with max. range of the sensor in 'real' units
#'   (e.g., "Meters" for Vemco depth tags). }
#'   \item{sensor_units}{A character string with 'real'-scale units (e.g.,
#'   "Meters" for 'raw' Vemco pressure tags). }
#'   }
#'
#'   The following columns are also required for **depth** and
#'   **temperature** sensors:
#'   \describe{
#'   \item{sensor_slope}{Slope parameter, for converting 'raw' (ADC) to 'real'
#'   measurements. }
#'   \item{sensor_intercept}{Intercept parameter, for converting 'raw' (ADC) to
#'   'real' measurements. }
#'   }
#'
#'   The following columns are also required for **acceleration** sensors:
#'   \describe{
#'   \item{accel_algorithm}{The algorithm used, accelerometers only. }
#'   \item{accel_sample_rate}{Sample rate used, accelerometers only. }
#'   \item{sensor_transmit_ratio}{Sensor transmit rate used, accelerometers
#'   only. }
#'   }
#'
#' @details Tag spec data are joined to detection data and then raw-scale sensor
#'   measurements are converted to real-scale using \eqn{sensor_value_real =
#'   sensor_intercept + (sensor_value * sensor_slope)}, where \eqn{sensor_value}
#'   is in raw scale.
#'
#' @details It is possible that `transmitter_codespace` and
#'   `transmitter_id` are not unique among transmitters, so users must
#'   ensure that the each combination of those columns occurs only once in
#'   `tag_specs` and is the correct record for the corresponding tags in
#'   `det`.
#'
#' @return The input data frame, data.table, or tibble with the following
#'   columns added (see column descriptions above):
#'
#'   \itemize{
#'     \item sensor_range
#'     \item sensor_units
#'     \item sensor_slope
#'     \item sensor_intercept
#'     \item accel_algorithm
#'     \item accel_sample_rate
#'     \item sensor_transmit_ratio
#'     \item sensor_value_real
#'  }
#'
#' @author Chris Holbrook, \email{cholbrook@usgs.gov}
#'
#' @examples
#'
#' # get path to example detection file
#' det_file <- system.file("extdata",
#'   "lamprey_detections.csv",
#'   package = "glatos"
#' )
#'
#' lamprey_detections <- read_glatos_detections(det_file)
#'
#' # get path to example Vemco tag spec file
#' spec_file <- system.file("extdata",
#'   "lamprey_tag_specs.xls",
#'   package = "glatos"
#' )
#'
#' lamprey_tags <- read_vemco_tag_specs(spec_file, file_format = "vemco_xls")
#'
#' # note use of '$specs' in tag_specs argument
#' dtc <- real_sensor_values(lamprey_detections, lamprey_tags$specs)
#'
#' # now view records with sensor measurements
#' dtc[!is.na(dtc$sensor_value_real), ]
#'
#' @export

real_sensor_values <- function(det, tag_specs) {
  ##  Declare global variables for NSE & R CMD check
  ord <- transmitter_codespace <- transmitter_id <- code_space <- id_code <-
    sensor_value_real <- sensor_intercept <- sensor_value <- sensor_slope <- NULL

  # coerce to data.table
  dtc <- data.table::as.data.table(det)

  dtc[, ord := 1:.N] # original order

  data.table::setkey(dtc, transmitter_codespace, transmitter_id)

  # coerce to data.table
  tags <- data.table::as.data.table(tag_specs)

  data.table::setkey(tags, code_space, id_code)

  tags <- tags[, c(
    "code_space", "id_code", "sensor_range", "sensor_units",
    "sensor_slope", "sensor_intercept", "accel_algorithm",
    "accel_sample_rate", "sensor_transmit_ratio"
  ),
  with = FALSE
  ]

  # merge
  dtc <- merge(dtc, tags,
    by.x = data.table::key(dtc),
    by.y = data.table::key(tags)
  )


  data.table::setkey(dtc, ord) # reorder to original

  # drop unwanted column
  drop_col <- "ord"
  dtc[, !drop_col, with = FALSE]

  # calculate real values
  dtc[, sensor_value_real := sensor_intercept + (sensor_value * sensor_slope)]


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
