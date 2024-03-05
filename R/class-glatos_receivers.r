#' Construct, check, and validate a glatos_receivers object
#'
#' @description Creates, checks, or validates a glatos_receivers object.
#'
#' @param ... Named vectors, minimally one for each required column of the
#'   specified class:
#'
#'   \describe{
#'   \item{`deploy_lat`}{numeric, latitude, in decimal degrees, WGS84, southern
#'   hemisphere is negative.}
#'   \item{`deploy_long`}{numeric, longitude, in decimal degrees, WGS84, western
#'    hemistphere is negative.}
#'   \item{`deploy_date_time`}{must be POSIXct, timestamp (in UTC) when
#'   receiver was deployed (i.e., start of telemetry sampling interval.)}
#'   \item{`recover_date_time`}{must be POSIXct, timestamp (in UTC) when
#'   receiver was recovered (i.e., end of telemetry sampling interval.)}
#'   \item{`ins_serial_no`}{must be character, unique serial number of receiver.
#'   }
#'   }
#'
#' @param x A data.frame or object that inherits from data.frame (e.g.,
#'   data.table, tibble) and contains all required columns (see `...`).
#'
#' @param validate logical, indicates if column names and classes should be
#'   checked against requirements.
#'
#' @examples
#'
#' #  glatos_receivers
#' x <- data.frame(
#'   station = c("WHT-009", "FDT-001", "FDT-004", "FDT-003"),
#'   deploy_lat = c(43.7, 45.9, 45.9, 45.9),
#'   deploy_long = c(-82.5, -83.5, -83.5, -83.5),
#'   deploy_date_time = as.POSIXct(
#'     c(
#'       "2010-09-22 18:05:00",
#'       "2010-11-12 15:07:00",
#'       "2010-11-12 15:36:00",
#'       "2010-11-12 15:56:00"
#'     ),
#'     tz = "UTC"
#'   ),
#'   recover_date_time = as.POSIXct(
#'     c(
#'       "2012-08-15 16:52:00",
#'       "2012-05-15 13:25:00",
#'       "2012-05-15 14:15:00",
#'       "2012-05-15 14:40:00"
#'     ),
#'     tz = "UTC"
#'   ),
#'   ins_serial_no = c("109450", "442", "441", "444")
#' )
#'
#' gr_df1 <- glatos_receivers(
#'   station = x$station,
#'   deploy_lat = x$deploy_lat,
#'   deploy_long = x$deploy_long,
#'   deploy_date_time = x$deploy_date_time,
#'   recover_date_time = x$recover_date_time,
#'   ins_serial_no = x$ins_serial_no
#' )
#'
#'
#' # as_glatos_receivers
#' gr_df2 <- as_glatos_receivers(x)
#'
#'
#' # sf input
#'
#' library(sf)
#'
#' # use remove = FALSE to keep required columns
#' x_sf <- sf::st_as_sf(x,
#'   coords = c("deploy_long", "deploy_lat"),
#'   remove = FALSE
#' )
#'
#' gr_sf <- as_glatos_receivers(x_sf)
#'
#'
#' # tibble input
#' library(tibble)
#'
#' x_tbl <- as_tibble(x)
#'
#' gr_tbl <- as_glatos_receivers(x_tbl)
#'
#' # All below will error as invalid
#'
#' # data.frame input; missing column name
#' library(dplyr) # for rename
#' x2 <- rename(x,
#'   receiver_loc = station,
#'   deploy_timestamp = deploy_date_time
#' )
#'
#' try(
#'   gr2 <- as_glatos_receivers(x2)
#' )
#'
#' # data.frame input; wrong column class
#' x3 <- mutate(x,
#'   ins_serial_no = as.integer(ins_serial_no),
#'   deploy_date_time = as.character(deploy_date_time)
#' )
#'
#' try(
#'   gr3 <- as_glatos_receivers(x3)
#' )
#'
#' # Validation and checking
#'
#' validate_glatos_receivers(x)
#'
#' is_glatos_receivers(x) # FALSE
#'
#' is_glatos_receivers(gr_df1) # TRUE

#' @section Construction: `glatos_receivers()` creates a `glatos_receivers` from
#'   individual vectors (one for each column) and optionally checks for required
#'   column names and classes using `validate_glatos_receivers()`.
#' @export
glatos_receivers <- function(..., validate = TRUE) {
  inargs <- list(...)

  x <- as.data.frame(inargs)

  x <- as_glatos_receivers(x, validate = validate)

  return(x)
}

#' @section Coercion: `as_glatos_receivers()` coerces a data.frame, or object that
#'   inherits from data.frame, to `glatos_receivers` and optionally checks for
#'   required column names and classes using `validate_glatos_receivers()`.
#' @rdname glatos_receivers
#' @export
as_glatos_receivers <- function(x, validate = TRUE) {
  # Input must inherit from data frame
  if (!inherits(x, "data.frame")) stop("Input x must inherit from data.frame.")

  # add new class as first but keep existing (e.g., data.frame)
  class(x) <- c("glatos_receivers", class(x))

  if (validate) validate_glatos_receivers(x)

  return(x)
}

#' @section Validation:
#' `is_glatos_receivers()` checks class attribute for `"glatos_receivers"`
#' @rdname glatos_receivers
#' @export
is_glatos_receivers <- function(x) inherits(x, "glatos_receivers")


#' @section Validation:
#' `validate_glatos_receivers()` checks for required column names and classes
#' @rdname glatos_receivers
#' @export
validate_glatos_receivers <- function(x) {
  req_cols <- list(
    deploy_lat = "numeric",
    deploy_long = "numeric",
    deploy_date_time = "POSIXct",
    recover_date_time = "POSIXct",
    ins_serial_no = "character"
  )

  glatos_check_col_names(x, req_cols)

  # Check column classes

  glatos_check_col_classes(x, req_cols)

  return(TRUE)
}
