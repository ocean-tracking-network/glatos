#' Construct, check, and validate a glatos_detections object
#'
#' @description Creates, checks, or validates a glatos_detections object.
#'
#' @param ... Named vectors, minimally one for each required column of the
#'   specified class:
#'
#'   \describe{
#'   \item{`animal_id`}{must be character, indifies unique individual animal.}
#'   \item{`detection_timestamp_utc`}{must be POSIXct, timestamps(in UTC) of
#'   detection.}
#'   \item{`deploy_lat`}{must be numeric, latitude, in decimal degrees, WGS84,
#'   southern hemisphere is negative.}
#'   \item{`deploy_long`}{must be numeric, longitude, in decimal degrees,
#'    WGS84, western hemisphere is negative.}
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
#' #  glatos_detections
#' x <- data.frame(
#'   animal_id = c("153", "153", "153", "153"),
#'   detection_timestamp_utc = as.POSIXct(
#'     c(
#'       "2012-04-29 01:48:37",
#'       "2012-04-29 01:52:55",
#'       "2012-04-29 01:55:12",
#'       "2012-04-29 01:56:42"
#'     ),
#'     tz = "UTC"
#'   ),
#'   deploy_lat = c(43.39165, 43.39165, 43.39165, 43.39165),
#'   deploy_long = c(-83.99264, -83.99264, -83.99264, -83.99264)
#' )
#'
#' gd_df1 <- glatos_detections(
#'   animal_id = x$animal_id,
#'   detection_timestamp_utc =
#'     x$detection_timestamp_utc,
#'   deploy_lat = x$deploy_lat,
#'   deploy_long = x$deploy_long
#' )
#'
#'
#' # as_glatos_detections
#' gd_df2 <- as_glatos_detections(x)
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
#' gd_sf <- as_glatos_detections(x_sf)
#'
#'
#' # tibble input
#' library(tibble)
#'
#' x_tbl <- as_tibble(x)
#'
#' gd_tbl <- as_glatos_detections(x_tbl)
#'
#'
#' # All below will error as invalid
#'
#' # data.frame input; missing column name
#' library(dplyr) # for rename
#' x2 <- rename(x,
#'   fish_id = animal_id,
#'   det_date_time = detection_timestamp_utc
#' )
#'
#' try(
#'   gd2 <- as_glatos_detections(x2)
#' )
#'
#' # data.frame input; wrong column class
#' x3 <- mutate(x,
#'   animal_id = as.integer(animal_id),
#'   detection_timestamp_utc = as.character(detection_timestamp_utc)
#' )
#'
#' try(
#'   gr3 <- as_glatos_detections(x3)
#' )
#'
#' # Validation and checking
#'
#' validate_glatos_detections(x)
#'
#' is_glatos_detections(x) # FALSE
#'
#' is_glatos_detections(gd_df1) # TRUE

#' @section Construction: `glatos_detections()` creates a `glatos_detections`
#'   object from individual vectors (one for each column) and optionally checks
#'   for required column names and classes using `validate_glatos_detections()`.
#' @export
glatos_detections <- function(..., validate = TRUE) {
  inargs <- list(...)

  x <- as.data.frame(inargs)

  x <- as_glatos_detections(x, validate = validate)

  return(x)
}

#' @section Coercion: `as_glatos_detections()` coerces a data.frame, or object that
#'   inherits from data.frame, to `glatos_detections` and optionally checks for
#'   required column names and classes using `validate_glatos_detections()`.
#' @rdname glatos_detections
#' @export
as_glatos_detections <- function(x, validate = TRUE) {
  # Input must inherit from data frame
  if (!inherits(x, "data.frame")) stop("Input x must inherit from data.frame.")

  # add new class as first but keep existing (e.g., data.frame)
  class(x) <- c("glatos_detections", class(x))

  if (validate) validate_glatos_detections(x)

  return(x)
}

#' @section Validation:
#' `is_glatos_detections()` checks class attribute for `"glatos_detections"`
#' @rdname glatos_detections
#' @export
is_glatos_detections <- function(x) inherits(x, "glatos_detections")


#' @section Validation:
#' `validate_glatos_detections()` checks for required column names and classes
#' @rdname glatos_detections
#' @export
validate_glatos_detections <- function(x) {
  req_cols <- list(
    animal_id = "character",
    detection_timestamp_utc = "POSIXct",
    deploy_lat = "numeric",
    deploy_long = "numeric"
  )

  glatos_check_col_names(x, req_cols)

  # Check column classes

  glatos_check_col_classes(x, req_cols)

  return(TRUE)
}
