#' @title Calculate direction (heading) of a vector (in degrees)
#'
#' @description
#' Calculate direction (heading) of each link of a vector (in degrees)
#'
#' @param x A numeric vector of x coordinates; minimum of 2.\cr
#'          OR \cr
#'          A two-column matrix or data frame with x coordinates in
#'          column 1 and y coordinates in column 2.
#' @param y A numeric vector of y coordinates; minimum of 2.
#' @param coord_sys The type of geographical coordinate system used. Possible
#'   values are `NA` (for any cartesian grid; e.g., UTM) or `longlat` (for
#'   WGS84 in decimal degrees).
#'
#' @details
#' Calculates direction (in degrees) for each of k-1 vectors, where
#' k = length(x) - 1. Lengths of `x` and `y` must be
#' equal.
#'
#' @return A numeric scalar with heading in degrees or a numeric vector of
#' headings if `length(x) > 2`.
#'
#' If units are decimal degrees (i.e., `coord_sys = "longlat"`) then
#' the angles returned will represent the heading at the start of each vector.
#'
#' @note
#' This function is called from within [crw_in_polygon()]
#'
#' @author C. Holbrook (cholbrook@usgs.gov)
#'
#' @examples
#'
#' # example using generic cartesian (regular grid) coordinates
#' x <- c(2, 4)
#' y <- c(2, 4)
#' vector_heading(x, y)
#'
#' x2 <- c(2, 4, 2)
#' y2 <- c(2, 4, 2)
#' vector_heading(x2, y2)
#'
#'
#' # example using WGS84 lat-lon
#' # e.g., from Duluth to Toronto to Detroit
#'
#' path1 <- data.frame(
#'   city = c("Duluth", "Toronoto", "Detroit"),
#'   longitude = c(-92.1005, -79.3832, -83.0458),
#'   latitude = c(46.7867, 43.6532, 42.3314)
#' )
#'
#' # example using the x, y input method way
#' vector_heading(
#'   x = c(-92.1005, -79.3832, -83.0458),
#'   y = c(46.7867, 43.6532, 42.3314),
#'   coord_sys = "longlat"
#' )
#'
#' # example using the x-only input method
#' vector_heading(
#'   x = path1[, c("longitude", "latitude")],
#'   coord_sys = "longlat"
#' )
#'
#' @export
vector_heading <- function(x, y = NULL, coord_sys = NA) {
  # check if x is two-column matrix or data.frame
  if (!is.vector(x)) {
    if (ncol(x) == 2) {
      x2 <- x[, 1]
      y2 <- x[, 2]

      if (!is.null(y)) {
        warning("Input object 'y' was ignored because 'x' was two-column object.")
      }

      x <- x2
      y <- y2
    } else {
      stop("Input 'x' must either be vector or two-column object.")
    }
  }

  # convenience functions
  deg2rad <- function(x) x * pi / 180
  rad2deg <- function(x) x * 180 / pi

  if (is.na(coord_sys)) {
    theta_rad <- atan2(diff(x), diff(y))
  } else if (coord_sys == "longlat") {
    lat_rad <- deg2rad(y)
    lon_dif <- deg2rad(diff(x))
    term1 <- sin(lon_dif) * cos(lat_rad[-1])
    term2 <- cos(lat_rad[-length(lat_rad)]) * sin(lat_rad[-1])
    term3 <- sin(lat_rad[-length(lat_rad)]) * cos(lat_rad[-1]) * cos(lon_dif)
    theta_rad <- atan2(term1, term2 - term3)
  } else {
    stop("Invalid input for 'coord_sys' argument.")
  }

  # convert to degrees
  return(rad2deg(theta_rad) %% 360)
}
