#' @title Identify new location based on distance and bearing from another
#'
#' @description Calculates latitude and longitude for new point that is x meters
#'   away at bearing y from a geographic location (Longitude, Latitude) using
#'   great circle distances.
#'
#' @param lon vector of longitudes (dd) to calculate offset points
#' @param lat vector of latitudes (dd) to calculate offset points
#' @param offsetDist vector of distances to calculate offset point (meters or
#'   feet)
#' @param offsetDir vector of directions to calculate point from starting point.
#'   Options are NA,"N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S",
#'   "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"
#' @param distUnit specify meters or ft ("m" or "ft")
#'
#' @examples
#' lat <- rep(44.0, 17)
#' lon <- rep(-83.0, 17)
#'
#' offsetDir <- c(
#'   NA, "N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S",
#'   "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"
#' )
#'
#' offsetDist <- seq(100, 1700, by = 100)
#' distUnit <- "m"
#'
#' point_offset(lon, lat, offsetDist, offsetDir, distUnit)
#'
#' @export

point_offset <- function(lon = NA, lat = NA, offsetDist = NA, offsetDir = NA,
                         distUnit = "m") {
  if (distUnit == "ft") offsetDist <- 0.3048 * offsetDist # convert to m if needed
  if (!(distUnit) %in% c("ft", "m")) {
    stop("Input attribute 'dirUnit' must be 'm' (meters) or 'ft' (feet).")
  }

  dirKey <- data.frame(
    txt = c(
      NA, "N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW",
      "SW", "WSW", "W", "WNW", "NW", "NNW"
    ),
    deg = c(
      NA, 0, 22.5, 45, 67.5, 90, 112.5, 135, 157.5, 180, 202.5, 225, 247.5,
      270, 292.5, 315, 337.5
    ),
    stringsAsFactors = F
  )

  bearing <- dirKey$deg[match(offsetDir, dirKey$txt)]

  haversine <- function(lon, lat, bearing, offsetDist) {
    lat <- lat * pi / 180
    lon <- lon * pi / 180
    R <- 6378137
    bearing <- bearing * pi / 180
    lat2 <- asin(sin(lat) * cos(offsetDist / R) + cos(lat) * sin(offsetDist / R) * cos(bearing))
    lon2 <- 180 / pi * (
      lon + atan2(
        sin(bearing) * sin(offsetDist / R) * cos(lat),
        cos(offsetDist / R) - sin(lat) * sin(lat2)
      )
    )

    lat2 <- 180 / pi * lat2

    coords <- matrix(
      c(lon2, lat2),
      ncol = 2,
      dimnames = list(NULL, c("lon", "lat"))
    )

    return(coords)
  }

  pos <- haversine(lon, lat, bearing, offsetDist)
  return(pos)
}
