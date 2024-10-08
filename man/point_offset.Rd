% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util-point_offset.r
\name{point_offset}
\alias{point_offset}
\title{Identify new location based on distance and bearing from another}
\usage{
point_offset(
  lon = NA,
  lat = NA,
  offsetDist = NA,
  offsetDir = NA,
  distUnit = "m"
)
}
\arguments{
\item{lon}{vector of longitudes (dd) to calculate offset points}

\item{lat}{vector of latitudes (dd) to calculate offset points}

\item{offsetDist}{vector of distances to calculate offset point (meters or
feet)}

\item{offsetDir}{vector of directions to calculate point from starting point.
Options are NA,"N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S",
"SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"}

\item{distUnit}{specify meters or ft ("m" or "ft")}
}
\description{
Calculates latitude and longitude for new point that is x meters
away at bearing y from a geographic location (Longitude, Latitude) using
great circle distances.
}
\examples{
lat <- rep(44.0, 17)
lon <- rep(-83.0, 17)

offsetDir <- c(
  NA, "N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S",
  "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"
)

offsetDist <- seq(100, 1700, by = 100)
distUnit <- "m"

point_offset(lon, lat, offsetDist, offsetDir, distUnit)

}
