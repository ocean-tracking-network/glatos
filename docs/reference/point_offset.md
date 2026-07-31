# Identify new location based on distance and bearing from another

Calculates latitude and longitude for new point that is x meters away at
bearing y from a geographic location (Longitude, Latitude) using great
circle distances.

## Usage

``` r
point_offset(
  lon = NA,
  lat = NA,
  offsetDist = NA,
  offsetDir = NA,
  distUnit = "m"
)
```

## Arguments

- lon:

  vector of longitudes (dd) to calculate offset points

- lat:

  vector of latitudes (dd) to calculate offset points

- offsetDist:

  vector of distances to calculate offset point (meters or feet)

- offsetDir:

  vector of directions to calculate point from starting point. Options
  are NA,"N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW",
  "SW", "WSW", "W", "WNW", "NW", "NNW"

- distUnit:

  specify meters or ft ("m" or "ft")

## Examples

``` r
lat <- rep(44.0, 17)
lon <- rep(-83.0, 17)

offsetDir <- c(
  NA, "N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S",
  "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"
)

offsetDist <- seq(100, 1700, by = 100)
distUnit <- "m"

point_offset(lon, lat, offsetDist, offsetDir, distUnit)
#>             lon      lat
#>  [1,]        NA       NA
#>  [2,] -83.00000 44.00180
#>  [3,] -82.99857 44.00249
#>  [4,] -82.99647 44.00254
#>  [5,] -82.99423 44.00172
#>  [6,] -82.99251 44.00000
#>  [7,] -82.99192 43.99759
#>  [8,] -82.99294 43.99492
#>  [9,] -82.99570 43.99253
#> [10,] -83.00000 43.99102
#> [11,] -83.00526 43.99087
#> [12,] -83.01060 43.99238
#> [13,] -83.01500 43.99553
#> [14,] -83.01748 44.00000
#> [15,] -83.01731 44.00516
#> [16,] -83.01413 44.01016
#> [17,] -83.00813 44.01411
```
