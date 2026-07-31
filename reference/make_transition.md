# Create transition layer from spatial object.

Create transition layer for
[`interpolate_path()`](https://github.io/reference/interpolate_path.md)
spatial object.

## Usage

``` r
make_transition(poly, res, receiver_points = NULL, epsg = 3175, buffer = NULL)
```

## Arguments

- poly:

  A spatial polygon object of class
  [SpatialPolygonsDataFrame](https://edzer.github.io/sp/reference/SpatialPolygons.html)
  or a [sf::sf()](https://r-spatial.github.io/sf/reference/sf.html)
  object with a geometry column of polygon and/or multipolygon objects.

- res:

  two element vector that specifies the x and y dimension of output
  raster cells. Units are same as `poly` crs. May be calculated from
  desired resolution in meters using
  [`scale_meters_to_degrees()`](https://github.io/reference/scale_meters_to_degrees.md).

- receiver_points:

  Object containing coordinates of receiver locations. Must be of class
  `SpatialPointsDataFrame`, `SpatialPoints`, `sf`, `glatos_receivers`,
  or `glatos_detections`.

- epsg:

  coordinate reference code that describes projection used for buffers.
  Defaults to NAD83/Great Lakes and St. Lawrence Albers.

- buffer:

  Buffer, in same units as `epsg`, that will be added to `poly` before
  rasterization.

## Value

A list with two elements:

- transition:

  a geo-corrected transition raster layer where land = 0 and water = 1
  (see
  [`gdistance::Transition`](https://AgrDataSci.github.io/gdistance/reference/Transition-classes.html))

- rast:

  rasterized input layer of class `raster`

## Details

`make_transition` uses
[`jarasterize()`](https://github.io/reference/jarasterize.md) to convert
a polygon shapefile into a raster layer and geo-corrected transition
layer
[`interpolate_path()`](https://github.io/reference/interpolate_path.md).
Raster cell values on land equal 0, cells in water equal 1. Output is a
two-object list containing the raster layer and transition layer.

If `receiver_points` is provided, any receiver not in water is buffered
by the distance from the receiver to the nearest water. This allows all
receivers to be coded as in water if the receiver is on land.

Poly object is transformed into planer map projection specified by epsg
argument for calculation of transition object if receiver_points is
provided. Output is projected to crs of input `poly`.

output transition layer is corrected for projection distortions using
[`gdistance::geoCorrection`](https://AgrDataSci.github.io/gdistance/reference/geoCorrection.html).
Adjacent cells are connected by 16 directions and transition function
returns 0 (land) for movements between land and water and \>0 for all
over-water movements.

Note that this function underwent breaking changes between 0.7.3 and
0.8.0 (uses `jasterize` instead of `gdalUtilities::gdal_rasterize` see
NEWS).

## Author

Todd Hayden, Chris Holbrook

## Examples

``` r

# Example 1 - read from sf polygon object
# use example polygon for Great lakes

# calculate resolution in degrees (from meters)
#  note this applies to cell at center, cell sizes will vary
res <- scale_meters_to_degrees(5000, sf = great_lakes_polygon)

# make_transition layer
tst <- make_transition(great_lakes_polygon, res = res)
#> Rasterizing...
#> Done (3 secs)
#> Making transition layer...
#> Done (1.9 secs)

if (FALSE) { # \dontrun{
# plot raster layer (notice water = 1, land = 0)
raster::plot(tst$rast)

# compare to polygon
plot(sf::st_geometry(great_lakes_polygon), add = TRUE)
} # }

# Example 2 - add 1 km buffer (same resolution)

# make_transition layer
tst2 <- make_transition(great_lakes_polygon,
  res = res,
  buffer = 3000
)
#> Rasterizing...
#> Done (2 secs)
#> Making transition layer...
#> Done (1 secs)

if (FALSE) { # \dontrun{
# plot raster layer (notice water = 1, land = 0)
raster::plot(tst2$rast)

# compare to polygon
plot(sf::st_geometry(great_lakes_polygon), add = TRUE)
} # }

# Example 3 - read from ESRI Shapefile and include receiver file
# to account for any receivers outside of great lakes polygon

# path to polygon shapefile
poly <- system.file("extdata", "shoreline.zip", package = "glatos")
poly <- sf::st_read(paste0("/vsizip/", poly))
#> Reading layer `shoreline' from data source 
#>   `/vsizip//home/runner/work/_temp/Library/glatos/extdata/shoreline.zip' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 4 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -92.19876 ymin: 41.32391 xmax: -70.4547 ymax: 49.01076
#> Geodetic CRS:  WGS 84

# read in glatos receivers object
rec_file <- system.file("extdata", "sample_receivers.csv",
  package = "glatos"
)
recs <- read_glatos_receivers(rec_file)

# change a coordinate to on-land to show impact...
recs[1, "deploy_lat"] <- recs[1, "deploy_lat"] + 4

# make_transition layer (roughly 500 m res)
tst <- make_transition(poly, res = c(0.065, 0.046), receiver_points = recs)
#> Rasterizing...
#> Done (3.3 secs)
#> Making transition layer...
#> Done (1.2 secs)

if (FALSE) { # \dontrun{
# plot raster layer
# notice the huge circle rasterized as "water"  north of Lake Superior.
# This occurred because we had a "receiver" deployed at that locations
raster::plot(tst$rast)
points(recs$deploy_long, recs$deploy_lat, col = "red", pch = 20)

# plot transition layer
raster::plot(raster::raster(tst$transition))
} # }

# Example 4- transition layer of Lake Huron only with receivers

# transform to great lakes projection
poly <- sf::st_transform(great_lakes_polygon, crs = 3175)

# set attribute-geometry relationship to constant.
# this avoids error when cropping
sf::st_agr(poly) <- "constant"

# crop Great lakes polygon file
poly <- sf::st_crop(
  x = poly, xmin = 829242.55, ymin = 698928.27,
  xmax = 1270000.97, ymax = 1097196.15
)

# read in glatos receivers object
rec_file <- system.file("extdata", "sample_receivers.csv",
  package = "glatos"
)
recs <- read_glatos_receivers(rec_file)

# extract receivers in "HECWL" project
# all receiver stations except one is in Lake Huron
recs <- recs[recs$glatos_project == "HECWL", ]

# remove two stations not in Lake Huron
recs <- recs[!recs$glatos_array %in% c("MAU", "LVD"), ]

# convert recs to simple feature  object (sf)
recs <- sf::st_as_sf(recs,
  coords = c("deploy_long", "deploy_lat"),
  crs = 4326
)

# transform receivers to same projection as great lakes polygon
recs <- sf::st_transform(recs, crs = 3175)

if (FALSE) { # \dontrun{
# check by plotting
plot(sf::st_geometry(poly), col = NA)
plot(sf::st_geometry(recs), col = "red", add = TRUE)
} # }

# create slightly higher resolution transition layer
#   (note that res in in meters here because crs is 3175)
tst1 <- make_transition(poly, res = 5000, receiver_points = recs)
#> Rasterizing...
#> Done (0.5 secs)
#> Making transition layer...
#> Done (0.1 secs)

if (FALSE) { # \dontrun{
# plot raster layer
raster::plot(tst1$rast)

plot(sf::st_geometry(recs),
  add = TRUE, col = "red", pch = 20
)

# plot transition layer
raster::plot(raster::raster(tst1$transition))
} # }
```
