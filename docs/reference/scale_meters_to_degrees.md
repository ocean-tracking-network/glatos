# Get degree-scale equivalent of meter-scale distance on a spatial object

Get degree-scale equivalent of meter-scale distance on an `sf` object.

## Usage

``` r
scale_meters_to_degrees(x, sf, ref = "center", epsg = 3175)
```

## Arguments

- x:

  Distance, in meters (or units of `epsg`), to be converted to degrees
  along each dimension of `sf`. May be a two element vector (denoting
  distance along x and y axes, respectively) or a single number (same
  distance along both axes).

- sf:

  An `sf` spatial object.

- ref:

  Reference point for scale. If `center` (default), then returned value
  is the distance (in degrees), along each dimension, at the center of
  `sf` that is equivalent to `x`. Other possible values are `min` and
  `max`.

- epsg:

  Code denoting coordinate reference system in which calculations are
  made. Must be a Cartesian projection.

## Value

A two-element vector with distance in x and y (longitude and latitude)
dimensions, respectively.

## Details

A helper function to determine input resolution to
[`make_transition()`](https://ocean-tracking-network.github.io/glatos/reference/make_transition.md).

## Examples

``` r

# how many long, lat equal to 5000 m at center of great_lakes_polygon?
scale_meters_to_degrees(
  x = 5000,
  sf = great_lakes_polygon
)
#> [1] 0.06517904 0.04591286

# how many long, lat equal to 5000 m at top of great_lakes_polygon?
scale_meters_to_degrees(
  x = 5000,
  sf = great_lakes_polygon,
  ref = "max"
)
#> [1] 0.07009974 0.04604033

# how many long, lat equal to 5000 m at bottom of great_lakes_polygon?
scale_meters_to_degrees(
  x = 5000,
  sf = great_lakes_polygon,
  ref = "min"
)
#> [1] 0.06090292 0.04600239
```
