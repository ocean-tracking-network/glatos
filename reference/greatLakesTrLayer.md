# A `TransitionLayer` of the Great Lakes that only prevents transition over land

A TransitionLayer object that only allows transitions to occur within
water (i.e., prohibits movement onto land).

## Usage

``` r
greatLakesTrLayer
```

## Format

An object of class `TransitionLayer` of dimension 667 x 667 x 1.

## Details

This dataset was developed for non-linear interpolation of fish movement
paths from telemetry data and is used by default in
[interpolate_path](https://github.io/reference/interpolate_path.md).

Created from
[great_lakes_polygon](https://github.io/reference/great_lakes_polygon.md);
see 'data-raw/data-greatLakesTrLayer.r'.

## See also

[interpolate_path](https://github.io/reference/interpolate_path.md),
[gdistance](https://AgrDataSci.github.io/gdistance/reference/gdistance.html)

## Author

Todd Hayden (rebuilt by C. Holbrook)

## Examples

``` r
if (FALSE) { # \dontrun{
raster::plot(raster::raster(greatLakesTrLayer))
} # }
```
