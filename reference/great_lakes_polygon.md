# An sf POLYGON object with Great Lakes coastline

An sf POLYGON object with Great Lakes coastline, used as default map
background in several [glatos](https://github.io/reference/glatos.md)
functions.

## Usage

``` r
great_lakes_polygon
```

## Format

An object of class `sf` (inherits from `data.frame`) with 1 rows and 2
columns.

## Details

Created from [shoreline](https://github.io/reference/shoreline.md)
shapefile (see 'data-raw/data-great_lakes_polygon.r).

## Author

Todd Hayden (coerced to sf via by C. Holbrook)

## Examples

``` r
if (FALSE) { # \dontrun{
plot(sf::st_geometry(great_lakes_polygon))
} # }
```
