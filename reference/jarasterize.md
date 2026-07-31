# Just another rasterizer

An 'all-touched-capable' rasterizer that depends only on `sf` and
`raster`.

## Usage

``` r
jarasterize(
  x,
  res,
  value = 1,
  bg_value = 0,
  all_touched = TRUE,
  silent = FALSE
)
```

## Arguments

- x:

  An `sf` object (polygon, lines, or points).

- res:

  Resolution (raster cell size) in units of `x`'s `crs`. May be either a
  2-element vector (for lon and lat, respectively) or a single value
  (same value used for each dimension).

- value:

  The value assigned to cells matching `x`.

- bg_value:

  The value assigned to cells not matching `x`.

- all_touched:

  If `TRUE` (default), raster will return `value` for every cell touched
  by polygon (and `bg_value` for all others); otherwise, raster will
  return `value` for all cells whose center points are within the
  polygon.

- silent:

  If false (default), progress messages are not displayed.

## Value

A `raster::rasterLayer` object.

## Examples

``` r

# Example 1. lon lat WGS input

poly1 <- great_lakes_polygon

plot(sf::st_geometry(poly1))


rast1 <- jarasterize(poly1, res = c(0.1, 0.05))
#> Rasterizing...
#> Done (2.5 secs)

if (FALSE) { # \dontrun{
# compare to polygon
x11(width = 12, height = 8)
raster::plot(rast1)
plot(sf::st_geometry(poly1), add = TRUE)
} # }

# Example 2. projected input; 5 km cell size

poly2 <- sf::st_transform(poly1, crs = 3175)

rast2 <- jarasterize(poly2, res = 5000)
#> Rasterizing...
#> Done (2.8 secs)

if (FALSE) { # \dontrun{
# compare to polygon
x11(width = 12, height = 8)
raster::plot(rast2)
plot(sf::st_geometry(poly2), add = TRUE)
} # }

# Example 3. projected input; 5 km cell size; all_touched = FALSE

rast3 <- jarasterize(poly2, res = 5000, all_touched = FALSE)
#> Rasterizing...
#> Done (0.3 secs)

if (FALSE) { # \dontrun{
# compare to polygon
x11(width = 12, height = 8)
raster::plot(rast3)
plot(sf::st_geometry(poly2), add = TRUE)
} # }
```
