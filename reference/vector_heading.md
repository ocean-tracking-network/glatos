# Calculate direction (heading) of a vector (in degrees)

Calculate direction (heading) of each link of a vector (in degrees)

## Usage

``` r
vector_heading(x, y = NULL, coord_sys = NA)
```

## Arguments

- x:

  A numeric vector of x coordinates; minimum of 2.  
  OR  
  A two-column matrix or data frame with x coordinates in column 1 and y
  coordinates in column 2.

- y:

  A numeric vector of y coordinates; minimum of 2.

- coord_sys:

  The type of geographical coordinate system used. Possible values are
  `NA` (for any cartesian grid; e.g., UTM) or `longlat` (for WGS84 in
  decimal degrees).

## Value

A numeric scalar with heading in degrees or a numeric vector of headings
if `length(x) > 2`.

If units are decimal degrees (i.e., `coord_sys = "longlat"`) then the
angles returned will represent the heading at the start of each vector.

## Details

Calculates direction (in degrees) for each of k-1 vectors, where k =
length(x) - 1. Lengths of `x` and `y` must be equal.

## Note

This function is called from within
[`crw_in_polygon()`](https://github.io/reference/crw_in_polygon.md)

## Author

C. Holbrook (cholbrook@glfc.org)

## Examples

``` r

# example using generic cartesian (regular grid) coordinates
x <- c(2, 4)
y <- c(2, 4)
vector_heading(x, y)
#> [1] 45

x2 <- c(2, 4, 2)
y2 <- c(2, 4, 2)
vector_heading(x2, y2)
#> [1]  45 225


# example using WGS84 lat-lon
# e.g., from Duluth to Toronto to Detroit

path1 <- data.frame(
  city = c("Duluth", "Toronoto", "Detroit"),
  longitude = c(-92.1005, -79.3832, -83.0458),
  latitude = c(46.7867, 43.6532, 42.3314)
)

# example using the x, y input method way
vector_heading(
  x = c(-92.1005, -79.3832, -83.0458),
  y = c(46.7867, 43.6532, 42.3314),
  coord_sys = "longlat"
)
#> [1] 104.6800 244.9958

# example using the x-only input method
vector_heading(
  x = path1[, c("longitude", "latitude")],
  coord_sys = "longlat"
)
#> [1] 104.6800 244.9958
```
