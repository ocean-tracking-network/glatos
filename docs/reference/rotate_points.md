# Rotate points in a 2-d plane

Rotate points around a point in a 2-d plane

## Usage

``` r
rotate_points(x, y, theta, focus)
```

## Arguments

- x:

  A numeric vector of x coordinates; minimum of 2.

- y:

  A numeric vector of y coordinates; minimum of 2.

- theta:

  A numeric scalar with the angle of rotation in degrees; positive is
  clockwise.

- focus:

  A numeric vector of x (first element) and y (second element)
  coordinates for the point around which `x` and `y` will rotate.

## Value

A two-column data frame containing:

- x:

  x coordinates

- y:

  y coordinates

## Details

Points are shifted to be centered at the focus, then rotated using a
rotation matrix, then shifted back to original focus.

## Note

This function is called from
[`crw_in_polygon()`](https://ocean-tracking-network.github.io/glatos/reference/crw_in_polygon.md)

## Author

C. Holbrook (cholbrook@glfc.org)

## Examples

``` r
x <- runif(10, 0, 10)
y <- runif(10, 0, 10)
plot(x, y, type = "b", pch = 20)
foo <- rotate_points(x, y, 20, c(5, 5))
points(foo$x, foo$y, type = "b", pch = 20, col = "red")

```
