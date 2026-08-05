# Round timestamp by fractional second and coerce to character

Round timestamp by fractional second and coerce to character

## Usage

``` r
format_POSIXt(x, digits = 0, drop0trailing = TRUE)
```

## Arguments

- x:

  A `POSIXct` or `POSIXlt` object.

- digits:

  The number of decimal places to which seconds is rounded.

- drop0trailing:

  logical (default = TRUE), indicating if trailing zeros, i.e., "0"
  after the decimal mark, should be removed. Passed to
  [format](https://rdrr.io/r/base/format.html) which passes to
  [prettyNum](https://rdrr.io/r/base/formatc.html).

## Value

A character vector in format like `"%Y-%m-%d %H:%M:%OSn"` (see
[strptime](https://rdrr.io/r/base/strptime.html) but see 'detail' for
differences).

## Details

Differs from e.g., `format(x, format = "%Y-%m-%d %H:%M:%OS6")` in that
(1) rounding is used (not truncation) and (2) trailing 0s can be omitted
(via `drop0trailing`).

Differs from `lubridate::round_Date` in that it is accurate for \< 1 sec
(see example 1 below for motivating example) but requires coercion to
POSIXlt before rounding and coercing to character.

## Examples

``` r

# Example 1 - motivating example: e.g., trouble with lubridate::round_Date
t1 <- as.POSIXct("2011-05-08 05:37:39.245541", tz = "UTC")
format(t1, digits = 6)
#> [1] "2011-05-08 05:37:39.245541"

t2 <- lubridate::round_date(t1, unit = "0.00001s")
format(t2, digits = 6)
#> [1] "2011-05-08 05:37:39.245539"

t3 <- format_POSIXt(t1, digits = 5)
format(t3, digits = 6)
#> [1] "2011-05-08 05:37:39.24554"

# Example 2
t1 <- as.POSIXct(
  c(
    "2011-03-08 23:59:58",
    "2011-03-08 23:59:58.828867"
  ),
  tz = "UTC"
)
format_POSIXt(t1, digits = 5, drop0trailing = FALSE)
#> [1] "2011-03-08 23:59:58.00000" "2011-03-08 23:59:58.82887"
format_POSIXt(t1, digits = 5, drop0trailing = TRUE)
#> [1] "2011-03-08 23:59:58"       "2011-03-08 23:59:58.82887"
```
