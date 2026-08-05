# Check a time zone string against OlsonNames()

Check a time zone string against OlsonNames()

## Usage

``` r
check_timezone(tz, ignore.case = FALSE)
```

## Arguments

- tz:

  a character string. The time zone specification to be used for the
  conversion. Only values in
  [`OlsonNames()`](https://rdrr.io/r/base/timezones.html) are allowed.
  `""` is not allowed.

- ignore.case:

  logical. if FALSE, the pattern matching is case sensitive and if TRUE
  (default), case is ignored during matching. Passed to
  [`grep()`](https://rdrr.io/r/base/grep.html).

## Value

If `tz` is valid (depends on `ignore.case`), then it is returned.
Otherwise, `NA` is returned.

## Examples

``` r
if (FALSE) { # \dontrun{

x <- c("UTC", "US/Eastern", "US/EASTERN", "foo")

check_timezone(tz = x)

check_timezone(tz = x, ignore.case = TRUE)
} # }
```
