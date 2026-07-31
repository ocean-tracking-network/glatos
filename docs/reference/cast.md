# Cast a list of scalars to a new class

Cast a list of scalars, with potentially mixed classes, to a new class.

## Usage

``` r
cast(
  x,
  new_class,
  old_class = c("logical", "character", "numeric", "Date", "POSIXct"),
  defer_exceptions = TRUE,
  ...
)
```

## Arguments

- x:

  A list of scalars.

- new_class:

  A text string with name of new class.

- old_class:

  A character vector with names of classes in `x` that will be cast to
  `new_class`. Any record with a different class will result in NA (with
  error). Default value is
  `c("logical", "numeric", "character", "Date", "POSIXct")`.

- defer_exceptions:

  If TRUE (default value) then errors and warnings will be returned as
  attributes with prefix "error\_" or "warning\_".

- ...:

  Other arguments passed to the casting function (e.g.,
  `tz = "US/Eastern"` when `new_class` is `POSIXct`).

## Value

A vector of length same as `x` and class as `new_class`.

## Details

Written specifically for
[`readxl::read_excel`](https://readxl.tidyverse.org/reference/read_excel.html)
with `col_types = "list"` to evaluate class of each record/row
independently and then present user with a single report of all errors
(instead of sequential one. at. a. time).

## Examples

``` r

x <- list(TRUE, "A", NA, 3.1415, Sys.time(), Sys.Date(), "1997-05-13 12:43:21")

sapply(x, class)
#> [[1]]
#> [1] "logical"
#> 
#> [[2]]
#> [1] "character"
#> 
#> [[3]]
#> [1] "logical"
#> 
#> [[4]]
#> [1] "numeric"
#> 
#> [[5]]
#> [1] "POSIXct" "POSIXt" 
#> 
#> [[6]]
#> [1] "Date"
#> 
#> [[7]]
#> [1] "character"
#> 

cast(x, "character")
#> [1] "TRUE"                       "A"                         
#> [3] NA                           "3.1415"                    
#> [5] "2026-07-29 09:40:32.263729" "2026-07-29"                
#> [7] "1997-05-13 12:43:21"       
#> attr(,"warning_cast_to_check")
#> [1] "rows 1, 4, 5, 6"

cast(x, "numeric")
#> [1] 1.000000e+00           NA           NA 3.141500e+00 1.785332e+09
#> [6] 2.066300e+04           NA
#> attr(,"error_cast_failed")
#> [1] "rows 2, 7"
#> attr(,"warning_cast_to_check")
#> [1] "rows 1, 5, 6"

cast(x, "Date")
#> [1] NA           NA           NA           "1970-01-04" "2026-07-29"
#> [6] "2026-07-29" "1997-05-13"

cast(x, "POSIXct")
#> [1] NA                        NA                       
#> [3] NA                        "1969-12-31 19:00:03 EST"
#> [5] "2026-07-29 09:40:32 EDT" "2026-07-28 20:00:00 EDT"
#> [7] "1997-05-13 12:43:21 EDT"

cast(x, "POSIXct", tz = "US/Pacific")
#> [1] NA                        NA                       
#> [3] NA                        "1969-12-31 19:00:03 EST"
#> [5] "2026-07-29 09:40:32 EDT" "2026-07-28 20:00:00 EDT"
#> [7] "1997-05-13 08:43:21 EDT"

# separate tz for each element
cast(x, "POSIXct", tz = c("US/Eastern", rep("US/Pacific", 5)))
#> [1] NA                        NA                       
#> [3] NA                        "1969-12-31 19:00:03 EST"
#> [5] "2026-07-29 09:40:32 EDT" "2026-07-28 20:00:00 EDT"
#> [7] "1997-05-13 08:43:21 EDT"

# Only cast from if class is character
cast(x, "POSIXct", old_class = "character")
#> [1] NA                        NA                       
#> [3] NA                        NA                       
#> [5] NA                        NA                       
#> [7] "1997-05-13 12:43:21 EDT"

# Only cast from if class is character or POSIXct
cast(x, "character", old_class = c("character", "POSIXct"))
#> [1] NA                           "A"                         
#> [3] NA                           NA                          
#> [5] "2026-07-29 09:40:32.263729" NA                          
#> [7] "1997-05-13 12:43:21"       
#> attr(,"error_input_class_skipped")
#> [1] "rows 1, 4, 6"
#> attr(,"warning_cast_to_check")
#> [1] "row 5"

if (FALSE) { # \dontrun{

# Bad (unsupported) new_class
cast(x, "foo")

# Bad (unsupported) old_class
cast(x, "character", old_class = c("character", "foo"))
} # }
```
