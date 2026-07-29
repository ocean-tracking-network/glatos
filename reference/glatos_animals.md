# Construct, check, and validate a glatos_animals object

Creates, checks, or validates a glatos_animals object.

## Usage

``` r
glatos_animals(..., validate = TRUE)

as_glatos_animals(x, validate = TRUE)

is_glatos_animals(x)

validate_glatos_animals(x)
```

## Arguments

- ...:

  Named vectors, minimally one for each required column of the specified
  class:

  `animal_id`

  :   must be character, uniquely identifies each animal.

  `tag_id_code`

  :   must be character, identification code transmitted by the tag
      (e.g., "1363" for Innovasea PPM coding").

  `tag_code_space`

  :   must be character, code space transmitted by the tag (e.g.,
      "A69-9002").

  `utc_release_date_time`

  :   must be POSIXct, timestamp (in UTC) when animal was released
      (i.e., start of telemetry sampling interval.)

- validate:

  logical, indicates if column names and classes should be checked
  against requirements.

- x:

  A data.frame or object that inherits from data.frame (e.g.,
  data.table, tibble) and contains all required columns (see `...`).

## Construction

`glatos_animals()` creates a `glatos_animals` from individual vectors
(one for each column) and optionally checks for required column names
and classes using `validate_glatos_animals()`.

## Coercion

`as_glatos_animals()` coerces a data.frame, or object that inherits from
data.frame, to `glatos_animals` and optionally checks for required
column names and classes using `validate_glatos_animals()`.

## Validation

`is_glatos_animals()` checks class attribute for `"glatos_animals"`

`validate_glatos_animals()` checks for required column names and classes

## Examples

``` r

#  glatos_animals
x <- data.frame(
  animal_id = c("120", "107", "109"),
  tag_id_code = c("32024", "32012", "32014"),
  tag_code_space = c("A69-9001", "A69-9001", "A69-9001"),
  utc_release_date_time = as.POSIXct(
    c(
      "2011-03-28 00:00:00",
      "2011-03-28 00:01:00",
      "2011-03-28 00:05:00"
    ),
    tz = "UTC"
  ),
  release_latitude = c(41.56093, 41.56093, 41.56093),
  release_longitude = c(-83.645, -83.645, -83.645)
)

ga_df1 <- glatos_animals(
  animal_id = x$animal_id,
  tag_id_code = x$tag_id_code,
  tag_code_space = x$tag_code_space,
  utc_release_date_time = x$utc_release_date_time
)


# as_glatos_animals
ga_df2 <- as_glatos_animals(x)


# sf input

library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE

x_sf <- sf::st_as_sf(x,
  coords = c("release_longitude", "release_latitude")
)

ga_sf <- as_glatos_animals(x_sf)


# tibble input

x_tbl <- dplyr::as_tibble(x)

ga_tbl <- as_glatos_animals(x_tbl)


# All below will error as invalid

# data.frame input; missing column name
library(dplyr) # for rename
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
x2 <- rename(x,
  fish_name = animal_id,
  release_timestamp = utc_release_date_time
)

try(
  ga2 <- as_glatos_animals(x2)
)
#> Error : Required column(s) missing from input x:
#>  animal_id
#>  utc_release_date_time

# data.frame input; wrong column class
x3 <- mutate(x,
  animal_id = as.integer(animal_id),
  utc_release_date_time = as.character(utc_release_date_time)
)

try(
  ga3 <- as_glatos_animals(x3)
)
#> Error : The following column(s) have wrong class: 
#>  animal_id (must be 'character')
#>  utc_release_date_time (must be 'POSIXct')

# Validation and checking

validate_glatos_animals(x)
#> [1] TRUE

is_glatos_animals(x) # FALSE
#> [1] FALSE

is_glatos_animals(ga_df1) # TRUE
#> [1] TRUE
```
