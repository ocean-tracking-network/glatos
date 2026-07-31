# Read data from a GLATOS detection file

Read data from a standard GLATOS detection (csv) file and return a
data.frame of class `glatos_detections`.

## Usage

``` r
read_glatos_detections(det_file, version = NULL)
```

## Arguments

- det_file:

  A character string with path and name of detection file in standard
  GLATOS format (\*.csv). If only file name is given, then the file must
  be located in the working directory. File must be a standard GLATOS
  file (e.g., *xxxxx_detectionsWithLocs_yyyymmdd_hhmmss.csv*) submitted
  via GLATOSWeb Data Portal <https://glatos.org>.

- version:

  An optional character string with the glatos file version number. If
  NULL (default value) then version will be determined by evaluating
  file structure. The only allowed values currently are `NULL`, `"1.3"`,
  and `"1.4"`. Any other values will trigger an error. Users should
  never need to set this argument (`NULL` should work).

## Value

A data.frame of class `glatos_detections`.

## Details

Data are loaded using
[fread](https://rdrr.io/pkg/data.table/man/fread.html) and timestamps
are coerced to POSIXct using
[fast_strptime](https://lubridate.tidyverse.org/reference/parse_date_time.html).
All times must be in UTC timezone per GLATOS standard.

Column `animal_id` is considered a required column by many other
functions in this package, so it will be created if any records are
`NULL`. When created, it will be constructed from
`transmitter_codespace` and `transmitter_id`, separated by '-'.

## Author

C. Holbrook <cholbrook@glfc.org>

## Examples

``` r
# get path to example detection file
det_file <- system.file("extdata", "walleye_detections.csv",
  package = "glatos"
)

# note that code above is needed to find the example file
# for real glatos data, use something like below
# det_file <- "c:/path_to_file/HECWL_detectionsWithLocs_20150321_132242.csv"

det <- read_glatos_detections(det_file)
```
