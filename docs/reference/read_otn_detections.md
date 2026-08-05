# Read data from a OTN detection file

Read data from a standard OTN detection (csv) file and return a
data.frame of class `glatos_detections`.

## Usage

``` r
read_otn_detections(det_file, format = "new")
```

## Arguments

- det_file:

  A character string with path and name of detection file in OTN
  detection extract format (\*.csv or \*.parquet). If only file name is
  given, then the file must be located in the working directory.

- format:

  Either 'new' or 'old', denoting whether or not the file being loaded
  is a CSV predating OTN's parquet rollout (old) or not (new).

## Value

A data.frame of class `glatos_detections` that includes OTN columns that
do not map directly to GLATOS columns.

## Details

Data are loaded using
[`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html)
package and timestamps are coerced to POSIXct using
[`lubridate::fast_strptime()`](https://lubridate.tidyverse.org/reference/parse_date_time.html).
All times must be in UTC timezone per GLATOS standard.

Column names are changed to match GLATOS standard columns when possible.
Otherwise, OTN columns and column names are retained.

## Author

A. Nunes, <anunes@dal.ca>

## Examples

``` r
# get path to example detection file
det_file <- system.file("extdata", "blue_shark_detections.csv",
  package = "glatos"
)
det <- read_otn_detections(det_file)
```
