# Read data from a GLATOS receiver location file

Read data from a standard GLATOS receiver location (csv) file and return
a data.frame of class `glatos_receivers`.

## Usage

``` r
read_glatos_receivers(rec_file, version = NULL)
```

## Arguments

- rec_file:

  A character string with path and name of receiver location file in
  standard GLATOS format (\*.csv). If only file name is given, then the
  file must be located in the working directory. File must be a standard
  GLATOS file (e.g., *GLATOS_receiverLocations_yyyymmdd_xxxxxx.csv*)
  obtained from GLATOSWeb Data Portal <https://glatos.org>.

- version:

  An optional character string with the GLATOS file version number. If
  NULL (default value) then version will be determined by evaluating
  file structure. The only allowed values currently are `NULL` and
  `"1.0"`. Any other values will trigger an error.

## Value

A data.frame of class `glatos_receivers`.

## Details

Data are loaded using
[fread](https://rdrr.io/pkg/data.table/man/fread.html) and timestamps
are coerced to POSIXct using
[fast_strptime](https://lubridate.tidyverse.org/reference/parse_date_time.html).
All timestamps must be 'YYYY-MM-DD HH:MM' format and in UTC timezone per
GLATOS standard.

## Author

C. Holbrook (cholbrook@glfc.org)

## Examples

``` r
# get path to example receiver_locations file
rec_file <- system.file("extdata",
  "sample_receivers.csv",
  package = "glatos"
)

# note that code above is needed to find the example file
# for real glatos data, use something like below
# rec_file <- "c:/path_to_file/GLATOS_receiverLocations_20150321_132242.csv"

rcv <- read_glatos_receivers(rec_file)
```
