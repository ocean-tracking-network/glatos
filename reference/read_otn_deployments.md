# Read data from a OTN deployment file

Read data from a standard OTN deployment (csv) file and return a
data.frame of class `glatos_receivers`.

## Usage

``` r
read_otn_deployments(
  deployment_file,
  deploy_date_col = "deploy_date",
  recovery_date_col = "recovery_date",
  last_download_col = "last_download",
  validate = TRUE
)
```

## Arguments

- deployment_file:

  A character string with path and name of deployment file in OTN
  deployment format (\*.csv). If only file name is given, then the file
  must be located in the working directory.

- deploy_date_col:

  A character string representing the column name containing deploy_date
  data. Defaults to "deploy_date".

- recovery_date_col:

  A character string representing the column name containing
  recovery_date. Defaults to "recovery_date."

- last_download_col:

  A character string representing the column name containing the
  last_download date. Defaults to "last_download."

- validate:

  logical, indicates if column names and classes should be checked
  against requirements. Passed to
  [`as_glatos_receivers()`](https://github.io/reference/glatos_receivers.md).

## Value

A data.frame of class `glatos_receivers` that includes OTN columns that
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
if (FALSE) { # \dontrun{
# get path to example deployments file
deployment_file <- system.file("extdata", "hfx_deployments.csv",
  package = "glatos"
)
dep <- read_otn_deployments(deployment_file)
} # }
```
