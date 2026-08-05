# Read detection data exported from Innovasea VUE software

Read detection data exported from Innovasea VUE software

## Usage

``` r
read_vue_detection_csv(src, show_progress = FALSE)
```

## Arguments

- src:

  A character string with path and name of a CSV file produced
  containing detection data exported from Innovasea VUE software. If
  only file name is given, then the file must be located in the working
  directory.

- show_progress:

  Optional argument passed to
  [`data.table::fread`](https://rdrr.io/pkg/data.table/man/fread.html)'s
  `showProgress`.

## Value

A data.frame of class `vue_detections`.

## Details

Reading is done via
[`fread`](https://rdrr.io/pkg/data.table/man/fread.html).

All timestamp columns are assumed to be in UTC.

## Author

C. Holbrook (cholbrook@glfc.org)

## Examples

``` r
csv_file <- system.file("extdata",
  "VR2W_109924_20110718_1.csv",
  package = "glatos"
)

vue_det <- read_vue_detection_csv(csv_file)
```
