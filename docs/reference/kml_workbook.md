# Make a KML or KMZ file of receiver and animal release locations

Convert standard GLATOS receiver location and animal release data to a
KML (or optionally KMZ) file (e.g., for viewing in Google Earth). (NOTE:
EARLY DEVELOPMENT VERSION).

## Usage

``` r
kml_workbook(
  wb = NULL,
  wb_file = NULL,
  receiver_locs = NULL,
  animals = NULL,
  kmz = FALSE,
  show_ongoing_recs = TRUE,
  end_date = NULL,
  out_file = NULL,
  wb_version = NULL,
  ...
)
```

## Arguments

- wb:

  A `glatos_workbook` object created by
  [read_glatos_workbook](https://ocean-tracking-network.github.io/glatos/reference/read_glatos_workbook.md).

- wb_file:

  A character string with path and name of workbook in standard GLATOS
  format (\*.xlsm). If only file name is given, then the file must be
  located in the working directory. File must be a standard GLATOS file
  (e.g., *xxxxx_GLATOS_YYYYMMDD.xlsm*) submitted via GLATOSWeb Data
  Portal <https://glatos.org>.

- receiver_locs:

  **not yet implemented**

- animals:

  **not yet implemented**

- kmz:

  logical; If TRUE, a KMZ file (zipped KML file) will be created.
  Default value is FALSE.

- show_ongoing_recs:

  Indicates if ongoing stations (missing recovery timestamp) should be
  included in result.

- end_date:

  End date (e.g. "YYYY-MM-DD") to be used for any ongoing stations (if
  showOngoing == T). Defaults to current system time.

- out_file:

  File name (path optional) of output file. If path not specified then
  file will be written to working directory. Extension is not checked
  against `kmz`. Required if `wb_file` is NULL. If not specified and
  `wb_file` is given, then file will be written to file with name
  matching `wb_file`.

- wb_version:

  An optional character string with the workbook version number. Passed
  to
  [read_glatos_workbook](https://ocean-tracking-network.github.io/glatos/reference/read_glatos_workbook.md)
  when input is `wb_file`.

- ...:

  optional arguments that influence kml/kmz features. Curently only two
  options:

  `labelSize`

  : A numeric scalar with the size of placemark labels (only shown when
    placemark is highlighted by user).

  `iconSize`

  : A numeric scalar with the size of placemark icons.

## Value

A KML (and optionally, KMZ) file, written to the directory that contains
the input GLATOS workbook, or `out_file` otherwise. Path to output file
is returned.

## Details

Receiver locations will be visible between deployment and recovery
timestamps at each location. Release locations will be displayed when
the display window includes the date of release.

## Author

C. Holbrook <cholbrook@glfc.org>

## Examples

``` r
if (FALSE) { # \dontrun{
# get path to example GLATOS Data Workbook
wb_file <- system.file("extdata",
  "walleye_workbook.xlsm",
  package = "glatos"
)

# read workbook directly
kml_workbook(wb_file = wb_file)

# now with bigger label and point and out_file
kml_workbook(
  wb_file = wb_file, labelSize = 20, iconSize = 1,
  out_file = "bigger.kml"
)

# read workbook directly; output kmz
kml_workbook(wb_file = wb_file, kmz = TRUE)

# get path to example GLATOS Data Workbook
wb <- read_glatos_workbook(wb_file)
kml_workbook(wb = wb, kmz = TRUE, out_file = "bigger.kmz")
} # }
```
