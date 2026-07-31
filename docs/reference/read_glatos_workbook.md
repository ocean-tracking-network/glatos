# Read data from a GLATOS project workbook

Read data from a GLATOS project workbook (xlsm or xlsx file).

## Usage

``` r
read_glatos_workbook(
  wb_file,
  read_all = FALSE,
  wb_version = NULL,
  simplify = TRUE
)
```

## Arguments

- wb_file:

  A character string with path and name of workbook in standard GLATOS
  format (.xlsm or .xlsx). If only file name is given, then the file
  must be located in the working directory. See the GLATOSWeb Data
  Portal <https://glatos.org> for file format definitions.

- read_all:

  If TRUE, then all columns and sheets (e.g., user-created
  "project-specific" columns or sheets) in the workbook will be
  imported. If FALSE (default value) then only columns and sheets in the
  standard GLATOS workbook will be imported (project-specific columns
  will be ignored).

- wb_version:

  An optional character string with the workbook version number. If NULL
  (default value) then version will be determined by evaluating workbook
  structure. Currently, the only allowed values are `NULL`, `"1.3"`, and
  `"1.4"`. See Details. Any other values will trigger an error.

- simplify:

  If TRUE (default value), then the returned object is a
  `glatos_workbook` object. If FALSE, then the returned object is a list
  with an element for each sheet in `wb_file`. See Details.

## Value

If `simplify = TRUE`, a list of class `glatos_workbook` with three
elements (described below) containing data from the standard GLATOS
Workbook sheets. If `read_all = TRUE`, then additional elements will be
added with names corresponding to non-standard sheet names.

- metadata:

  A list with data about the project and workbook.

- animals:

  A data frame of class `glatos_animals` with data about tagged animals.

- receivers:

  A data frame of class `glatos_receivers` with data about telemetry
  receivers.

## Details

When `simplify = TRUE`, data in workbook sheets 'Deployment',
'Recovery', and 'Location' are merged on columns 'GLATOS_PROJECT',
'GLATOS_ARRAY', 'STATION_NO', 'CONSECUTIVE_DEPLOY_NO', AND
'INS_SERIAL_NO' to produce the output data frame `receivers`. Data in
workbook sheets 'Project' and 'Tagging' are passed through to new data
frames named 'project' and 'animals', respectively, and data from
workbook sheet 'Proposed' is not included in result. When
`simplify = FALSE`, data in all sheets in the standard workbook are
passed through to new data frames with like names (e.g., 'location',
'deployment', 'recovery').

If `read_all = TRUE` then each sheet not included in the standard
workbook (e.g., not named 'Project', 'Location', 'Deployment',
'Recovery', or 'Tagging') will be included as an element of the returned
list; and in standard workbook sheets, any non-standard columns (i.e,
'project-specific fields') will be included in the result. Names of
non-standard columns may be changed (e.g., for uniqueness), with
warnings.

Data are read from the input file using
[read_excel](https://readxl.tidyverse.org/reference/read_excel.html) in
the 'readxl' package. If `read_all = TRUE` then the type of data in each
user-defined column (and sheet) will be 'guessed' by
[read_excel](https://readxl.tidyverse.org/reference/read_excel.html).
Therefore, if `read_all = TRUE` then the structure of those columns
should be carefully reviewed in the result. See
[read_excel](https://readxl.tidyverse.org/reference/read_excel.html) for
details.

Column `animal_id` is considered a required column by many other
functions in this package, so it will be created if any records are
`NULL`. When created, it will be constructed from `tag_code_space` and
`tag_id_code`, separated by '-'.

Timezone attribute of all timestamp columns (class `POSIXct`) in output
will be "UTC" and all 'glatos-specific' timestamp and timezone columns
will be omitted from result.

As of glatos 0.9.0, if a sheet contains two columns with the same name,
then the sheet is not loaded and an error is returned. In earlier
versions, a suffix was added to all but the first in each set of
duplicate column names.

As of glatos 0.9.8, time zones (e.g., columns named "GLATOS_TIMEZONE")
are checked against
[`OlsonNames()`](https://rdrr.io/r/base/timezones.html) after prepending
"US/". E.g., "Eastern" becomes "US/Eastern". Matching is not case
sensitive, so "EASTERN" is valid, but is replaced by "US/Eastern", with
a warning. Invalid time zones will result in `NA` timestamp values, with
a warning.

## Note

***On warnings and errors about date and timestamp formats.*** Date and
time columns are sometimes stored as text in Excel. When those records
are loaded by this function, there are two possible outcomes.\
\
1. If the records are formatted according to the GLATOS Data Dictionary
specification (e.g., "YYYY-MM-DD" for dates and "YYYY-MM-DD HH:MM" for
timestamps; see [https:\\glatos.org](https:\\glatos.org)) those records
should be properly loaded into R, but the user is encouraged to verify
that they were loaded correctly, so a warning points the user to those
records in the workbook. Users may want to format as custom date in the
workbook to avoid warnings in the future.\
\
2. If the format of a date-as-text column is not consistent with GLATOS
specification, then no data will be loaded and an error will alert the
user to this condition. Similarly, if a date or date-time column is
stored as a number in Excel, then no data will be loaded and an error
will alert the user to this condition.\
\
***On cells with locked formatting in Excel:*** Occasionally the format
of a cell in Excel will be locked. In those cases, it is sometimes
possible to force date formatting in Excel by (1) highlighting the
columns that need reformatting, (2) select 'Text-to-columns' in the
'Data' menu, (3) select 'Delimited' and 'next', (4) uncheck all
delimiters and 'next', (5) choose 'Date: YMD' in the 'Column data
format' box, and (6) 'Finish'.

## See also

[read_excel](https://readxl.tidyverse.org/reference/read_excel.html)

## Author

C. Holbrook <cholbrook@glfc.org>

## Examples

``` r

# Example 1: Version 1.3 (xlsm)

# get path to example GLATOS Data Workbook
wb_file <- system.file("extdata",
  "walleye_workbook.xlsm",
  package = "glatos"
)

# note that code above is needed to find the example file
# for real glatos data, use something like below
# wb_file <- "c:/path_to_file/HECWL_GLATOS_20150321.xlsm"

wb <- read_glatos_workbook(wb_file)

wba <- read_glatos_workbook(wb_file, read_all = TRUE)

wbr <- read_glatos_workbook(wb_file, simplify = FALSE)


# Example 2: Version 1.4 (xlsx)

wb2_file <- system.file("extdata",
  "walleye_workbook.xlsx",
  package = "glatos"
)

wb2 <- read_glatos_workbook(wb2_file)

wb2a <- read_glatos_workbook(wb2_file, read_all = TRUE)

wbr2 <- read_glatos_workbook(wb2_file, simplify = FALSE)
```
