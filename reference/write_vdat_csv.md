# Write a vdat_list object to disk in Innovasea Fathom VDAT CSV format

Write a vdat_list object to disk in Innovasea Fathom VDAT CSV format

## Usage

``` r
write_vdat_csv(
  vdat,
  record_types = NULL,
  out_file = NULL,
  output_format = "csv.fathom",
  include_empty = FALSE,
  export_settings = NULL
)
```

## Arguments

- vdat:

  A `vdat_list` object; e.g., produced by
  [`read_vdat_csv`](https://github.io/reference/read_vdat_csv.md)..

- record_types:

  An optional vector of character strings with names of record types to
  include in output. E.g., "DET" for detection records. Default (`NULL`)
  will write all record types present in input CSV `vdat`.

- out_file:

  Character string with name of CSV file to be written. If `NULL`
  (default), or if `out_file` only contains a path, then file name will
  be derived from the data source file name using
  `` tail(vdat$DATA_SOURCE_FILE$`File Name`, 1) ``.

- output_format:

  Character string with output format. Options are: `"csv.fathom"`
  (default) writes a single CSV file (for each input file) with multiple
  record types interleaved; `"csv.fathom.split"` writes a folder (for
  each input file) containing a separate CSV for each record type.

- include_empty:

  Logical (default = `FALSE`). If `output_format = "csv.fathom.split"`,
  should files be written for empty objects.

- export_settings:

  (NOT YET IMPLEMENTED). Placeholder for future specification of other
  options available via Fathom Data Export app. (E.g., 'Data Types to
  Include', 'Data Filter', 'Filename Suffix', 'Time Offset in Hours',
  'Split CSV by UTC Day'.)

## Value

A character string with full path and file name to output file.

## Details

Writing is done via
[`fwrite`](https://rdrr.io/pkg/data.table/man/fwrite.html).

## Author

C. Holbrook (cholbrook@glfc.org)

## Examples

``` r
if (FALSE) { # \dontrun{

# Example 1. Read and write a single file

vrl_file <- system.file("extdata", "detection_files_raw",
  "VR2W_109924_20110718_1.vrl",
  package = "glatos"
)

temp_dir <- tempdir()

csv_file <- vdat_convert(vrl_file, out_dir = temp_dir)

# utils::browseURL(temp_dir)

# read all record types
vdat <- read_vdat_csv(csv_file)

# write to single file (output_format = "csv.fathom")
temp_file <- tempfile(fileext = ".csv")
write_vdat_csv(vdat, out_file = temp_file)

# write to multiple files (fathom split option)
temp_dir2 <- tempdir()
write_vdat_csv(vdat,
  out_file = temp_dir2,
  output_format = "csv.fathom.split"
)
} # }
```
