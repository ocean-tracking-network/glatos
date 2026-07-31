# A schema for Innovasea Fathom (VDAT) CSV files

A schema for Innovasea Fathom (VDAT) CSV files, produced by 'vdat.exe'
(Fathom Connect) or
[`vdat_convert()`](https://github.io/reference/vdat_convert.md).

## Usage

``` r
vdat_csv_schema
```

## Format

An object of class `list` of length 2.

## Details

A list of lists of data frames that define record types (e.g., `DIAG`,
`DET`, `EVENT_OFFLOAD`), columm names (e.g. `Device Time (UTC)`,
`Time`), and data types (e.g., `character`, `POSIXct`) for
comma-separated-values text file containing data produced by Innovasea's
`vdat.exe` (packaged with Fathom Connect software).

This is used to enforce column names and data types in
[`read_vdat_csv()`](https://github.io/reference/read_vdat_csv.md).

## Author

C. Holbrook
