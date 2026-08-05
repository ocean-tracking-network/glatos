# Identify and check GLATOS workbook file version

Identify and check version of a GLATOS workbook file (xlsm or xlsx)
based on its structure.

## Usage

``` r
identify_workbook_version(wb_file, wb_version = NULL)
```

## Arguments

- wb_file:

  A character string with path and name of workbook in standard GLATOS
  format (.xlsm or .xlsx). If only file name is given, then the file
  must be located in the working directory. See the GLATOSWeb Data
  Portal <https://glatos.org> for file format definitions.

- wb_version:

  An optional character string with the workbook version number. If NULL
  (default value) then version will be determined by evaluating workbook
  structure. Currently, the only allowed values are `NULL`, `"1.3"`, and
  `"1.4"`. See Details. Any other values will trigger an error.

## Value

A character string with version number ("1.3", "1.4").

## Examples

``` r

# Example 1: Version 1.3 (xlsm)

# get path to example GLATOS Data Workbook
wb_file <- system.file("extdata",
  "walleye_workbook.xlsm",
  package = "glatos"
)

identify_workbook_version(wb_file)
#> [1] "1.3"


# Example 2: Version 1.4 (xlsx)

wb2_file <- system.file("extdata",
  "walleye_workbook.xlsx",
  package = "glatos"
)

identify_workbook_version(wb2_file)
#> [1] "1.4"
```
