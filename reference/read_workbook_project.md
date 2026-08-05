# Read Project sheet from GLATOS workbook file

Read Project sheet from GLATOS workbook file

## Usage

``` r
read_workbook_project(wb_file)
```

## Arguments

- wb_file:

  A character string with path and name of workbook in standard GLATOS
  format (.xlsm or .xlsx). If only file name is given, then the file
  must be located in the working directory. See the GLATOSWeb Data
  Portal <https://glatos.org> for file format definitions.

## Value

A list with six elements:

1.  **project_code:** GLATOS Project Code.

2.  **principle_investigator:** Name of Principle Investigator.

3.  **pi_email:** Email address of Principle Investigator.

4.  **source_file:** Name of input `wb_file`.

5.  **wb_version:** Version of GLATOS workbook file.

6.  **created:** Timestamp when source file metadata was changed (from
    `file.info(wb_file)$ctime`).

## Examples

``` r

# Example 1: Version 1.3 (xlsm)

# get path to example GLATOS Data Workbook
wb_file <- system.file("extdata",
  "walleye_workbook.xlsm",
  package = "glatos"
)

read_workbook_project(wb_file)
#> $project_code
#> [1] "HECWL"
#> 
#> $principle_investigator
#> [1] "PI"
#> 
#> $pi_email
#> [1] "thayden@usgs.gov"
#> 
#> $source_file
#> [1] "walleye_workbook.xlsm"
#> 
#> $wb_version
#> [1] "1.3"
#> 
#> $created
#> [1] "2026-08-05 17:21:12 UTC"
#> 


# Example 2: Version 1.4 (xlsx)

wb2_file <- system.file("extdata",
  "walleye_workbook.xlsx",
  package = "glatos"
)

read_workbook_project(wb2_file)
#> $project_code
#> [1] "HECWL"
#> 
#> $principle_investigator
#> [1] "PI"
#> 
#> $pi_email
#> [1] "thayden@usgs.gov"
#> 
#> $source_file
#> [1] "walleye_workbook.xlsx"
#> 
#> $wb_version
#> [1] "1.4"
#> 
#> $created
#> [1] "2026-08-05 17:21:12 UTC"
#> 
```
