# Loads the OTN tagging metadata sheet to prepare it for use in `convert_otn_to_att`

Loads the OTN tagging metadata sheet to prepare it for use in
`convert_otn_to_att`

## Usage

``` r
prepare_tag_sheet(path, header_line = 5, sheet_name = 2)
```

## Arguments

- path:

  the path to the tagging sheet

- header_line:

  what line the headers are on

- sheet_name:

  the sheet name or number containing the metadata

## Value

a data.frame created from the excel file.

## Details

The function takes the path to the tagging sheet, what line to start
reading the headers from, and what sheet in the excel file to use. It
converts column names to be used by `convert_otn_to_att`.

## Author

Ryan Gosse

## Examples

``` r

#--------------------------------------------------
# EXAMPLE #1 - loading from NSBS tagging

library(glatos)
tag_path <- system.file("extdata", "otn_nsbs_tag_metadata.xls",
  package = "glatos"
)

tags <- prepare_tag_sheet(tag_path, 5, 2)
```
