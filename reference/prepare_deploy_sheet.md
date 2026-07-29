# Loads the OTN receiver deployment metadata sheet to prepare it for use in `convert_otn_to_att`

Loads the OTN receiver deployment metadata sheet to prepare it for use
in `convert_otn_to_att`

## Usage

``` r
prepare_deploy_sheet(
  path,
  header_line = 5,
  sheet_name = 1,
  combine_arr_stn = TRUE
)
```

## Arguments

- path:

  the path to the deployment sheet

- header_line:

  what line the headers are on

- sheet_name:

  the sheet name or number containing the metadata

- combine_arr_stn:

  whether or not to to join the station and array columns. Format
  depends on OTN node

## Value

a data.frame created from the excel file.

## Details

The function takes the path to the deployment sheet, what line to start
reading from, and what sheet in the excel file to use. It converts
column names to be used by `convert_otn_to_att`.

## Author

Ryan Gosse

## Examples

``` r

#--------------------------------------------------
# EXAMPLE #1 - loading from NSBS simplified Deployments

library(glatos)
deploy_path <- system.file("extdata", "hfx_deploy_simplified.xlsx",
  package = "glatos"
)

deploy <- prepare_deploy_sheet(deploy_path,
  header_line = 1,
  sheet_name = 1
)
```
