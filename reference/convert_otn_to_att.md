# Convert detections, tagging metadata, and deployment metadata to a format that ATT accepts.

Convert `glatos_detections`, OTN tagging metadata and OTN deployment
metadata to `ATT` format for use in the Animal Tracking Toolbox
<https://github.com/vinayudyawer/ATT>, now part of `VTrack`
<https://github.com/RossDwyer/VTrack>.

## Usage

``` r
convert_otn_to_att(
  detectionObj,
  taggingSheet,
  deploymentObj = NULL,
  deploymentSheet = NULL,
  timeFilter = TRUE,
  crs = sf::st_crs(4326)
)
```

## Arguments

- detectionObj:

  A `glatos_detections` object (e.g., created by
  [read_otn_detections](https://github.io/reference/read_otn_detections.md)
  or
  [read_glatos_detections](https://github.io/reference/read_glatos_detections.md))
  or a `data.frame` containing required columns (see
  [glatos_detections](https://github.io/reference/glatos_detections.md)).

- taggingSheet:

  a data frame from `prepare_tag_sheet`

- deploymentObj:

  a data frame from `read_otn_deployments`

- deploymentSheet:

  a data frame from `prepare_deploy_sheet`

- timeFilter:

  Whether the data should be filtered using the deployment and
  recovery/last download times of receivers. Defaults to TRUE, if not
  all receiver metadata is available, this should be set to FALSE
  otherwise there will be data loss.

- crs:

  an object of class `crs` (see
  [sf::st_crs](https://r-spatial.github.io/sf/reference/st_crs.html))
  with geographic coordinate system for all spatial information
  (latitude/longitude). If none provided or `crs` is not recognized,
  defaults to WGS84.

## Value

a list of 3 tibbles containing tag detections, tag metadata, and station
metadata, to be ingested by VTrack/ATT

## Details

This function takes 3 data frames containing detections, tagging
metadata, and deployment metadata from either `read_otn_deployments` or
`prepare_deploy_sheet` and transforms them into 3 `tibble` objects
inside of a list. The input that AAT uses to get this data product is
located here:
<https://github.com/vinayudyawer/ATT/blob/master/README.md> and our
mappings are found here:
<https://github.com/ocean-tracking-network/glatos/issues/75#issuecomment-982822886>
in a comment by Ryan Gosse.

## Author

Ryan Gosse

## Examples

``` r
if (FALSE) { # \dontrun{
#--------------------------------------------------
# EXAMPLE #1 - loading from Deployment Object

library(glatos)

dets_path <- system.file("extdata", "blue_shark_detections.csv",
  package = "glatos"
)
deploy_path <- system.file("extdata", "hfx_deployments.csv",
  package = "glatos"
)
tag_path <- system.file("extdata", "otn_nsbs_tag_metadata.xls",
  package = "glatos"
)

dets <- read_otn_detections(dets_path)
tags <- prepare_tag_sheet(tag_path, 5, 2)
deploy <- read_otn_deployments(deploy_path)

ATTdata <- convert_otn_to_att(dets, tags, deploymentObj = deploy)

#--------------------------------------------------
# EXAMPLE #2 - loading from Deployment Sheet

library(glatos)

dets_path <- system.file("extdata", "blue_shark_detections_old.csv",
  package = "glatos"
)
deploy_path <- system.file("extdata", "hfx_deploy_simplified.xlsx",
  package = "glatos"
)
tag_path <- system.file("extdata", "otn_nsbs_tag_metadata.xls",
  package = "glatos"
)

dets <- read_otn_detections(dets_path, format = "old")
tags <- prepare_tag_sheet(tag_path, 5, 2)
deploy <- prepare_deploy_sheet(deploy_path, 1, 1)

ATTdata <- convert_otn_to_att(dets, tags, deploymentSheet = deploy)
} # }
```
