# Convert detections, transmitter, receiver, and animal metadata to a format that ATT accepts.

Convert `glatos_detections` and transmitter, receiver, and animal
metadata from the OTN ERDDAP to `ATT` format for use in the Animal
Tracking Toolbox <https://github.com/vinayudyawer/ATT>, now part of
`VTrack` <https://github.com/RossDwyer/VTrack>.

## Usage

``` r
convert_otn_erddap_to_att(
  detectionObj,
  erdTags,
  erdRcv,
  erdAni,
  crs = sf::st_crs(4326)
)
```

## Arguments

- detectionObj:

  A `glatos_detections` object (e.g., created by
  [read_otn_detections](https://ocean-tracking-network.github.io/glatos/reference/read_otn_detections.md)
  or
  [read_glatos_detections](https://ocean-tracking-network.github.io/glatos/reference/read_glatos_detections.md))
  or a `data.frame` containing required columns (see
  [glatos_detections](https://ocean-tracking-network.github.io/glatos/reference/glatos_detections.md)).

- erdTags:

  a data frame with tag release data from the OTN ERDDAP

- erdRcv:

  a data frame with receiver station data from the OTN ERDDAP

- erdAni:

  a data frame with animal data from the OTN ERDDAP

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

This function takes 4 data frames containing detection, and ERDDAP data
from the tags, receivers, and animals tables, and transforms them into 3
`tibble` objects inside of a list. The input that AAT uses to get this
data product is located here:
<https://github.com/vinayudyawer/ATT/blob/master/README.md> and our
mappings are found here:
<https://github.com/ocean-tracking-network/glatos/issues/75#issuecomment-982822886>
in a comment by Ryan Gosse. The OTN ERDDAP instance is here:
<https://members.oceantrack.org/erddap/tabledap/index.html> but please
note that this only contains public data.

## Author

Ryan Gosse

## Examples

``` r

#--------------------------------------------------
# EXAMPLE #1 - loading from the OTN ERDDAP + vignettes

library(glatos)

# get path to example files from OTN ERDDAP
ani_erd_file <- system.file("extdata", "otn_aat_animals.csv",
  package = "glatos"
)
animals <- read.csv(ani_erd_file) # load the CSVs from ERDDAP

tags_erd_file <- system.file("extdata", "otn_aat_tag_releases.csv",
  package = "glatos"
)
tags <- read.csv(tags_erd_file)

rcv_erd_file <- system.file("extdata", "otn_aat_receivers.csv",
  package = "glatos"
)
stations <- read.csv(rcv_erd_file)

# Remove first row; (blank or metadata about the column)
animals <- animals[-1, ]
tags <- tags[-1, ]
stations <- stations[-1, ]

# get blue shark example data
shrk_det_file <- system.file("extdata", "blue_shark_detections.csv",
  package = "glatos"
)
blue_shark_detections <- read_otn_detections(shrk_det_file) # load shark data

ATTdata <- convert_otn_erddap_to_att(
  detectionObj = blue_shark_detections,
  erdTags = tags,
  erdRcv = stations,
  erdAni = animals
)
```
