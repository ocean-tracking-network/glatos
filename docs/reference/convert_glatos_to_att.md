# Convert detections and receiver metadata to a format that ATT accepts.

Convert `glatos_detections` and `glatos_receiver` objects to `ATT` for
compatibility with the Animal Tracking Toolbox
<https://github.com/vinayudyawer/ATT>, now part of `VTrack`
<https://github.com/RossDwyer/VTrack>.

## Usage

``` r
convert_glatos_to_att(detectionObj, receiverObj, crs = sf::st_crs(4326))
```

## Arguments

- detectionObj:

  A `glatos_detections` object (e.g., created by
  [read_glatos_detections](https://ocean-tracking-network.github.io/glatos/reference/read_glatos_detections.md))
  or a `data.frame` containing required columns (see
  [glatos_detections](https://ocean-tracking-network.github.io/glatos/reference/glatos_detections.md)).

- receiverObj:

  A `glatos_receivers` object (e.g., created by
  [read_glatos_receivers](https://ocean-tracking-network.github.io/glatos/reference/read_glatos_receivers.md))
  or a `data.frame` containing required columns (see
  [glatos_receivers](https://ocean-tracking-network.github.io/glatos/reference/glatos_receivers.md)).

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

This function takes 2 lists containing detection and reciever data and
transforms them into one list containing 3 `tibble` objects. The input
that AAT uses to get this data product is located here:
<https://github.com/vinayudyawer/ATT/blob/master/README.md> and our
mappings are found here:
<https://github.com/ocean-tracking-network/glatos/issues/75#issuecomment-982822886>
in a comment by Ryan Gosse.

Note that the `Tag.Detections` element of the output can contain fewer
records than `detectionObj` because detections are omitted if they do
not match a station deployment-recovery interval in `receiverObj`. For
example, in the walleye data example, `walleye_detections` contains 7180
rows but `Tag.Detectons` output only contains 7083 rows.

## Author

Ryan Gosse

## Examples

``` r

#--------------------------------------------------
# EXAMPLE #1 - loading from the vignette data

library(glatos)
wal_det_file <- system.file("extdata", "walleye_detections.csv",
  package = "glatos"
)
walleye_detections <- read_glatos_detections(wal_det_file) # load walleye data

rec_file <- system.file("extdata", "sample_receivers.csv",
  package = "glatos"
)
rcv <- read_glatos_receivers(rec_file) # load receiver data

ATTdata <- convert_glatos_to_att(walleye_detections, rcv)
```
