# KML To CSV Conversion

Function for extracting features (points, lines, polygons) from kml
files and writing them to csv files.

## Usage

``` r
kml_to_csv(filePath, type = c("points", "lines", "polygons"))
```

## Arguments

- filePath:

  The pathname for the kml file you wish to convert.

- type:

  Optional character string indicating the type(s) of feature(s) to read
  from the kml file. Valid values are
  `c("points", "lines", and "polygons")`.

## Value

A csv file (same name as input `filePath` but with `csv` extension) is
written to directory containing input `filePath` with five columns

- name:

  Feature name

- feature_type:

  Feature type

- seq:

  Sequential position in feature

- longitude:

  Longitude

- latitude:

  Latitude

- altitude:

  Altitude

## Details

kmz files are not supported. Make sure exports from Google earth are
saved as kml. Or extract (unzip) kml from kmz.

## Examples

``` r

# Get example kml with two polygons
kml_file <- system.file("extdata", "example_polygons.kml",
  package = "glatos"
)

kml_to_csv(kml_file)
#> [1] "/tmp/RtmpUoCFFC/temp_libpath5cdd282ffe47/glatos/extdata/example_polygons.csv"
```
