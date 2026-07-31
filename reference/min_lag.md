# Calculate 'min_lag' for identifying potential false positive detections

Calculate minimum time interval (min_lag) between successive detections
and add to detection data set for identifying potential false
detections.

## Usage

``` r
min_lag(det)
```

## Arguments

- det:

  A `glatos_detections` object (e.g., produced by
  [read_glatos_detections](https://github.io/reference/read_glatos_detections.md)).

  *OR* a data frame containing detection data with the following
  columns:

  detection_timestamp_utc

  :   Detection timestamps; MUST be of class POSIXct.

  transmitter_codespace

  :   A character string with transmitter code space (e.g., "A69-1061"
      for Vemco PPM coding").

  transmitter_id

  :   A character string with transmitter ID code (e.g., "1363" for
      Vemco PPM coding").

  receiver_sn

  :   A character vector with unique receiver serial number.

## Value

A column `min_lag` (defined above) is added to input object.

## Details

`min_lag` is loosely based on the the "short interval" described by
Pincock (2012) and replicates the `min_lag` column in the standard
glatos detection export file. In this case (GLATOS), `min_lag` is
defined for each detection as the shortest interval (in seconds) between
either the previous or next detection (whichever is closest) of the same
transmitter code (defined here as combination of transmitter_codespace
and transmitter_id) on the same receiver.

A new column (`min_lag`) is added to the input dataframe that represents
the time (in seconds) between the current detection and the next
detection (either before or after) of the same transmitter on the same
receiver. This function replicates the 'min_lag' column included in the
standard glatos export. Data is sorted internally to calculate `min_lag`
but output is returned in the original (input) order

## References

Pincock, D.G., 2012. False detections: what they are and how to remove
them from detection data. Vemco Division, Amirix Systems Inc., Halifax,
Nova Scotia.  
<http://www.vemco.com/pdf/false_detections.pdf>

## See also

[`false_detections()`](https://github.io/reference/false_detections.md)

## Author

Chris Holbrook, Todd Hayden, Angela Dini

## Examples

``` r

# load example detection file
det_file <- system.file("extdata", "walleye_detections.csv",
  package = "glatos"
)
det <- read_glatos_detections(det_file)

# rename existing min_lag column
colnames(det)[colnames(det) == "min_lag"] <- "min_lag.x"

# calculate min_lag
det <- min_lag(det)

head(det)
#>   animal_id detection_timestamp_utc glatos_array station_no
#> 1       153     2012-04-29 01:48:37          TTB          2
#> 2       153     2012-04-29 01:52:55          TTB          2
#> 3       153     2012-04-29 01:55:12          TTB          2
#> 4       153     2012-04-29 01:56:42          TTB          2
#> 5       153     2012-04-29 01:58:37          TTB          2
#> 6       153     2012-04-29 02:01:22          TTB          2
#>   transmitter_codespace transmitter_id sensor_value sensor_unit deploy_lat
#> 1              A69-9001          32054           NA        <NA>   43.39165
#> 2              A69-9001          32054           NA        <NA>   43.39165
#> 3              A69-9001          32054           NA        <NA>   43.39165
#> 4              A69-9001          32054           NA        <NA>   43.39165
#> 5              A69-9001          32054           NA        <NA>   43.39165
#> 6              A69-9001          32054           NA        <NA>   43.39165
#>   deploy_long receiver_sn tag_type tag_model tag_serial_number common_name_e
#> 1   -83.99264      113213     <NA>      <NA>              <NA>       walleye
#> 2   -83.99264      113213     <NA>      <NA>              <NA>       walleye
#> 3   -83.99264      113213     <NA>      <NA>              <NA>       walleye
#> 4   -83.99264      113213     <NA>      <NA>              <NA>       walleye
#> 5   -83.99264      113213     <NA>      <NA>              <NA>       walleye
#> 6   -83.99264      113213     <NA>      <NA>              <NA>       walleye
#>      capture_location length weight sex release_group release_location
#> 1 Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
#> 2 Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
#> 3 Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
#> 4 Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
#> 5 Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
#> 6 Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
#>   release_latitude release_longitude utc_release_date_time
#> 1               NA                NA   2012-03-20 20:00:00
#> 2               NA                NA   2012-03-20 20:00:00
#> 3               NA                NA   2012-03-20 20:00:00
#> 4               NA                NA   2012-03-20 20:00:00
#> 5               NA                NA   2012-03-20 20:00:00
#> 6               NA                NA   2012-03-20 20:00:00
#>   glatos_project_transmitter glatos_project_receiver glatos_tag_recovered
#> 1                      HECWL                   HECWL                   NO
#> 2                      HECWL                   HECWL                   NO
#> 3                      HECWL                   HECWL                   NO
#> 4                      HECWL                   HECWL                   NO
#> 5                      HECWL                   HECWL                   NO
#> 6                      HECWL                   HECWL                   NO
#>   glatos_caught_date station min_lag.x min_lag
#> 1               <NA> TTB-002       258     258
#> 2               <NA> TTB-002       137     137
#> 3               <NA> TTB-002        90      90
#> 4               <NA> TTB-002        90      90
#> 5               <NA> TTB-002       115     115
#> 6               <NA> TTB-002       145     145
```
