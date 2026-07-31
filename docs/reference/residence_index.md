# Generate the residence index from a set of detections

This residence index tool will take condensed detection event data (from
[`detection_events()`](https://ocean-tracking-network.github.io/glatos/reference/detection_events.md)
and caculate the residence index for each location. The information
passed to the function is what is used to calculate the residence index,
make sure you are only passing the data you want taken into
consideration for the residence index (i.e. species, stations, tags,
etc.).

## Usage

``` r
residence_index(
  detections,
  calculation_method = "kessel",
  locations = NULL,
  group_col = "animal_id",
  time_interval_size = "1 day",
  groupwise_total = TRUE
)
```

## Arguments

- detections:

  A data.frame from the
  [`detection_events()`](https://ocean-tracking-network.github.io/glatos/reference/detection_events.md)
  function.

- calculation_method:

  A character string with the calculation method using one of the
  following: `kessel`, `time_interval`, `timedelta`,
  `aggregate_with_overlap`, or `aggregate_no_overlap`.

- locations:

  An optional data frame that identifies all unique locations where RI
  will be calculated. Three columns required:

  location

  : Character string with unique location identifier.

  mean_longitude

  : Location longitude (for mapping).

  mean_latitude

  : Location latitude (for mapping).

  If `locations = NULL` (default value) then RI will only be calculated
  at locations present in `detections$location`.

- group_col:

  Optional character string (can be multiple) that identifies additional
  grouping variables for RI calculations. The default value
  (`group_col = "animal_id"`) will calculate and return RI for each
  animal at each location (i.e., for each unique combination of
  `location` and `animal_id`. If `group_col = NULL` then RI will be
  calculated by location only (will not account for animal or any other
  variable).

- time_interval_size:

  Character string with size of the time interval used when
  `calculation_method = "time_interval"`. This is passed to
  [seq.Date](https://rdrr.io/r/base/seq.Date.html)'s `by` argument, so
  must meet the requirements of that argument for that function (e.g.,
  "1 day", "4 hours", etc.). Default is `"1 day"`.

- groupwise_total:

  Logical that determines how the denominator is calculated in RI. If
  FALSE (default) then the denominator represents the total number of
  time intervals or time (depending on calculation method) among all
  records. Otherwise (if FALSE), the denominator represents the total
  number of time intervals or time within each group level (e.g., for
  each animal if `group_col = "animal_id"`.

## Value

A data.frame of days_detected, residency_index, location, mean_latitude,
mean_longitude

## Details

The **kessel** method converts both the `first_detection` and
`last_detection` columns into a date with no hours, minutes, or seconds.
Next it creates a list of the unique days where a detection was seen.
The size of the list is returned as the total number of days as an
integer. This calculation is used to determine the total number of
distinct days (T) and the total number of distinct days per location
(S). Possible rounding error may occur as a detection on 2016-01-01
23:59:59 and a detection on 2016-01-02 00:00:01 would be counted as two
days when it is really 2-3 seconds.

\$\$ RI = S/T\$\$ \$\$ RI = Residence Index\$\$ \$\$S = Distinct number
of days detected at the location\$\$ \$\$T = Distinct number of days
detected at any location\$\$

The **time_interval** calculation method determines the number of time
intervals (size determined by `time_interval_size` argument) in which
detections occurred at each `location` and as a fraction of the number
of time intervals in which detections occurred among all sites. For each
location, residency index (RI) is calculated:

\$\$ RI = L/T\$\$ \$\$ RI = Residence Index\$\$ \$\$L = Distinct number
of time intervals in which detection observed at this location\$\$ \$\$T
= Distinct number of time intervals in which detection observed at any
location\$\$

For consistency with other `calculation_method`s, the L and T are not
reported, but are converted cumulative time covered in days and reported
in columns `days_detected` and `total_days`.

The **timedelta** calculation method determines the first detection and
the last detection of all detections. The time difference is then taken
as the values to be used in calculating the residence index. The
timedelta for each station is divided by the timedelta of the array to
determine the residence index.

\$\$ RI = Delta S/Delta T\$\$

\$\$RI = Residence Index\$\$

\$\$Delta S = Last detection time at the location - First detection time
at the location\$\$

\$\$Delta T = Last detection time at any location - First detection time
at any location\$\$

The **aggregate_with_overlap** calculation method takes the length of
time of each detection and sums them together. A total is returned. The
sum for each location is then divided by the sum among all locations to
determine the residence index.

\$\$ RI = AwOS/AwOT\$\$

\$\$RI = Residence Index\$\$

\$\$AwOS = Sum of length of time of each detection at the location\$\$

\$\$AwOT = Sum of length of time of each detection among all
locations\$\$

The **aggregate_no_overlap** calculation method takes the length of time
of each detection and sums them together. However, any overlap in time
between one or more detections is excluded from the sum. For example, if
the first detection is from `2016-01-01 01:02:43` to
`2016-01-01 01:10:12` and the second detection is from
`2016-01-01 01:09:01 `to `2016-01-01 01:12:43`, then the sum of those
two detections would be 10 minutes. A total is returned once all
detections of been added without overlap. The sum for each location is
then divided by the sum among all locations to determine the residence
index.

\$\$ RI = AnOS/AnOT\$\$

\$\$RI = Residence Index\$\$

\$\$AnOS = Sum of length of time of each detection at the location,
excluding any overlap\$\$

\$\$AnOT = Sum of length of time of each detection among all locations,
excluding any overlap\$\$

## References

Kessel, S.T., Hussey, N.E., Crawford, R.E., Yurkowski, D.J., O'Neill,
C.V. and Fisk, A.T., 2016. Distinct patterns of Arctic cod (*Boreogadus
saida*) presence and absence in a shallow high Arctic embayment,
revealed across open-water and ice-covered periods through acoustic
telemetry. Polar Biology, 39(6), pp.1057-1068.
<https://www.researchgate.net/publication/279269147>

## Author

A. Nunes, <anunes@dal.ca>

## Examples

``` r
# get path to example detection file
det_file <- system.file("extdata", "walleye_detections.csv",
  package = "glatos"
)
det <- read_glatos_detections(det_file)
detection_events <- glatos::detection_events(det)
#> The event filter distilled 7180 detections down to 165 distinct detection events.
rik_data <- glatos::residence_index(detection_events,
  calculation_method = "kessel"
)
rit_data <- glatos::residence_index(detection_events,
  calculation_method = "time_interval"
)
rit_data <- glatos::residence_index(detection_events,
  calculation_method = "timedelta"
)
riawo_data <- glatos::residence_index(detection_events,
  calculation_method = "aggregate_with_overlap"
)
#> NOTE: Becuase an individual animal cannot overlap with itself, this will produce the same output as aggregate_no_overlap when animal_id is passed to group_col.
riano_data <- glatos::residence_index(detection_events,
  calculation_method = "aggregate_no_overlap"
)
```
