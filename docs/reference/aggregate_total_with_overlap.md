# The function below aggregates timedelta of first_detection and last_detection of each detection into a final timedelta then returns a float of the number of days. If the first_detection and last_detection are the same, a timedelta of one second is assumed.

The function below aggregates timedelta of first_detection and
last_detection of each detection into a final timedelta then returns a
float of the number of days. If the first_detection and last_detection
are the same, a timedelta of one second is assumed.

## Usage

``` r
aggregate_total_with_overlap(detections)
```

## Arguments

- detections:

  -data frame pulled from the compressed detections CSV
