# The function below aggregates timedelta of first_detection and last_detection, excluding overlap between detections. Any overlap between two detections is converted to a new detection using the earlier first_detection and the latest last_detection. If the first_detection and last_detection are the same, a timedelta of one second is assumed.

The function below aggregates timedelta of first_detection and
last_detection, excluding overlap between detections. Any overlap
between two detections is converted to a new detection using the earlier
first_detection and the latest last_detection. If the first_detection
and last_detection are the same, a timedelta of one second is assumed.

## Usage

``` r
aggregate_total_no_overlap(detections)
```

## Arguments

- detections:

  - data frame pulled from the compressed detections CSV
