# The function below takes a detection events data frame and determines the number of time bins in which detections were observed and returns the cumulative time covered by all bins, in days. Interval (bin) size is determined by the 'time_interval_size' argument.

For each event (row in detection events data frame), the function
sequences from first_detection to last_detection by time_interval_size,
then counts the number of unique intervals.

## Usage

``` r
interval_count(detections, time_interval_size)
```

## Arguments

- detections:

  - data frame from detection_events (condensed = TRUE)

- time_interval_size:

  time increment string as in seq.Date 'by' argument
