# Determines which calculation method to use for the residency index.

Wrapper method for the calulation methods above.

## Usage

``` r
get_days(dets, calculation_method = "kessel", time_interval_size = "1 day")
```

## Arguments

- dets:

  - data frame pulled from the detection events

- calculation_method:

  - determines which method above will be used to count total time and
    location time

- time_interval_size:

  - size of time interval
