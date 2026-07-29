# Detection Efficiency data set

Sample detection efficiency data set from Lake Papineau, Quebec, Canada.

## Usage

``` r
sample_detection_efficiency
```

## Format

A data frame with 7 rows and 5 variables

- distance_m:

  distance away from the receiver in meters

- avg_percent:

  average detection efficiency

- std_dev:

  standard deviation of detection efficiency

- avg_percent_d:

  average detection efficiency in decimal form needs to be created by
  dividing `avg_percent` by 100

- intercept:

  y-intercept used for third order polynomial, set at 100. Needs to be
  added to the original dataframe

## Source

B.L. Hlina

## Details

Data is from a preliminary range test, where tags were deployed at set
distances away from a VR2W receiver for 24 hours. Once downloaded the
vrl files were used in Vemco's Range Testing Software to produced this
dataset.
