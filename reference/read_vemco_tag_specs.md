# Read telemetry transmitter (tag) specification data from a Vemco file

Read telemetry transmitter (tag) specification data from a file and
return a list with tag specifications and tag operating schedule.

## Usage

``` r
read_vemco_tag_specs(tag_file, file_format = NULL)
```

## Arguments

- tag_file:

  A character string with path and name of file in a supported standard
  format in quotes. If only file name is given, then the file must be
  located in the working directory.

- file_format:

  A character string with the tag spec file format in quotes. If NULL
  (default value) then version will be determined by evaluating file
  structure. The only allowed values are `NULL` and `"vemco_xls"`. Any
  other values will trigger an error.

## Value

A list containing two data frames with tag specifications and tag
operating schedule.

A list element called `specs` is a data frame contains tag
specifications data in 17 columns:

- serial_number:
- manufacturer:
- model:
- id_count:
- code_space:
- id_code:
- n_steps:
- sensor_type:
- sensor_range:
- sensor_units:
- sensor_slope:
- sensor_intercept:
- accel_algorithm:
- accel_sample_rate:
- sensor_transmit_ratio:
- est_battery_life_days:
- battery_life_stat:

A list element called `schedule` is a data frame containing tag
operating shedule data in 11 columns:

- serial_number:
- code_space:
- id_code:
- step:
- next_step:
- status:
- duration_days:
- power:
- min_delay_secs:
- max_delay_secs:
- accel_on_time_secs:

## Details

The file format `vemco_xls` is a MS Excel file provided to tag
purchasers by Vemco.

This function is not endorsed or supported by any transmitter
manufacturer.

## Author

C. Holbrook, <cholbrook@glfc.org>

## Examples

``` r
# get path to example Vemco tag spec file
spec_file <- system.file("extdata",
  "lamprey_tag_specs.xls",
  package = "glatos"
)
my_tags <- read_vemco_tag_specs(spec_file, file_format = "vemco_xls")
```
