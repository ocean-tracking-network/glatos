# Summarize detections by animal, location, or both

Calculate number of fish detected, number of detections, first and last
detection timestamps, and/or mean location of receivers or groups,
depending on specific type of summary requested.

## Usage

``` r
summarize_detections(
  det,
  location_col = "glatos_array",
  receiver_locs = NULL,
  animals = NULL,
  summ_type = "animal"
)
```

## Arguments

- det:

  A `glatos_detections` object (e.g., produced by
  [read_glatos_detections](https://ocean-tracking-network.github.io/glatos/reference/read_glatos_detections.md)).

  *OR* a data frame containing detection data with four columns
  described below and one column containing a location grouping
  variable, whose name is specified by `location_col` (see below).

  The following four columns must appear in `det`, except `deploy_lat`
  and `deploy_lon` are not needed if `receiver_locs` is specified:

  `animal_id`

  :   Individual animal identifier; character.

  `detection_timestamp_utc`

  :   Timestamps for the detections (MUST be of class 'POSIXct').

  `deploy_lat`

  :   Latitude of receiver deployment in decimal degrees, NAD83.

  `deploy_long`

  :   Longitude of receiver deployment in decimal degrees, NAD83.

- location_col:

  A character string indicating the column name in `det` (and
  `receiver_locs` if specified) that will be used as the location
  grouping variable (e.g. "glatos_array"), in quotes.

- receiver_locs:

  An optional data frame containing receiver data with the two columns
  ('deploy_lat', 'deploy_long') described below and one column
  containing a location grouping variable, whose name is specified by
  `location_col` (see above). The following two columns must appear in
  `receiver_locs`:

  - `deploy_lat` Latitude of receiver deployment in decimal degrees,
    NAD83.

  - `deploy_long` Longitude of receiver deployment in decimal degrees,
    NAD83.

- animals:

  A character vector with values of 'animal_id' that will be included in
  summary. This allows (1) animals *not* detected (i.e., not present in
  `det`) to be included in the summary and/or (2) unwanted animals in
  `det` to be excluded from the summary.

- summ_type:

  A character string indicating the primary focus of the summary.
  Possible values are `"animal"` (default), `"location"`, and `"both"`.
  See Details below.

## Value

If `summ_type = "animal"` (default): A data frame, data.table, or tibble
containing six columns:

- `animal_id`: described above.

- `num_locs`: number of locations.

- `num_dets`: number of detections.

- `first_det`: first detection timestamp.

- `last_det`: last detections timestamp.

- `locations`: character string with locations detected, separated by
  spaces.

If `summ_type = "location"`: A data frame, data.table, or tibble
containing eight columns:

- `LOCATION_COL`: defined by `location_col`.

- `num_fish`: number of unique animals detected.

- `num_dets`: number of detections.

- `first_det`: first detection timestamp.

- `last_det`: last detections timestamp.

- `mean_lat`: mean latitude of receivers at this location.

- `mean_lon`: mean longitude of receivers at this location.

- `animals`: character string with animal_ids detected, separated by
  spaces.

If `summ_type = "both"`: A data frame, data.table, or tibble containing
seven columns:

- `animal_id`: described above.

- `LOCATION_COL`: defined by `location_col`.

- `num_dets`: number of detections.

- `first_det`: first detection timestamp.

- `last_det`: last detections timestamp.

- `mean_lat`: mean latitude of receivers at this location.

- `mean_lon`: mean longitude of receivers at this location.

## Details

Input argument `summ_type` determines which of three possible summaries
is conducted. If `summ_type = "animal"` (default), the output summary
includes the following for each unique value of `animal_id`: number of
unique locations (defined by unique values of `location_col`), total
number of detections across all locations, timestamp of first and last
detection across all locations, and a space-delimited string showing all
locations where each animal was detected. If `summ_type = "location"`,
the output summary includes the following for each unique value of
`location_col`: number of animals (defined by unique values of
`animal_id`), total number of detections across all animals, timestamp
of first and last detection across all animals, mean latitude and
longitude of each location group, and a space-delimited string of each
unique animal that was detected. If `summ_type = "both"`, the output
summary includes the following for each unique combination of
`location_col` and `animal_id`: total number of detections, timestamp of
first and last detection, and mean latitude and longitude.

If `receiver_locs = NULL` (default), then mean latitude and longitude of
each location (`mean_lat` and `mean_lon` in output data frame) will be
calculated from data in `det`. Therefore, mean locations in the output
summary may not represent the mean among all receiver stations in a
particular group if detections did not occur on all receivers in each
group. However, when actual receiver locations are specified by
`receiver_locs`, then `mean_lat` and `mean_lon` will be calculated from
`receiver_locs`. Also, if mean location is not desired or suitable, then
`receiver_locs` can be used to pass a single user-specified `deploy_lat`
and `deploy_long` for each unique value of `location_col`, whose values
would then represent `mean_lat` and `mean_lon` in the output summary.

## Author

T. R. Binder and C. Holbrook

## Examples

``` r

# get path to example detection file
det_file <- system.file("extdata", "walleye_detections.csv",
  package = "glatos"
)
det <- read_glatos_detections(det_file)

# Basic summaries

# by animal
ds <- summarize_detections(det)

# by location
ds <- summarize_detections(det, summ_type = "location")

# by animal and location
ds <- summarize_detections(det, summ_type = "both")


# Include user-defined location_col

# by animal
det$some_place <- ifelse(grepl("^S", det$glatos_array), "s", "not_s")

ds <- summarize_detections(det, location_col = "some_place")

# by location
ds <- summarize_detections(det,
  location_col = "some_place",
  summ_type = "location"
)

# by animal and location
ds <- summarize_detections(det,
  location_col = "some_place",
  summ_type = "both"
)


# Include locations where no animals detected

# get example receiver data
rec_file <- system.file("extdata", "sample_receivers.csv",
  package = "glatos"
)
rec <- read_glatos_receivers(rec_file)

ds <- summarize_detections(det, receiver_locs = rec, summ_type = "location")


# Include animals that were not detected
# get example animal data from walleye workbook
wb_file <- system.file("extdata", "walleye_workbook.xlsm",
  package = "glatos"
)
wb <- read_glatos_workbook(wb_file)

ds <- summarize_detections(det, animals = wb$animals, summ_type = "animal")

# Include by animals and locations that were not detected
ds <- summarize_detections(det,
  receiver_locs = rec, animals = wb$animals,
  summ_type = "both"
)
```
