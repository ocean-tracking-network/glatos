# Make bubble plots showing the number of fish detected across a defined set of receiver locations.

Make bubble plots showing the number of fish detected across a defined
set of receiver locations.

## Usage

``` r
detection_bubble_plot(
  det,
  location_col = "glatos_array",
  receiver_locs = NULL,
  map = NULL,
  out_file = NULL,
  background_ylim = c(41.3, 49),
  background_xlim = c(-92.45, -75.87),
  symbol_radius = 1,
  col_grad = c("white", "red"),
  scale_loc = NULL
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

- map:

  An optional sp, sf, or terra::SpatVect spatial object that can by
  plotted with using `plot` to be included as the background for the
  plot. If NULL, then the example Great Lakes polygon object
  (`data(great_lakes_polygon)`) will be used. Map CRS must be in
  EPSG:4326 or conversion will be attempted.

- out_file:

  An optional character string with the name (including extension) of
  output file created. File extension will determine type of file
  written. For example, `"BubblePlot.png"` will write a png file to the
  working directory. If `NULL` (default) then the plot will be printed
  to the default plot device. Supported extensions: png, jpeg, bmp, and
  tiff.

- background_ylim:

  A two-element numeric vector that defines minimum and maximum extents
  of the viewable plot area along the y-axis (i.e., longitude).

- background_xlim:

  A two-element numeric vector that defines minimum and maximum extents
  of the viewable plot area along the x-axis (i.e., latitude).

- symbol_radius:

  Radius of each "bubble" on the plot in units of percent of x-axis
  scale. Default value = 1 (i.e., 1 percent of x-axis).

- col_grad:

  A two-element character vector indicating the start and end colors of
  the gradient scale used to color-code "bubbles".

- scale_loc:

  An optional 4-element numeric vector, to be passed to
  plotrix::color.legend, indicating the plotting location of the legend
  in the same units as `map`. Elements in the vector are the lower left
  and upper right coordinates of the rectangle of colors (i.e., c(xleft,
  ybottom, xright, ytop)). If `scale_loc` = NULL (default), the legend
  is plotted along the left edge of the plot.

## Value

A data frame produced by
`glatos::summarize_detections(det, location_col = location_col, receiver_locs = receiver_locs, summ_type = "location")`

If not out_file is specified, then an image is printed to the default
plot device. If out_file is specified, then an image of specified type
is written to `out_file`.

## Details

Data are summarized using
[summarize_detections](https://ocean-tracking-network.github.io/glatos/reference/summarize_detections.md).

If `receiver_locs` is specified (not NULL) then the plot will show all
receivers in `receiver_locs` including any that detected none of the
transmitters in `det`. Although this is helpful to view locations where
fish were *not* detected, the user will usually want to take care to
include only receivers that were in the water during the period of
interest. If you are using a glatos receiver locations file to specify
location for plotting, you will likely want to filter the receiver data
by depoyment and receovery dates to exclude deployments that occured
outside of the period of interest.

"col_grad" is used in a call to
[colorRampPalette](https://rdrr.io/r/grDevices/colorRamp.html), which
will accept a vector containing any two colors return by
[colors](https://rdrr.io/r/grDevices/colors.html) as character strings.

## See also

[`summarize_detections()`](https://ocean-tracking-network.github.io/glatos/reference/summarize_detections.md)

## Author

T. R. Binder, edited by A. Dini

## Examples

``` r

# get path to example detection file
det_file <- system.file("extdata", "walleye_detections.csv",
  package = "glatos"
)
det <- read_glatos_detections(det_file)

# call with defaults
detection_bubble_plot(det, map = great_lakes_polygon)
#>    glatos_array num_fish num_dets           first_det            last_det
#> 1           DRF        1       62 2012-05-26 15:12:15 2012-05-26 19:00:20
#> 2           DRL        1      186 2012-05-25 10:10:54 2012-05-25 22:48:07
#> 3           DRU        1      169 2012-05-27 07:33:37 2012-05-27 22:56:50
#> 4           FMP        1      856 2012-08-23 02:31:57 2012-09-26 21:33:37
#> 6           OSC        1       47 2012-05-25 04:52:21 2013-05-09 15:10:33
#> 7           PRS        1      665 2012-07-26 10:23:32 2012-10-20 14:27:35
#> 8           RAR        1     1765 2013-04-06 16:38:50 2013-05-01 17:19:15
#> 9           SBI        1      452 2012-05-23 01:24:51 2013-05-07 18:03:32
#> 10          SBO        1      429 2012-05-24 08:01:20 2013-05-09 00:00:15
#> 11          SCL        1       56 2012-05-29 20:06:34 2012-05-30 07:12:43
#> 12          SCM        1       41 2012-05-30 08:42:48 2012-05-31 02:11:16
#> 13          SGR        1      171 2012-04-30 04:46:40 2013-04-18 02:58:39
#> 14          SHR        1        1 2013-04-17 02:55:59 2013-04-17 02:55:59
#> 15          STG        1        7 2012-10-29 21:19:04 2012-10-29 21:54:28
#> 16          THB        1      362 2012-07-15 11:15:40 2012-07-20 09:28:34
#> 17          TSR        1      221 2013-04-14 18:35:52 2013-04-28 17:53:37
#> 18          TTB        1       56 2012-04-29 01:48:37 2013-04-17 02:52:31
#> 5           MAU        2     1634 2012-03-27 13:05:27 2012-04-09 18:33:54
#>    mean_lat  mean_lon animals
#> 1  42.24937 -83.11824      23
#> 2  42.09788 -83.11929      23
#> 3  42.34058 -82.97523      23
#> 4  45.50111 -83.90479     153
#> 6  44.45161 -83.30285     153
#> 7  45.34239 -83.44448     153
#> 8  41.63705 -82.97412      22
#> 9  44.13348 -83.43968     153
#> 10 44.23100 -83.41075     153
#> 11 42.56357 -82.57476      23
#> 12 42.75687 -82.47448      23
#> 13 43.61160 -83.86796     153
#> 14 43.37698 -83.99115     153
#> 15 44.71315 -83.20110     153
#> 16 44.95750 -83.29510     153
#> 17 41.62433 -83.01284      22
#> 18 43.38809 -83.98855     153
#> 5  41.60799 -83.57188   22 23

# change symbol size and color
detection_bubble_plot(det, symbol_radius = 2, col_grad = c("grey90", "grey10"))
#>    glatos_array num_fish num_dets           first_det            last_det
#> 1           DRF        1       62 2012-05-26 15:12:15 2012-05-26 19:00:20
#> 2           DRL        1      186 2012-05-25 10:10:54 2012-05-25 22:48:07
#> 3           DRU        1      169 2012-05-27 07:33:37 2012-05-27 22:56:50
#> 4           FMP        1      856 2012-08-23 02:31:57 2012-09-26 21:33:37
#> 6           OSC        1       47 2012-05-25 04:52:21 2013-05-09 15:10:33
#> 7           PRS        1      665 2012-07-26 10:23:32 2012-10-20 14:27:35
#> 8           RAR        1     1765 2013-04-06 16:38:50 2013-05-01 17:19:15
#> 9           SBI        1      452 2012-05-23 01:24:51 2013-05-07 18:03:32
#> 10          SBO        1      429 2012-05-24 08:01:20 2013-05-09 00:00:15
#> 11          SCL        1       56 2012-05-29 20:06:34 2012-05-30 07:12:43
#> 12          SCM        1       41 2012-05-30 08:42:48 2012-05-31 02:11:16
#> 13          SGR        1      171 2012-04-30 04:46:40 2013-04-18 02:58:39
#> 14          SHR        1        1 2013-04-17 02:55:59 2013-04-17 02:55:59
#> 15          STG        1        7 2012-10-29 21:19:04 2012-10-29 21:54:28
#> 16          THB        1      362 2012-07-15 11:15:40 2012-07-20 09:28:34
#> 17          TSR        1      221 2013-04-14 18:35:52 2013-04-28 17:53:37
#> 18          TTB        1       56 2012-04-29 01:48:37 2013-04-17 02:52:31
#> 5           MAU        2     1634 2012-03-27 13:05:27 2012-04-09 18:33:54
#>    mean_lat  mean_lon animals
#> 1  42.24937 -83.11824      23
#> 2  42.09788 -83.11929      23
#> 3  42.34058 -82.97523      23
#> 4  45.50111 -83.90479     153
#> 6  44.45161 -83.30285     153
#> 7  45.34239 -83.44448     153
#> 8  41.63705 -82.97412      22
#> 9  44.13348 -83.43968     153
#> 10 44.23100 -83.41075     153
#> 11 42.56357 -82.57476      23
#> 12 42.75687 -82.47448      23
#> 13 43.61160 -83.86796     153
#> 14 43.37698 -83.99115     153
#> 15 44.71315 -83.20110     153
#> 16 44.95750 -83.29510     153
#> 17 41.62433 -83.01284      22
#> 18 43.38809 -83.98855     153
#> 5  41.60799 -83.57188   22 23

# Add all receivers

# get path to example receiver file
rec_file <- system.file("extdata", "sample_receivers.csv",
  package = "glatos"
)
rec <- read_glatos_receivers(rec_file)

detection_bubble_plot(det, receiver_locs = rec)
#>    glatos_array num_fish num_dets           first_det            last_det
#> 1           AGR        0        0                <NA>                <NA>
#> 2           BBI        0        0                <NA>                <NA>
#> 3           BBW        0        0                <NA>                <NA>
#> 4           BLC        0        0                <NA>                <NA>
#> 5           BLL        0        0                <NA>                <NA>
#> 6           BMR        0        0                <NA>                <NA>
#> 7           BPW        0        0                <NA>                <NA>
#> 8           BRS        0        0                <NA>                <NA>
#> 9           CBA        0        0                <NA>                <NA>
#> 10          CBR        0        0                <NA>                <NA>
#> 11          CE1        0        0                <NA>                <NA>
#> 12          CE2        0        0                <NA>                <NA>
#> 13          CE3        0        0                <NA>                <NA>
#> 14          CHB        0        0                <NA>                <NA>
#> 15          CKI        0        0                <NA>                <NA>
#> 16          CSR        0        0                <NA>                <NA>
#> 17          DCK        0        0                <NA>                <NA>
#> 18          DOR        0        0                <NA>                <NA>
#> 21          DRM        0        0                <NA>                <NA>
#> 23          DTR        0        0                <NA>                <NA>
#> 24          EAG        0        0                <NA>                <NA>
#> 25          ECH        0        0                <NA>                <NA>
#> 26          EDS        0        0                <NA>                <NA>
#> 27          EMA        0        0                <NA>                <NA>
#> 28          EMB        0        0                <NA>                <NA>
#> 29          EMC        0        0                <NA>                <NA>
#> 30          EMD        0        0                <NA>                <NA>
#> 31          EXG        0        0                <NA>                <NA>
#> 32          FDS        0        0                <NA>                <NA>
#> 33          FDT        0        0                <NA>                <NA>
#> 34          FLT        0        0                <NA>                <NA>
#> 36          FRC        0        0                <NA>                <NA>
#> 37          GAT        0        0                <NA>                <NA>
#> 38          GRD        0        0                <NA>                <NA>
#> 39          HBB        0        0                <NA>                <NA>
#> 40          HBC        0        0                <NA>                <NA>
#> 41          HPT        0        0                <NA>                <NA>
#> 42          IGN        0        0                <NA>                <NA>
#> 43          IS1        0        0                <NA>                <NA>
#> 44          JAS        0        0                <NA>                <NA>
#> 45          LGD        0        0                <NA>                <NA>
#> 46          LKO        0        0                <NA>                <NA>
#> 47          LRC        0        0                <NA>                <NA>
#> 48          LRP        0        0                <NA>                <NA>
#> 49          LVD        0        0                <NA>                <NA>
#> 50          LWS        0        0                <NA>                <NA>
#> 52          MCK        0        0                <NA>                <NA>
#> 53          MNB        0        0                <NA>                <NA>
#> 54          MWF        0        0                <NA>                <NA>
#> 55          NNM        0        0                <NA>                <NA>
#> 56          OHM        0        0                <NA>                <NA>
#> 57          ORM        0        0                <NA>                <NA>
#> 59          OVP        0        0                <NA>                <NA>
#> 60          PAS        0        0                <NA>                <NA>
#> 61          PCH        0        0                <NA>                <NA>
#> 63          PWC        0        0                <NA>                <NA>
#> 65          RND        0        0                <NA>                <NA>
#> 66          RNS        0        0                <NA>                <NA>
#> 67          RTR        0        0                <NA>                <NA>
#> 68          RVR        0        0                <NA>                <NA>
#> 73          SCU        0        0                <NA>                <NA>
#> 76          SPS        0        0                <NA>                <NA>
#> 77          SSM        0        0                <NA>                <NA>
#> 79          STR        0        0                <NA>                <NA>
#> 81          THR        0        0                <NA>                <NA>
#> 82          TOI        0        0                <NA>                <NA>
#> 85          TUB        0        0                <NA>                <NA>
#> 86          URC        0        0                <NA>                <NA>
#> 87          WEG        0        0                <NA>                <NA>
#> 88          WHT        0        0                <NA>                <NA>
#> 19          DRF        1       62 2012-05-26 15:12:15 2012-05-26 19:00:20
#> 20          DRL        1      186 2012-05-25 10:10:54 2012-05-25 22:48:07
#> 22          DRU        1      169 2012-05-27 07:33:37 2012-05-27 22:56:50
#> 35          FMP        1      856 2012-08-23 02:31:57 2012-09-26 21:33:37
#> 58          OSC        1       47 2012-05-25 04:52:21 2013-05-09 15:10:33
#> 62          PRS        1      665 2012-07-26 10:23:32 2012-10-20 14:27:35
#> 64          RAR        1     1765 2013-04-06 16:38:50 2013-05-01 17:19:15
#> 69          SBI        1      452 2012-05-23 01:24:51 2013-05-07 18:03:32
#> 70          SBO        1      429 2012-05-24 08:01:20 2013-05-09 00:00:15
#> 71          SCL        1       56 2012-05-29 20:06:34 2012-05-30 07:12:43
#> 72          SCM        1       41 2012-05-30 08:42:48 2012-05-31 02:11:16
#> 74          SGR        1      171 2012-04-30 04:46:40 2013-04-18 02:58:39
#> 75          SHR        1        1 2013-04-17 02:55:59 2013-04-17 02:55:59
#> 78          STG        1        7 2012-10-29 21:19:04 2012-10-29 21:54:28
#> 80          THB        1      362 2012-07-15 11:15:40 2012-07-20 09:28:34
#> 83          TSR        1      221 2013-04-14 18:35:52 2013-04-28 17:53:37
#> 84          TTB        1       56 2012-04-29 01:48:37 2013-04-17 02:52:31
#> 51          MAU        2     1634 2012-03-27 13:05:27 2012-04-09 18:33:54
#>    mean_lat  mean_lon animals
#> 1  44.02980 -83.68433    <NA>
#> 2  45.69734 -84.41925    <NA>
#> 3  45.77276 -84.61658    <NA>
#> 4  46.49420 -84.27662    <NA>
#> 5  46.53541 -84.21317    <NA>
#> 6  45.53289 -84.12079    <NA>
#> 7  43.08186 -82.20260    <NA>
#> 8  46.47216 -84.46207    <NA>
#> 9  41.82020 -81.30392    <NA>
#> 10 45.65261 -84.46752    <NA>
#> 11 43.63108 -79.32430    <NA>
#> 12 43.62807 -79.32715    <NA>
#> 13 43.62619 -79.33060    <NA>
#> 14 43.64033 -79.33105    <NA>
#> 15 46.49521 -84.27035    <NA>
#> 16 43.36799 -83.96504    <NA>
#> 17 46.36007 -84.13276    <NA>
#> 18 43.64731 -79.35467    <NA>
#> 21 45.92327 -83.65502    <NA>
#> 23 45.98763 -83.88809    <NA>
#> 24 43.63335 -79.35188    <NA>
#> 25 46.51782 -84.02165    <NA>
#> 26 46.49851 -84.33377    <NA>
#> 27 43.62084 -79.34173    <NA>
#> 28 43.62453 -79.33883    <NA>
#> 29 43.62935 -79.33401    <NA>
#> 30 43.63258 -79.32763    <NA>
#> 31 43.62927 -79.41886    <NA>
#> 32 43.63758 -79.38940    <NA>
#> 33 45.93486 -83.48593    <NA>
#> 34 43.31209 -84.02969    <NA>
#> 36 46.45858 -84.27980    <NA>
#> 37 43.62431 -79.34998    <NA>
#> 38 46.52954 -84.15295    <NA>
#> 39 43.17178 -82.09256    <NA>
#> 40 43.88090 -82.59831    <NA>
#> 41 46.46114 -84.12862    <NA>
#> 42 45.85610 -84.67575    <NA>
#> 43 46.47969 -84.29553    <NA>
#> 44 43.64308 -79.36829    <NA>
#> 45 46.41098 -84.14955    <NA>
#> 46 43.63490 -79.32217    <NA>
#> 47 46.26405 -84.18871    <NA>
#> 48 46.48007 -84.28788    <NA>
#> 49 42.12930 -83.12518    <NA>
#> 50 46.51282 -84.24440    <NA>
#> 52 45.81904 -84.75589    <NA>
#> 53 46.32552 -84.15832    <NA>
#> 54 43.63881 -79.37734    <NA>
#> 55 46.38761 -84.23144    <NA>
#> 56 43.63956 -79.32485    <NA>
#> 57 45.49189 -84.07187    <NA>
#> 59 46.50095 -84.39186    <NA>
#> 60 43.64613 -79.36030    <NA>
#> 61 46.52558 -84.17251    <NA>
#> 63 46.50521 -84.35220    <NA>
#> 65 45.82494 -84.60678    <NA>
#> 66 46.25878 -84.10362    <NA>
#> 67 46.54254 -84.21432    <NA>
#> 68 46.47160 -84.29758    <NA>
#> 73 43.00502 -82.41486    <NA>
#> 76 43.63719 -79.39152    <NA>
#> 77 46.50811 -84.34101    <NA>
#> 79 46.31807 -84.11548    <NA>
#> 81 45.06519 -83.43151    <NA>
#> 82 43.62240 -79.37472    <NA>
#> 85 43.64524 -79.34102    <NA>
#> 86 46.28645 -84.21152    <NA>
#> 87 43.63209 -79.39813    <NA>
#> 88 43.73778 -82.54340    <NA>
#> 19 42.21130 -83.12282      23
#> 20 42.09358 -83.14240      23
#> 22 42.34595 -82.95458      23
#> 35 45.50038 -83.90513     153
#> 58 44.45158 -83.24894     153
#> 62 45.33992 -83.44845     153
#> 64 41.63733 -82.97433      22
#> 69 44.09220 -83.38488     153
#> 70 44.13973 -83.26330     153
#> 71 42.60437 -82.62050      23
#> 72 42.74667 -82.47673      23
#> 74 43.61091 -83.87384     153
#> 75 43.37418 -83.99795     153
#> 78 44.71326 -83.20107     153
#> 80 44.95102 -83.29738     153
#> 83 41.62433 -83.01284      22
#> 84 43.38934 -83.98999     153
#> 51 41.59606 -83.58600   22 23


#' #Subset receivers to include on receivers that were deployed during the
#' detection interval.

# get path to example receiver file
rec_file <- system.file("extdata", "sample_receivers.csv",
  package = "glatos"
)
rec <- read_glatos_receivers(rec_file)

first <- min(det$detection_timestamp_utc) # time of first detection
last <- max(det$detection_timestamp_utc) # time of last detection

# Subset receiver deployments oustide the detection period.
# !is.na(rec$recover_date_time) eliminates receivers that have been
# deployed but not yet recovered.
plot_rec <- rec[rec$deploy_date_time < last &
  rec$recover_date_time > first &
  !is.na(rec$recover_date_time), ]

detection_bubble_plot(det, receiver_locs = plot_rec)
#>    glatos_array num_fish num_dets           first_det            last_det
#> 1           AGR        0        0                <NA>                <NA>
#> 2           BBI        0        0                <NA>                <NA>
#> 3           BBW        0        0                <NA>                <NA>
#> 4           BLC        0        0                <NA>                <NA>
#> 5           BLL        0        0                <NA>                <NA>
#> 6           BMR        0        0                <NA>                <NA>
#> 7           BPW        0        0                <NA>                <NA>
#> 8           BRS        0        0                <NA>                <NA>
#> 9           CBA        0        0                <NA>                <NA>
#> 10          CBR        0        0                <NA>                <NA>
#> 11          CE1        0        0                <NA>                <NA>
#> 12          CE2        0        0                <NA>                <NA>
#> 13          CE3        0        0                <NA>                <NA>
#> 14          CHB        0        0                <NA>                <NA>
#> 15          CKI        0        0                <NA>                <NA>
#> 16          CSR        0        0                <NA>                <NA>
#> 17          DCK        0        0                <NA>                <NA>
#> 18          DOR        0        0                <NA>                <NA>
#> 21          DRM        0        0                <NA>                <NA>
#> 23          DTR        0        0                <NA>                <NA>
#> 24          EAG        0        0                <NA>                <NA>
#> 25          ECH        0        0                <NA>                <NA>
#> 26          EDS        0        0                <NA>                <NA>
#> 27          EMA        0        0                <NA>                <NA>
#> 28          EMB        0        0                <NA>                <NA>
#> 29          EMC        0        0                <NA>                <NA>
#> 30          EMD        0        0                <NA>                <NA>
#> 31          EXG        0        0                <NA>                <NA>
#> 32          FDS        0        0                <NA>                <NA>
#> 33          FDT        0        0                <NA>                <NA>
#> 34          FLT        0        0                <NA>                <NA>
#> 36          FRC        0        0                <NA>                <NA>
#> 37          GAT        0        0                <NA>                <NA>
#> 38          GRD        0        0                <NA>                <NA>
#> 39          HBB        0        0                <NA>                <NA>
#> 40          HBC        0        0                <NA>                <NA>
#> 41          HPT        0        0                <NA>                <NA>
#> 42          IGN        0        0                <NA>                <NA>
#> 43          IS1        0        0                <NA>                <NA>
#> 44          JAS        0        0                <NA>                <NA>
#> 45          LGD        0        0                <NA>                <NA>
#> 46          LKO        0        0                <NA>                <NA>
#> 47          LRC        0        0                <NA>                <NA>
#> 48          LRP        0        0                <NA>                <NA>
#> 49          LVD        0        0                <NA>                <NA>
#> 50          LWS        0        0                <NA>                <NA>
#> 52          MCK        0        0                <NA>                <NA>
#> 53          MNB        0        0                <NA>                <NA>
#> 54          MWF        0        0                <NA>                <NA>
#> 55          NNM        0        0                <NA>                <NA>
#> 56          OHM        0        0                <NA>                <NA>
#> 57          ORM        0        0                <NA>                <NA>
#> 59          OVP        0        0                <NA>                <NA>
#> 60          PAS        0        0                <NA>                <NA>
#> 61          PCH        0        0                <NA>                <NA>
#> 63          PWC        0        0                <NA>                <NA>
#> 65          RND        0        0                <NA>                <NA>
#> 66          RNS        0        0                <NA>                <NA>
#> 67          RTR        0        0                <NA>                <NA>
#> 68          RVR        0        0                <NA>                <NA>
#> 73          SCU        0        0                <NA>                <NA>
#> 76          SPS        0        0                <NA>                <NA>
#> 77          SSM        0        0                <NA>                <NA>
#> 79          STR        0        0                <NA>                <NA>
#> 81          THR        0        0                <NA>                <NA>
#> 82          TOI        0        0                <NA>                <NA>
#> 85          TUB        0        0                <NA>                <NA>
#> 86          URC        0        0                <NA>                <NA>
#> 87          WEG        0        0                <NA>                <NA>
#> 88          WHT        0        0                <NA>                <NA>
#> 19          DRF        1       62 2012-05-26 15:12:15 2012-05-26 19:00:20
#> 20          DRL        1      186 2012-05-25 10:10:54 2012-05-25 22:48:07
#> 22          DRU        1      169 2012-05-27 07:33:37 2012-05-27 22:56:50
#> 35          FMP        1      856 2012-08-23 02:31:57 2012-09-26 21:33:37
#> 58          OSC        1       47 2012-05-25 04:52:21 2013-05-09 15:10:33
#> 62          PRS        1      665 2012-07-26 10:23:32 2012-10-20 14:27:35
#> 64          RAR        1     1765 2013-04-06 16:38:50 2013-05-01 17:19:15
#> 69          SBI        1      452 2012-05-23 01:24:51 2013-05-07 18:03:32
#> 70          SBO        1      429 2012-05-24 08:01:20 2013-05-09 00:00:15
#> 71          SCL        1       56 2012-05-29 20:06:34 2012-05-30 07:12:43
#> 72          SCM        1       41 2012-05-30 08:42:48 2012-05-31 02:11:16
#> 74          SGR        1      171 2012-04-30 04:46:40 2013-04-18 02:58:39
#> 75          SHR        1        1 2013-04-17 02:55:59 2013-04-17 02:55:59
#> 78          STG        1        7 2012-10-29 21:19:04 2012-10-29 21:54:28
#> 80          THB        1      362 2012-07-15 11:15:40 2012-07-20 09:28:34
#> 83          TSR        1      221 2013-04-14 18:35:52 2013-04-28 17:53:37
#> 84          TTB        1       56 2012-04-29 01:48:37 2013-04-17 02:52:31
#> 51          MAU        2     1634 2012-03-27 13:05:27 2012-04-09 18:33:54
#>    mean_lat  mean_lon animals
#> 1  44.02980 -83.68433    <NA>
#> 2  45.69734 -84.41925    <NA>
#> 3  45.77276 -84.61658    <NA>
#> 4  46.49420 -84.27662    <NA>
#> 5  46.53541 -84.21317    <NA>
#> 6  45.53289 -84.12079    <NA>
#> 7  43.08186 -82.20260    <NA>
#> 8  46.47216 -84.46207    <NA>
#> 9  41.82020 -81.30392    <NA>
#> 10 45.65261 -84.46752    <NA>
#> 11 43.63108 -79.32430    <NA>
#> 12 43.62807 -79.32715    <NA>
#> 13 43.62619 -79.33060    <NA>
#> 14 43.64033 -79.33105    <NA>
#> 15 46.49521 -84.27035    <NA>
#> 16 43.36799 -83.96504    <NA>
#> 17 46.36007 -84.13276    <NA>
#> 18 43.64731 -79.35467    <NA>
#> 21 45.92327 -83.65502    <NA>
#> 23 45.98763 -83.88809    <NA>
#> 24 43.63335 -79.35188    <NA>
#> 25 46.51782 -84.02165    <NA>
#> 26 46.49851 -84.33377    <NA>
#> 27 43.62084 -79.34173    <NA>
#> 28 43.62453 -79.33883    <NA>
#> 29 43.62935 -79.33401    <NA>
#> 30 43.63258 -79.32763    <NA>
#> 31 43.62927 -79.41886    <NA>
#> 32 43.63758 -79.38940    <NA>
#> 33 45.93486 -83.48593    <NA>
#> 34 43.31209 -84.02969    <NA>
#> 36 46.45858 -84.27980    <NA>
#> 37 43.62431 -79.34998    <NA>
#> 38 46.52954 -84.15295    <NA>
#> 39 43.17178 -82.09256    <NA>
#> 40 43.88090 -82.59831    <NA>
#> 41 46.46114 -84.12862    <NA>
#> 42 45.85610 -84.67575    <NA>
#> 43 46.47969 -84.29553    <NA>
#> 44 43.64308 -79.36829    <NA>
#> 45 46.41098 -84.14955    <NA>
#> 46 43.63490 -79.32217    <NA>
#> 47 46.26405 -84.18871    <NA>
#> 48 46.48007 -84.28788    <NA>
#> 49 42.12930 -83.12518    <NA>
#> 50 46.51282 -84.24440    <NA>
#> 52 45.81904 -84.75589    <NA>
#> 53 46.32552 -84.15832    <NA>
#> 54 43.63881 -79.37734    <NA>
#> 55 46.38761 -84.23144    <NA>
#> 56 43.63956 -79.32485    <NA>
#> 57 45.49189 -84.07187    <NA>
#> 59 46.50095 -84.39186    <NA>
#> 60 43.64613 -79.36030    <NA>
#> 61 46.52558 -84.17251    <NA>
#> 63 46.50521 -84.35220    <NA>
#> 65 45.82494 -84.60678    <NA>
#> 66 46.25878 -84.10362    <NA>
#> 67 46.54254 -84.21432    <NA>
#> 68 46.47160 -84.29758    <NA>
#> 73 43.00502 -82.41486    <NA>
#> 76 43.63719 -79.39152    <NA>
#> 77 46.50811 -84.34101    <NA>
#> 79 46.31807 -84.11548    <NA>
#> 81 45.06519 -83.43151    <NA>
#> 82 43.62240 -79.37472    <NA>
#> 85 43.64524 -79.34102    <NA>
#> 86 46.28645 -84.21152    <NA>
#> 87 43.63209 -79.39813    <NA>
#> 88 43.73778 -82.54340    <NA>
#> 19 42.21130 -83.12282      23
#> 20 42.09358 -83.14240      23
#> 22 42.34595 -82.95458      23
#> 35 45.50038 -83.90513     153
#> 58 44.45158 -83.24894     153
#> 62 45.33992 -83.44845     153
#> 64 41.63733 -82.97433      22
#> 69 44.09220 -83.38488     153
#> 70 44.13973 -83.26330     153
#> 71 42.60437 -82.62050      23
#> 72 42.74667 -82.47673      23
#> 74 43.61144 -83.86945     153
#> 75 43.37418 -83.99795     153
#> 78 44.71326 -83.20107     153
#> 80 44.95102 -83.29738     153
#> 83 41.62433 -83.01284      22
#> 84 43.38858 -83.98910     153
#> 51 41.59606 -83.58600   22 23
```
