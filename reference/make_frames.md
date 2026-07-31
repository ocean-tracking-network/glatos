# Create an animated video of spatiotemporal path data

Create a set of frames (png image files) showing geographic location
data (e.g., detections of tagged fish or interpolated path data) at
discrete points in time on top of a Great Lakes shapefile and optionally
stitches frames into a video animation (mp4 file).

## Usage

``` r
make_frames(
  proc_obj,
  recs = NULL,
  out_dir = getwd(),
  background_ylim = c(41.3, 49),
  background_xlim = c(-92.45, -75.87),
  show_interpolated = TRUE,
  tail_dur = 0,
  animate = TRUE,
  ani_name = "animation.mp4",
  frame_delete = FALSE,
  overwrite = FALSE,
  preview = FALSE,
  bg_map = NULL,
  show_progress = TRUE,
  ...
)
```

## Arguments

- proc_obj:

  A data frame created by
  [`interpolate_path()`](https://github.io/reference/interpolate_path.md)
  function or a data frame containing 'animal_id', 'bin_timestamp',
  'latitude', 'longitude', and 'record_type'

- recs:

  An optional data frame containing at least four columns with receiver
  'deploy_lat', 'deploy_long', 'deploy_date_time', and
  'recover_date_time'. Other columns in object will be ignored. Default
  column names match GLATOS standard receiver location file (e.g.,
  'GLATOS_receiverLocations_yyyymmdd.csv').

- out_dir:

  A character string with file path to directory where individual frames
  for animations will be written. Default is working directory.

- background_ylim:

  Vector of two values specifying the min/max values for y-scale of
  plot. Units are degrees.

- background_xlim:

  Vector of two values specifying the min/max values for x-scale of
  plot. Units are degrees.

- show_interpolated:

  Boolean. Default (TRUE) include interpolated points.

- tail_dur:

  contains the duration (in same units as `proc_obj$bin_timestamp`; see
  [`interpolate_path()`](https://github.io/reference/interpolate_path.md))
  of trailing points in each frame. Default value is 0 (no trailing
  points). A value of `Inf` will show all points from start.

- animate:

  Boolean. Default (TRUE) creates video animation by calling
  [`make_video()`](https://github.io/reference/make_video.md) with
  `output = ani_name`. Default values are used for all other arguments.
  See Details below.

- ani_name:

  Character string with name and extension of animation output video
  file. Full path is optional. If file name only (no path), then the
  output video is written to 'out_dir' (same as images). To write to
  working directory, use "./" prefix (e.g.,
  `ani_name = "./animation.mp4"`. If `animate = TRUE`, the path and
  filename are passed to
  [`make_video()`](https://github.io/reference/make_video.md).

- frame_delete:

  Boolean. Default (`frame_delete = TRUE`) delete individual image
  frames after animation is created

- overwrite:

  Overwite the animation (output video) file if it already exists.
  Default (`overwrite = FALSE`) prevents file from being overwritten and
  will result in error if the file exists. Passed to
  [`make_video()`](https://github.io/reference/make_video.md) if
  `animate = TRUE`.

- preview:

  write first frame only. Useful for checking output before processing
  large number of frames. Default `preview = FALSE`

- bg_map:

  A sf points, lines, or polygons object. Spatial `sp` or `terra`
  objects will be converted to `sf`. Coordinate system of map must be
  latitude/longitude (WGS 84).

- show_progress:

  Logical. Progress bar and status messages will be shown if TRUE
  (default) and not shown if FALSE.

- ...:

  Optional graphing parameters for customizing elements of fish location
  points, receiver location points, timeline, and slider (moves along
  the timeline). See also **Details** and **Note** sections.

## Value

Sequentially-numbered png files (one for each frame) and one mp4 file
will be written to `out_dir`.

## Details

***To customize fish location points (from `proc_obj`):*** Add any
argument that can be passed to
[points](https://rdrr.io/r/graphics/points.html). The following values
will create the default plot:

- `cex`: symbol size; default = 2

- `col`: symbol color; default = "blue"

- `pch`: symbol type; default = 16

***To customize receiver location points (from `recs`):*** Add prefix
`recs.` to any argument that can be passed to
[points](https://rdrr.io/r/graphics/points.html). The following values
will create the default plot:

- `recs.cex`: symbol size; default = 1.5

- `recs.pch`: symbol type; default = 16

***To customize timeline:*** Add add prefix `timeline.` to any argument
of [axis](https://rdrr.io/r/graphics/axis.html). Note all elements of
the timeline except the sliding symbol (see 'slider' below) are created
by a call to `axis`. The following values will create the default plot:

- `timeline.at`: a sequence with locations of labels (with first and
  last being start and end) along x-axis; in units of longitude; by
  default this will center the timeline with five equally-spaced labels
  in the middle 80% of background_xlim.

- `timeline.pos`: location along the y-axis; in units of latitude; by
  default this will place the timeline up from the bottom 6% of the
  range of `background_ylim`

- `timeline.labels`: text used for labels; default =
  `format(labels, "\%Y-\%m-\%d")`, where labels are values of
  proc_obj\$bin_timestamp

- `timeline.col`: color of line; default = "grey70"

- `timeline.lwd`: width of line; default = 20 times the aspect ratio of
  the plot device

- `timeline.cex.axis`: size of labels; default = 2

***To customize time slider (symbol that slides):*** Add prefix
`timeline.` to any argument that can be passed to
[points](https://rdrr.io/r/graphics/points.html). The following values
will create the default plot:

- `timeslider.bg`: a single value with symbol bg color; default =
  "grey40"

- `timeslider.cex`: a single value with symbol size; default = 2

- `timeslider.col`: a single value with symbol type; default = "grey20"

- `timeslider.pch`: a single value with symbol type; default = 21

***To customize parameters controlled by `par`:*** Add prefix `par.` to
any argument that can be passed to
[par](https://rdrr.io/r/graphics/par.html). Note that `par.mar` controls
whitespace behind default timeslider. The following values will create
the default plot:

- `par.oma`: plot outer margins; default = c(0,0,0,0)

- `par.mar`: plot inner margins; default = c(6,0,0,0)

If `animate = TRUE` then the animation output file name (`ani_name`
argument) will be passed to the `output` argument in
[`make_video()`](https://github.io/reference/make_video.md). Default
values for all other
[`make_video()`](https://github.io/reference/make_video.md) arguments
will be used. Note that the default frame rate is 24 frames per second
(`framerate` argument in
[`make_video()`](https://github.io/reference/make_video.md)), which will
determine the total length (duration) of the output video. For example,
a video containing 240 images (frames) will run for 10 seconds at these
default parameters. Note that output video duration, dimensions (size),
and other ouput video characteristics can be modified by calling
[`make_video()`](https://github.io/reference/make_video.md) directly. To
do this, set `animate = FALSE` and then use
[`make_video()`](https://github.io/reference/make_video.md) to create a
video from the resulting set of images.

## Note

*Customizing plot elements with input argument `...`* The option to
allow customization of plot elements with input argument `...` provides
a great deal of flexibility, but users will need to be familiar with
each associated graphics functions (e.g.,
[axis](https://rdrr.io/r/graphics/axis.html) for timeline arguments). We
expect that this will require some trial and error and that input
argument `preview = TRUE` will be useful while exploring optional plot
arguments.

## Author

Todd Hayden, Tom Binder, Chris Holbrook

## Examples

``` r
if (FALSE) { # \dontrun{

# load detection data
det_file <- system.file("extdata", "walleye_detections.csv",
  package = "glatos"
)
dtc <- read_glatos_detections(det_file)

# take a look
head(dtc)

# load receiver location data
rec_file <- system.file("extdata",
  "sample_receivers.csv",
  package = "glatos"
)
recs <- read_glatos_receivers(rec_file)

# call with defaults; linear interpolation
pos1 <- interpolate_path(dtc)

# make frames, preview the first frame
myDir <- paste0(getwd(), "/frames1")
make_frames(pos1, recs = recs, out_dir = myDir, preview = TRUE)

# make frames but not animation
myDir <- paste0(getwd(), "/frames2")
make_frames(pos1, recs = recs, out_dir = myDir, animate = FALSE)

# make sequential frames, and animate.  Make animation and frames.
# change default color of fish markers to red and change marker and size.

myDir <- paste0(getwd(), "/frames3")
make_frames(pos1,
  recs = recs, out_dir = myDir, animate = TRUE,
  ani_name = "animation3.mp4", col = "red", pch = 16, cex = 3
)

# make sequential frames, animate, add 5-day tail
myDir <- paste0(getwd(), "/frames4")
make_frames(pos1,
  recs = recs, out_dir = myDir, animate = TRUE,
  ani_name = "animation4.mp4", tail_dur = 5
)

# make animation, remove frames.
myDir <- paste0(getwd(), "/frames5")
make_frames(pos1,
  recs = recs, out_dir = myDir, animate = TRUE,
  ani_name = "animation5.mp4", frame_delete = TRUE
)
} # }
```
