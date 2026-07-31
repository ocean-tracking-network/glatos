# Modify playback time of video

Speed up or slow down playback of video

## Usage

``` r
adjust_playback_time(
  scale_factor = 1,
  input,
  output_dir = getwd(),
  output = "new.mp4",
  overwrite = FALSE,
  diagnostic_mode = FALSE
)
```

## Arguments

- scale_factor:

  multiplicative factor changes duration of video playback. See details.

- input:

  character, path to video file (any file type supported by
  [av::av_encode_video](https://docs.ropensci.org/av//reference/encoding.html);
  e.g., \*.mp4, \*.wmv, etc)

- output_dir:

  character, output directory, default is working directory

- output:

  character, output file name

- overwrite:

  logical, default is `overwrite = TRUE`

- diagnostic_mode:

  Logical (default = FALSE). If true, returns FFMPEG output.

## Value

One video animation will be written to `output_dir` and the path and
name of output file with be returned.

## Details

A simple wrapper for
[av::av_encode_video](https://docs.ropensci.org/av//reference/encoding.html).

`adjust_playback_time` adjusts playback speed of video. `scale_factor`
controls the magnitude of speed-up or slow-down by modifying the
presentation timestamp of each video frame. Values of `scale_factor` \<
1 speed up playback and \> 1 slow down playback. In addition to changing
playback, function can change output format by specifying a different
file extension in `output`.

## Note

Input argument 'ffmpeg' was removed in glatos version 0.7.0.

## Author

Todd Hayden, Tom Binder, Chris Holbrook

## Examples

``` r
if (FALSE) { # \dontrun{

# load example frames
frames <- system.file("extdata", "frames", package = "glatos")

# make video animation
out_file <- file.path(tempdir(), "animation_av.mp4")
make_video(
  input_dir = frames,
  input_ext = ".png",
  output = out_file
)

# slow video down by a factor of 10
path <- file.path(tempdir(), "animation_av.mp4")
adjust_playback_time(
  scale_factor = 10,
  input = path,
  output_dir = tempdir(),
  output = "animation_av_slow.mp4",
  diagnostic_mode = FALSE,
  overwrite = TRUE
)

# slow video down by a factor of 10 and change format of output video
adjust_playback_time(
  scale_factor = 10,
  input = path,
  output_dir = tempdir(),
  output = "animation_av_slow.wmv",
  diagnostic_mode = FALSE,
  overwrite = TRUE
)

# speed up video
adjust_playback_time(
  scale_factor = 0.5,
  input = path,
  output_dir = tempdir(),
  output = "animation_av_fast.mp4",
  diagnostic_mode = FALSE,
  overwrite = TRUE
)
} # }
```
