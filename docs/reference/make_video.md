# Create video from sequence of still images

Stitch a sequence of images into a video animation. A simple wrapper for
[av::av_encode_video](https://docs.ropensci.org/av//reference/encoding.html).

## Usage

``` r
make_video(
  input_dir = getwd(),
  input_ext = ".png",
  output = "animation.mp4",
  duration = NULL,
  start_frame = 1,
  end_frame = NULL,
  size = NULL,
  overwrite = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- input_dir:

  directory containing images, default is working directory.

- input_ext:

  character, file extension of images to be stitched into a video. All
  images must have same extension, width, and height. Each imaged will
  be positioned in the video in alphabetical order by image file name.

- output:

  character, output video file name. See details.

- duration:

  integer, output video duration in seconds. If NULL (default) then this
  will be determined by the number of input frames and the framerate
  (default is 24 frames per second). E.g., a video containing 240 frames
  at default 24 fps will be 10 seconds long. See details.

- start_frame:

  integer, start frame. Defaults to `start=1`.

- end_frame:

  integer, end frame. Defaults to `end_frame = NULL` (i.e., last frame).

- size:

  integer vector with width and height of output video in pixels.
  Ignored if `vfilter` is passed via `...`.

- overwrite:

  logical, overwrite existing output file? (default = FALSE)

- verbose:

  logical, show output from
  [av::av_encode_video](https://docs.ropensci.org/av//reference/encoding.html)?
  Default = FALSE.

- ...:

  optional arguments passed to
  [av::av_encode_video](https://docs.ropensci.org/av//reference/encoding.html).
  Such as framerate, vfilter, codec.

## Value

One video animation will be written disk and the path and file name will
be returned.

## Details

This function was overhauled in glatos v 0.4.1 to simplify inputs and to
no longer require an external program (ffmpeg.exe). As a result input
arguments have changed, as described above. Starting with glatos v
0.7.0, any calls to `make_video` using the arguments from glatos v 0.4.0
or earlier will fail.

`make_video` is a simple wrapper of
[av::av_encode_video](https://docs.ropensci.org/av//reference/encoding.html).
It is intended to allow creation of videos from images (frames) created
by
[glatos::make_frames](https://ocean-tracking-network.github.io/glatos/reference/make_frames.md)
as simple as possible. More advanced features of `av`, can be used by
including any argument of
[av::av_encode_video](https://docs.ropensci.org/av//reference/encoding.html)
in the call to `make_video`, or by calling
[av::av_encode_video](https://docs.ropensci.org/av//reference/encoding.html)
directly. More information about the `av` package is available at
https://cran.r-project.org/web/packages/av/index.html and
https://docs.ropensci.org/av/.

A directory of sequenced image files (.png, .jpeg) are passed to
`input_dir` argument. The `input_ext` argument specifies the type of
files to be stitched into a video. The images passed to the function
must all have the same size, height, and format.

Function can create .mp4, .mov, .mkv, .flv .wmv, or .mpeg animations.
Format of created animation is determined by file extension of `output`.

If `start_frame` or `end_frame` are specified, then only frames within
the specified range will be included in the output video.

If `duration` is specified, then the output framerate will be determined
by the number of input frames and the framerate (default is 24 frames
per second). E.g., a video of 10 second duration containing 240 frames
will have an output frame rate of 24 fps. In some cases (when number of
frames is small) the number of frames may not divide evently into the
specified duration, so the output duration may differ from that
specified. If the output frame rate exceeds 30 fps, then a warning will
alert the user that some individual frame content may not be visible to
users. Video duration may also be controlled by setting the `framerate`
argument of
[av::av_encode_video](https://docs.ropensci.org/av//reference/encoding.html).
See `...` above.

## Author

Todd Hayden, Chris Holbrook

## Examples

``` r
if (FALSE) { # \dontrun{

# load frames
frames <- system.file("extdata", "frames", package = "glatos")

# make .mp4 video
make_video(
  input_dir = frames,
  input_ext = ".png",
  output = file.path(tempdir(), "animation1.mp4")
)

# set duration to 10 seconds
make_video(
  input_dir = frames,
  input_ext = ".png",
  output = file.path(tempdir(), "animation2.mp4"),
  duration = 10
)

# set size of ouput video
make_video(
  input_dir = frames,
  input_ext = ".png",
  output = file.path(tempdir(), "animation3.mp4"),
  size = c(320, 240)
)

# start animation on frame 10, end on frame 20
make_video(
  input_dir = frames,
  input_ext = ".png",
  output = file.path(tempdir(), "animation_4.mp4"),
  start_frame = 10,
  end_frame = 20
)

# make move backwards- start animation of frame 20 and end on frame 10
make_video(
  input_dir = frames,
  input_ext = ".png",
  output = file.path(tempdir(), "animation_5.mp4"),
  start_frame = 20,
  end_frame = 10
)

# make .wmv video
make_video(
  input_dir = frames,
  input_ext = ".png",
  output = file.path(tempdir(), "animation1.wmv")
)


#--- Examples using more advanced features of av_encode_video

# resize output video by specifying a scale filter
make_video(
  input_dir = frames,
  input_ext = ".png",
  output = file.path(tempdir(), "animation_6.mp4"),
  vfilter = "scale=320:240"
)

# slow the video by 10 times
make_video(
  input_dir = frames,
  input_ext = ".png",
  output = file.path(tempdir(), "animation_7.mp4"),
  vfilter = "setpts=10*PTS"
)

# slow video by 10 times and scale to 320x240 resolution
make_video(
  input_dir = frames,
  input_ext = ".png",
  output = file.path(tempdir(), "animation_8.mp4"),
  vfilter = "scale=320:240, setpts=10*PTS"
)
} # }
```
