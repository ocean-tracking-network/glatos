#' Modify playback time of video
#'
#' Speed up or slow down playback of video
#'
#' @param scale_factor multiplicative factor changes duration of video playback.
#'   See details.
#'
#' @param input character, path to video file (any file type supported by
#'   [av::av_encode_video][av_encode_video]; e.g., *.mp4, *.wmv, etc)
#'
#' @param output_dir character, output directory, default is working directory
#'
#' @param output character, output file name
#'
#' @param overwrite logical, default is `overwrite = TRUE`
#'
#' @param diagnostic_mode Logical (default = FALSE). If true, returns FFMPEG
#'   output.
#'
#' @note Input argument 'ffmpeg' was removed in glatos version 0.7.0.
#'
#' @details A simple wrapper for [av::av_encode_video][av_encode_video].
#'
#' @details \code{adjust_playback_time} adjusts playback speed of video.
#'   \code{scale_factor} controls the magnitude of speed-up or slow-down by
#'   modifying the presentation timestamp of each video frame. Values of
#'   \code{scale_factor} < 1 speed up playback and > 1 slow down playback. In
#'   addition to changing playback, function can change output format by
#'   specifying a different file extension in \code{output}.
#'
#' @return One video animation will be written to \code{output_dir} and the
#'   path and name of output file with be returned.
#'
#' @author Todd Hayden, Tom Binder, Chris Holbrook
#'
#' @examples
#' \dontrun{
#'
#' # load example frames
#' frames <- system.file("extdata", "frames", package = "glatos")
#'
#' # make video animation
#' out_file <- file.path(tempdir(), "animation_av.mp4")
#' make_video(
#'   input_dir = frames,
#'   input_ext = ".png",
#'   output = out_file
#' )
#'
#' # slow video down by a factor of 10
#' path <- file.path(tempdir(), "animation_av.mp4")
#' adjust_playback_time(
#'   scale_factor = 10,
#'   input = path,
#'   output_dir = tempdir(),
#'   output = "animation_av_slow.mp4",
#'   diagnostic_mode = FALSE,
#'   overwrite = TRUE
#' )
#'
#' # slow video down by a factor of 10 and change format of output video
#' adjust_playback_time(
#'   scale_factor = 10,
#'   input = path,
#'   output_dir = tempdir(),
#'   output = "animation_av_slow.wmv",
#'   diagnostic_mode = FALSE,
#'   overwrite = TRUE
#' )
#'
#' # speed up video
#' adjust_playback_time(
#'   scale_factor = 0.5,
#'   input = path,
#'   output_dir = tempdir(),
#'   output = "animation_av_fast.mp4",
#'   diagnostic_mode = FALSE,
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
adjust_playback_time <- function(
    scale_factor = 1,
    input,
    output_dir = getwd(),
    output = "new.mp4",
    overwrite = FALSE,
    diagnostic_mode = FALSE) {
  output_file <- normalizePath(file.path(output_dir, output), mustWork = FALSE)

  ffcall <- sprintf("setpts=%f*PTS", scale_factor)

  # check if output file exists
  if (file.exists(output_file) & overwrite == FALSE) {
    warning(
      "No video file written because output file already exists and ",
      "overwrite = FALSE."
    )
    return()
  }

  vid <- av::av_encode_video(
    input = input,
    output = output_file,
    vfilter = ffcall,
    verbose = diagnostic_mode
  )

  return(vid)
}
