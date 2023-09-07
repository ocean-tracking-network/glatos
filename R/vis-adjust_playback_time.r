##' Modify playback time of video
##'
##' Speed up or slow down playback of video
##' 
##' @param scale_factor multiplicative factor changes duration of video playback. See details.
##' @param input character, path to video file (any FFmpeg supported file type- *.mp4, *.wmv, etc)
##' @param output_dir character, output directory, default is working directory
##' @param output character, output file name
##' @param overwrite logical, default is `overwrite = TRUE`
##' @param ffmpeg NOTE: argument ignored. Not needed for package versions greater than 0.4.0.  Argument will be removed in a future version.
##' @param diagnostic_mode Logical (default = FALSE). If true, returns FFMPEG output.
##'  
##' @details `adjust_playback_time` adjusts playback speed of
##'   video.  `scale_factor` controls the magnitude of speed-up
##'   or slow-down by modifying the presentation timestamp of each
##'   video frame. For more information, see 
##'   [How to speed up/slow down a video](https://trac.ffmpeg.org/wiki).  
##'   Values of `scale_factor` < 1 speed up playback and >1 slow
##'   down video. In addition to changing playback, function can
##'   change output format by specifying a different file extension in
##'   `output`.
##' 
##' @return One video animation will be written to `output_dir`
##'
##' @author Todd Hayden, Tom Binder, Chris Holbrook
##'
##' @examples
##' 
##' \dontrun{
##' 
##' # load example frames 
##' frames <- system.file("extdata", "frames", package = "glatos")
##'
##' # make video animation
##' make_video(input_dir = frames, pattern = "%02d.png", output = "animation_av.mp4")
##'
##' # slow video down by a factor of 10 
##' path <- file.path(getwd(), "animation_av.mp4")
##' adjust_playback_time(scale_factor = 10, input = path, diagnostic = FALSE, overwrite = TRUE)
##'
##' # slow video down by a factor of 10 and change format of output video
##' adjust_playback_time(scale_factor = 10, input = path, output = "slow.wmv", diagnostic = FALSE)
##' 
##' # speed up video 
##' adjust_playback_time(scale_factor=0.5, input = path, output = "faster.mp4")
##' }
##' @export
##'

adjust_playback_time <- function(scale_factor = 1,
                                 input, 
                                 output_dir = getwd(),
                                 output = "new.mp4",
                                 overwrite = FALSE,
                                 ffmpeg = NA,
                                 diagnostic_mode = FALSE){
 
  output_file <- file.path(output_dir, output)
  ffcall <- sprintf("setpts=%f*PTS", scale_factor)

  #check if output file exists
  if(file.exists(output_file) & overwrite == FALSE) {
    warning("No video file written because output file already exists and ",
      "overwrite = FALSE.")
    return()
  }
  
  foo <- av_encode_video(input = input, output = output_file, vfilter = ffcall, verbose = diagnostic_mode)
  
  return(foo)

}
