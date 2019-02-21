##' Modify playback time of video
##'
##' Speed up or slow down playback of video using FFmpeg software.
##' 
##' @param scale_factor multiplicative factor changes duration of
##'   video playback. See details.
##'
##' @param input character, path to video file (any FFmpeg supported
##'   file type- *.mp4, *.wmv, see \link{make_frames})
##'
##' @param output_dir character, output directory, default is working
##'   directory
##'
##' @param output character, output file name
##' 
##' @param overwrite logical, default is \code{overwrite = TRUE}
##'
##' @param ffmpeg A file path (characer) to FFmpeg executable. This
##'   argument is only needed if ffmpeg is not added to your system
##'   path. For Windows machines, path must point to 'ffmpeg.exe',
##'   located in the bin subfolder within the ffmpeg folder.  For
##'   example on Windows machines,
##'   "C:\\Users\\Username\\Documents\\ffmpeg-3.4.1-win64-static\\bin\\ffmpeg.exe").
##'   On Mac, path must point to 'ffmpeg' within the 'bin'
##'   subfolder "/home/directory/Documents/bin/ffmpeg".  see \link{make_video}
##'
##' @details \code{adjust_playback_time} adjusts playback speed of
##'   video.  \code{scale_factor} controls the magnitude of speed-up
##'   or slow-down by modifying the presentation timestamp of each
##'   video frame. For more information, see 
##'   \href{https://trac.ffmpeg.org/wiki}{How to speed up/slow down a video}.  
##'   Values of \code{scale_factor} < 1 speed up playback and >1 slow
##'   down video. In addition to changing playback, function can
##'   change output format by specifying a different file extension in
##'   \code{output}.
##' 
##' @return One video animation will be written to \code{output_dir}
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
##' make_video(dir = frames, pattern = "%02d.png", output = "animation.mp4" )
##'
##' # slow video down by a factor of 10 
##' path <- file.path(getwd(), "animation.mp4")
##' adjust_playback_time(scale_factor = 10, input = path)
##'
##' # slow video down by a factor of 10 and change format of output video
##' adjust_playback_time(scale_factor = 10, input = path, output = "slow.wmv")
##' 
##' # speed up video 
##' adjust_playback_time(scale_factor=0.5, input = path, output = "faster.mp4")
##' }
##' @export
##'


adjust_playback_time <- function(scale_factor = 1,
                                 input, output_dir = getwd(),
                                 output = "new.mp4",
                                 overwrite = FALSE,
                                 ffmpeg = NA ){

# try calling ffmpeg
    cmd <- ifelse(is.na(ffmpeg), 'ffmpeg', ffmpeg)	
    ffVers <- suppressWarnings(system2(cmd, "-version", stdout=F)) #call ffmpeg
    if(ffVers == 127)
      stop(paste0('"ffmpeg.exe" was not found.\n',
                  'Ensure it is installed add added to system PATH variable\n',
                  "or specify path using input argument 'ffmpeg'\n\n",
                  'FFmpeg is available from:\n https://ffmpeg.org/\n',
                  'You may create the individual frames and then combine them\n',
                  'into an animation manually using video editing software\n', 
                  '(e.g., Windows Movie Maker or iMovie) by setting the animate\n',
                  'argument to FALSE.'),
           call. = FALSE)
  input <- shQuote(input)
  out <- shQuote(file.path(output_dir, output))
  ffcall <- sprintf('-i %s -filter:v "setpts=%f*PTS" %s %s', input,
                    scale_factor, out, (ifelse(overwrite, "-y", "-n")))
  system2(cmd, ffcall, stdout = FALSE)

}
