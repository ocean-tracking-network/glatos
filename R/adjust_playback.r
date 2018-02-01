##' Modify playback time of video
##'
##' Speed up or slow down playback of video using FFmpeg software.
##' 
##' @param scale_factor multiplicative factor changes duration of
##'   video playback. See details.
##' @param input character, path to video file
##' @param output_dir character, output directory, default is working
##'   directory
##' @param output character, output file name
##' @param overwrite logical, default is \code{TRUE}
##' @param ffmpeg A character string with path to install directory
##'   for ffmpeg.  This argument is only needed if ffmpeg has not been
##'   added to your path variable on your computer.  For Windows
##'   machines, path must point to ffmpeg.exe, typically located in
##'   bin folder.  For example,
##'
##' @details \code{adjust_playback_time} is a helper function that adjusts
##'   duration of video.  \code{scale_factor} controls the magnitude
##'   of speed-up or slow-down by modifying the presentation timestamp
##'   of each video frame. For more information, see
##'   Values of \code{scale_factor} < 1 speed up playback and >1 slow
##'   down video.
##'
##'
##' 
##' @return One video animation will be written to \code{output_dir} 
##'
##' @author Todd Hayden
##'
##' @export
##'

adjust_playback_time <- function(scale_factor = 1, input, output_dir = ".", output = "test.mp4", overwrite = FALSE, ffmpeg = NA ){

# try calling ffmpeg

  # add exe if ffmpeg is directory
  cmd <- ifelse(grepl("ffmpeg.exe$",ffmpeg) | is.na(ffmpeg), ffmpeg, 
    paste0(ffmpeg,"\\ffmpeg.exe"))
  cmd <- ifelse(is.na(ffmpeg), 'ffmpeg', cmd)
  ffVers <- suppressWarnings(system2(cmd, "-version",stdout=F)) #call ffmpeg
  if(ffVers == 127) stop(paste0('"ffmpeg.exe" was not found.\n',
    'Ensure it is installed add added to system PATH variable\n',
    "or specify path using input argument 'ffmpeg'\n\n",
    'FFmpeg is available from:\n https://ffmpeg.org/'), call.=FALSE)


    ffcall <- sprintf('-i %s -filter:v "setpts=%f*PTS" %s/%s %s', input, scale_factor, output_dir, output, (ifelse(overwrite, "-y", "-n")))


    system2(cmd, ffcall, stdout = FALSE)

}
