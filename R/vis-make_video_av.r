##' Create video from sequence of still images 
##'
##' Stitch a sequence of images into a video animation using R package "av"
##' 
##' @param dir directory containing images, default is working
##'   directory.
##' @param ext character, file extension of images to be stitched into
##'   a video.
##' @param output character, output video file name.  See details.
##' @param fps_in integer, intended framerate of input image sequence
##'   in frames per second.
##' @param start_frame integer, start frame. Defaults to
##'   \code{start=1}.
##' @param end_frame integer, end frame.  Defaults to \code{end_frame
##'   = NULL}.
##' @param codec character, video codec used.
##' @param verbose Logical (default = FALSE). If true, returns details
##'   of processing.
##' @param audio audio or video input file with sound for the output
##'   video.
##' @param vfilter a string defining an ffmpeg filter.  This is the
##'   same parameter as the -vf argument in FFmpeg.
##'
##' @details This function stitches a sequence of images into a video
##'   without an external installation of FFmpeg software.
##'   \code{make_video_av} is a simple wrapper of
##'   \code{av_encode_video}.  More information about the \code{av}
##'   package is available at
##'   https://cran.r-project.org/web/packages/av/index.html and
##'   https://docs.ropensci.org/av/.
##'
##' @details The argument \code{codec} defaults to \code{libx264} for
##'   most formats which is usually the best choice.  More information
##'   about other available codecs can be found by running
##'   \code{av::av_encoders}.  The argument \code{vfilter} is used to
##'   specify FFmpeg filters to modify video output.
##'   \code{av::av_filters()} function lists the possible filters
##'   available.  More information and examples of using filters to
##'   manipulate video output can be found at
##'   https://trac.ffmpeg.org/wiki/FilteringGuide#Examples
##'
##' @details A directory of sequenced image files (.png, .jpeg) are
##'   passed to \code{dir} argument.  The \code{ext} argument
##'   specifies the type of files to be stitched into a video.  The
##'   images passed to the function must all have the same size,
##'   height, and format.
##'
##' @details Function can create .mp4, .mov, .mkv, .flv .wmv, or
##'   .mpeg animations.  Format of created animation is determined by
##'   file extension of \code{output}.
##'
##' @details \code{make_video_av} allows user to specify input
##'   framerate \code{fps_in} of image sequence and specify the
##'   starting and ending frames (\code{start_frame},
##'   \code{end_frame}).  If specified, only images within range are
##'   used in animation and all other frames are skipped.
##'
##' @details
##'
##' @return One video animation will be written to \code{output_dir}
##' 
##' @author Todd Hayden, Tom Binder, Chris Holbrook
##'
##' @examples
##'
##' \dontrun{
##' 
##' # load frames
##' frames <- system.file("extdata", "frames", package = "glatos")
##'
##' # make .mp4 video
##' make_video_av(dir = frames, ext = ".png", output = "animation1.mp4", verbose = FALSE)
##'
##' # make .wmv video
##' make_video_av(dir=frames, ext = ".png", output = "animation2.wmv", verbose = TRUE)
##'
##' # start animation on frame 10, end on frame 20
##' make_video_av(dir=frames, ext = ".png", start_frame = 10, 
##'            end_frame = 20, output = "animation_3.mp4")
##'
##'# make move backwards- start animation of frame 20 and end on frame 10
##' make_video_av(dir=frames, ext = ".png", start_frame = 20, 
##'            end_frame = 10, output = "animation_4.mp4")
##'
##' # resize output video by specifying a scale filter
##' make_video_av(dir=frames, ext= ".png", vfilter = "scale=320:240",
##' output = "animation_5.mp4" )
##'
##' # slow the video by 10 times
##' make_video_av(dir=frames, ext= ".png", vfilter = "setpts=10*PTS",
##' output = "animation_6.mp4" )
##'
##' # slow video by 10 times and scale to 320x240 resolution
##' make_video_av(dir=frames, ext = ".png",
##' vfilter = "scale=320:240, setpts=10*PTS", output = "animation_7.mp4")
##'
##' # slow video by 10 times and smooth movement
##' make_video_av(dir=frames, ext = ".png", vfilter = "minterpolate='mi_mode=mci:mc_mode=aobmc:vsbmc=1:fps=120', setpts=10*PTS", output = "animation_8.mp4")
##'
##' # change input framerate
##' make_video_av(dir=frames, ext = ".png", fps_in = 5,
##' output = "animation_9.mp4")
##'
##' }
##'
##' @export

make_video_av <- function(...){
  
  # makes vector of images
  ext <- paste0("*", ext)
  files <- list.files(dir, full.names = TRUE, pattern = ext)

  #check if dir exists
  if(!dir.exists(dir)) stop(paste0("Input dir '", dir , "' not found."), 
                            .call = FALSE)
  
  # subset out frames if needed
  if(is.null(end_frame)){end_frame <- length(files)}
   fls <- files[start_frame:end_frame]

  if(is.null(vfilter)){vfilt <- "null"} else {vfilt <- vfilter}
  
  
  av::av_encode_video(input = fls, output = output, framerate = fps_in, vfilter = vfilt, codec = codec, verbose = verbose, audio = audio)

    message("Video file written to ", output, ".")
}


