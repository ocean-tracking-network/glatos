#'Create video from sequence of still images
#'
#'Stitch a sequence of images into a video animation. A simple wrapper for
#'[av::av_encode_video][av_encode_video].
#'
#'@param input_dir directory containing images, default is working directory.
#'
#'@param input_ext character, file extension of images to be stitched into a
#'  video. All images must have same extension, width, and height. Each imaged
#'  will be positioned in the video in alphabetical order by image file name.
#'
#'@param output character, output video file name. See details.
#'
#'@param duration integer, output video duration in seconds. If NULL (default)
#'  then this will be determined by the number of input frames and the
#'  framerate (default is 24 frames per second). E.g., a video containing 240
#'  frames at default 24 fps will be 10 seconds long. See details.
#'
#'@param start_frame integer, start frame. Defaults to \code{start=1}.
#'
#'@param end_frame integer, end frame.  Defaults to \code{end_frame = NULL}
#'  (i.e., last frame).
#'
#'@param size integer vector with width and height of output video in pixels.
#'  Ignored if \code{vfilter} is passed via \code{...}.
#'
#'@param overwrite logical, overwrite existing output file? (default = FALSE)
#'
#'@param verbose logical, show output from
#'  [av::av_encode_video][av_encode_video]? Default = FALSE.
#'
#'@param ... optional arguments passed to
#'  [av::av_encode_video][av_encode_video]. Such as framerate, vfilter, codec.
#'
#'@details This function was overhauled in glatos v 0.4.1 to simplify inputs
#'  and to no longer require an external program (ffmpeg.exe). As a result
#'  input arguments have changed, as described above. Starting with glatos v
#'  0.7.0, any calls to \code{make_video} using the arguments from glatos v
#'  0.4.0 or earlier will fail.
#'
#'@details \code{make_video} is a simple wrapper of
#'  [av::av_encode_video][av_encode_video]. It is intended to allow creation of
#'  videos from images (frames) created by [glatos::make_frames][make_frames]
#'  as simple as possible. More advanced features of \code{av}, can be used by
#'  including any argument of [av::av_encode_video][av_encode_video] in the
#'  call to \code{make_video}, or by calling
#'  [av::av_encode_video][av_encode_video] directly. More information about the
#'  \code{av} package is available at
#'  https://cran.r-project.org/web/packages/av/index.html and
#'  https://docs.ropensci.org/av/.
#'
#'@details A directory of sequenced image files (.png, .jpeg) are passed to
#'  \code{input_dir} argument. The \code{input_ext} argument specifies the
#'  type of files to be stitched into a video. The images passed to the
#'  function must all have the same size, height, and format.
#'
#'@details Function can create .mp4, .mov, .mkv, .flv .wmv, or .mpeg
#'  animations.  Format of created animation is determined by file extension of
#'  \code{output}.
#'
#'@details If \code{start_frame} or \code{end_frame} are specified, then only
#'  frames within the specified range will be included in the output video.
#'
#'@details If \code{duration} is specified, then the output framerate will be
#'  determined by the number of input frames and the framerate (default is 24
#'  frames per second). E.g., a video of 10 second duration containing 240
#'  frames will have an output frame rate of 24 fps. In some cases (when number
#'  of frames is small) the number of frames may not divide evently into the
#'  specified duration, so the output duration may differ from that specified.
#'  If the output frame rate exceeds 30 fps, then a warning will alert the user
#'  that some individual frame content may not be visible to users. Video
#'  duration may also be controlled by setting the \code{framerate} argument of
#'  \link[=encoding]{av::av_encode_video}. See \code{...} above.
#'
#'@return One video animation will be written disk and the path and file name
#'  will be returned.
#'
#'@author Todd Hayden, Chris Holbrook
#'
#' @examples
#'
#' \dontrun{
#'
#' # load frames
#' frames <- system.file("extdata", "frames", package = "glatos")
#'
#' # make .mp4 video
#' make_video(input_dir = frames, 
#'            input_ext = ".png", 
#'            output = file.path(tempdir(), "animation1.mp4"))
#'
#' # set duration to 10 seconds
#' make_video(input_dir = frames, 
#'            input_ext = ".png", 
#'            output = file.path(tempdir(), "animation2.mp4"),
#'            duration = 10)
#'
#' # set size of ouput video
#' make_video(input_dir = frames, 
#'            input_ext = ".png", 
#'            output = file.path(tempdir(), "animation3.mp4"),
#'            size = c(320, 240))
#'
#' # start animation on frame 10, end on frame 20
#' make_video(input_dir = frames, 
#'            input_ext = ".png", 
#'            output = file.path(tempdir(), "animation_4.mp4"),
#'            start_frame = 10, 
#'            end_frame = 20)
#'
#'# make move backwards- start animation of frame 20 and end on frame 10
#' make_video(input_dir = frames, 
#'            input_ext = ".png", 
#'            output = file.path(tempdir(), "animation_5.mp4"),
#'            start_frame = 20, 
#'            end_frame = 10)
#'
#'# make .wmv video
#' make_video(input_dir = frames, 
#'            input_ext = ".png", 
#'            output = file.path(tempdir(), "animation1.wmv"))
#'
#'
#' #--- Examples using more advanced features of av_encode_video
#'
#' # resize output video by specifying a scale filter
#' make_video(input_dir = frames, 
#'            input_ext = ".png", 
#'            output = file.path(tempdir(), "animation_6.mp4"),
#'            vfilter = "scale=320:240")
#'
#' # slow the video by 10 times
#' make_video(input_dir = frames, 
#'            input_ext = ".png", 
#'            output = file.path(tempdir(), "animation_7.mp4"),
#'            vfilter = "setpts=10*PTS")
#'
#' # slow video by 10 times and scale to 320x240 resolution
#' make_video(input_dir = frames, 
#'            input_ext = ".png", 
#'            output = file.path(tempdir(), "animation_8.mp4"),
#'            vfilter = "scale=320:240, setpts=10*PTS")
#'
#' }
#'
#'@export
make_video  <- function(input_dir = getwd(),
                        input_ext = ".png",
                        output = "animation.mp4",
                        duration = NULL,
                        start_frame = 1,
                        end_frame = NULL,
                        size = NULL,
                        overwrite = FALSE,
                        verbose = FALSE,
                        ...){
  
  #capture input arguments from ellipses
  in_args <- list(...)
  #in_args <- list(framerate = 30)

  #check if input_dir exists
  if(!dir.exists(input_dir)) stop(paste0("Input dir '", input_dir , 
                                         "' not found."), .call = FALSE)
  
  #strip . from ext if present
  input_ext <- gsub("^\\.", "", input_ext)
  
  #make regex string
  ext_regex <- paste0(".", eval(input_ext), "$")
  
  #vector of input file names
  input_files <- list.files(input_dir, full.names = TRUE, pattern = ext_regex)
  
  # subset out frames if needed
  if(is.null(end_frame)) end_frame <- length(input_files)
  fls <- input_files[start_frame:end_frame]

  #get av-specific optional arguments and update av_args
  av_args = formals(av::av_encode_video)
  av_args_in <- in_args[intersect(names(in_args), names(av_args))]
  av_args[names(av_args_in)] <- av_args_in
  
  #set size only if vfilt not specified; otherwise ignore size  
  if(!is.null(size) & !("vfilter" %in% names(av_args_in))){
    av_args["vfilter"] <- paste0("scale=", size[1], ":", size[2])
  }
  
  #calculate duration if not given
  if(is.null(duration)) { 
    duration <- length(fls) / av_args$framerate 
  } else {
    av_args$framerate <- round(length(fls) / duration)
  }
  
  #update input, output, verbose
  av_args$input <- fls
  av_args$output <- output
  av_args$verbose <- verbose
  
  #enforce overwrite
  if(!overwrite & file.exists(av_args$output)) stop("Operation aborted ",
                                                    "because output file ",
                                                    "exists and 'overwrite = ",
                                                    "FALSE'.") 
  
  output_file <- do.call(av::av_encode_video, av_args)

  #warn if framerate is greater than 30
  if(av_args$framerate > 30) warning("Specified duration (", duration, 
    " seconds) results in a framerate (", av_args$framerate, 
    " fps) that may be too fast to see!")
                    
  return(output_file)
}



#' @export
make_video_ffmpeg  <- function(...){
  
  # FFmpeg is no longer needed.  Function is now defunct.
  .Defunct(new = "make_video", package = "glatos")

}
