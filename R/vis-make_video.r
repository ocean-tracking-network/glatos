##' Create video from sequence of still images 
##'
##' Stitch a sequence of images into a video animation. A simple wrapper for 
##' \link[=encoding]{av::av_encode_video}.
##' 
##' @param input_dir directory containing images, default is working
##'   directory.
##' @param input_ext character, file extension of images to be stitched into
##'   a video. All images must have same extension, width, and height. Each 
##'   imaged will be positioned in the video in alaphabetical order by image 
##'   file name. 
##' @param output character, output video file name.  See details.
##' @param duration integer, output video duration in seconds. If NULL (default) 
##'   then this will be determined by the number of input frames and 
##'   the framerate (default is 24 frames per second). E.g., a video containing 
##'   240 frames at default 24 fps will be 10 seconds long. See details.
##' @param start_frame integer, start frame. Defaults to \code{start=1}.
##' @param end_frame integer, end frame.  Defaults to \code{end_frame
##'   = NULL} (i.e., last frame).
##' @param size integer vector with width and height of output video in pixels.
##'   Ignored if \code{vfilter} is passed via \code{...}. 
##' @param overwrite logical, overwrite existing output file? (default = FALSE)
##' @param verbose logical, show output from
##'   \link[=encoding]{av::av_encode_video}? Default = FALSE.
##' @param ... optional arguments passed to
##'   \link[=encoding]{av::av_encode_video}. Such as framerate, vfilter, codec.
##'
##' @details This function was overhauled in glatos v 0.4.1 to simplify inputs
##'   and to no longer require an external program (ffmpeg.exe). As a result
##'   input arguments have changed, as described above. Starting with glatos v
##'   0.4.1, any calls to \code{make_video} using the arguments from glatos v
##'   0.4.0 or earlier will be redirected to \link[glatos]{make_video_ffmpeg}.
##'   Such redirection provides backward-compatibility but may be removed in a
##'   future version.
##'   
##' @details \code{make_video} is a simple wrapper of \code{av_encode_video}. It
##'   is intended to allow creation of videos from images (frames) created by
##'   \link[glatos]{make_frames} as simple as possible. More advanced features
##'   of \code{av}, can be used by including any argument of
##'   \link[=encoding]{av::av_encode_video} in the call to \code{make_video}, or
##'   by calling \link[=encoding]{av::av_encode_video} directly. More
##'   information about the \code{av} package is available at
##'   https://cran.r-project.org/web/packages/av/index.html and
##'   https://docs.ropensci.org/av/.
##'
##' @details A directory of sequenced image files (.png, .jpeg) are
##'   passed to \code{input_dir} argument.  The \code{input_ext} argument
##'   specifies the type of files to be stitched into a video.  The
##'   images passed to the function must all have the same size,
##'   height, and format.
##'
##' @details Function can create .mp4, .mov, .mkv, .flv .wmv, or
##'   .mpeg animations.  Format of created animation is determined by
##'   file extension of \code{output}.
##'   
##' @details If \code{start_frame} or \code{end_frame} are specified, then only 
##'   frames within the specified range will be included in the output video.
##'
##' @details If \code{duration} is specified, then the output framerate will be
##'   determined by the number of input frames and the framerate (default is 24
##'   frames per second). E.g., a video of 10 second duration containing 240
##'   frames will have an output frame rate of 24 fps. In some cases (when
##'   number of frames is small) the number of frames may not divide evently
##'   into the specified duration, so the output duration may differ from that
##'   specified. If the output frame rate exceeds 30 fps, then a warning will
##'   alert the user that some individual frame content may not be visible to
##'   users. Video duration may also be controlled by setting the
##'   \code{framerate} argument of \link[=encoding]{av::av_encode_video}. See
##'   \code{...} above.
##'
##' @return One video animation will be written disk and the path and file 
##'   name will be return.
##' 
##' @author Todd Hayden, Chris Holbrook
##'
##' @examples
##'
##' \dontrun{
##' 
##' # load frames
##' frames <- system.file("extdata", "frames", package = "glatos")
##'
##' # make .mp4 video
##' make_video(input_dir=frames, input_ext=".png", output="animation1.mp4")
##'
##' # set duration to 10 seconds
##' make_video(input_dir=frames, input_ext=".png", output="animation2.mp4",
##'            duration = 10)
##'            
##' # set size of ouput video
##' make_video(input_dir=frames, input_ext=".png", output="animation3.mp4",
##'            size = c(320, 240))
##'
##' # start animation on frame 10, end on frame 20
##' make_video(input_dir=frames, input_ext=".png", output="animation_4.mp4",
##'            start_frame = 10, end_frame = 20)
##'
##'# make move backwards- start animation of frame 20 and end on frame 10
##' make_video(input_dir=frames, input_ext=".png", output="animation_5.mp4",
##'            start_frame = 20, end_frame = 10)
##'
##'# make .wmv video
##' make_video(input_dir=frames, input_ext=".png", output="animation1.wmv")
##'
##'
##' #--- Examples using more advanced features of av_encode_video
##'
##' # resize output video by specifying a scale filter
##' make_video(input_dir=frames, input_ext=".png", output = "animation_6.mp4", 
##'            vfilter="scale=320:240")
##'
##' # slow the video by 10 times
##' make_video(input_dir=frames, input_ext=".png", output = "animation_7.mp4", 
##'            vfilter = "setpts=10*PTS")
##'
##' # slow video by 10 times and scale to 320x240 resolution
##' make_video(input_dir=frames, input_ext=".png", output = "animation_8.mp4", 
##'            vfilter = "scale=320:240, setpts=10*PTS")
##'
##' # slow video by 10 times and smooth movement
##' make_video(input_dir=frames, input_ext=".png", output = "animation_8.mp4", 
##'  vfilter = "minterpolate='mi_mode=mci:mc_mode=aobmc:vsbmc=1:fps=120', setpts=10*PTS")
##' 
##' }
##'
##' @export


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
  #in_args <- list(pattern = "%02d.png")
  #in_args <- list(framerate = 30)

  #Check for legacy version (make_video_ffmpeg) and redirect if needed
  
  #get legacy-version arguments from glatos v 0.4.0 or earlier...
  mvf_args <- formals(make_video_ffmpeg)
  mvf_args_in <- in_args[intersect(names(in_args), names(mvf_args))]
  
  if(length(mvf_args_in) > 0) {
    #add formal make_video args also in make_video_ffmpeg
    mv_args <- formals(make_video)
    both_args <- intersect(names(mv_args), names(mvf_args))
    #add non-missing legacy args to optional args
    if(!missing(overwrite)) in_args <- c(list(overwrite = overwrite), in_args) 
    if(!missing(size)) in_args <- c(list(size = size), in_args) 
    if(!missing(end_frame)) in_args <- c(list(end_frame = end_frame), in_args) 
    if(!missing(start_frame)) in_args <- c(list(start_frame = start_frame), in_args) 
    if(!missing(output)) in_args <- c(list(output = output), in_args) 
    
    return(do.call(make_video_ffmpeg, in_args))
  } 

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
  
  message("Video file written to ", output, ".")  
  
  #warn if framerate is greater than 30
  if(av_args$framerate > 30) warning("Specified duration (", duration, 
    " seconds) results in a framerate (", av_args$framerate, 
    " fps) that may be too fast to see!")
                    
  return(output_file)
}


##' Create video from sequence of still images using ffmpeg.exe
##'
##' Stitch a sequence of images into a video animation using FFmpeg
##' software. \strong{NOTE: This function is DEPRECATED and is being maintained 
##' temporarily for backward compatibility with glatos v. 0.4.0 and earlier.}
##' \emph{Use \link[glatos]{make_video} instead.} See details.
##' 
##' @param dir directory containing images, default is working
##'   directory.
##' @param pattern character, pattern for matching image file names.
##'   See details.
##' @param output character, output file name.  See details.
##' @param output_dir output directory, default is working directory, but will
##'   be created if it does not exist.
##' @param fps_in integer, intended framerate of input image sequence
##'   in frames per second.
##' @param start_frame integer, start frame. Defaults to
##'   \code{start=1}.
##' @param end_frame integer, end frame.  Defaults to \code{end_frame
##'   = NULL}.
##' @param size character, the dimensions of the video
##'   output. Defaults to \code{"source"}, which is equal to the
##'   dimensions of the input files. Otherwise scaling is performed on
##'   the output. See details.
##' @param preset character, encoding presets available in
##'   FFmpeg. Defaults to \code{medium}. See details.
##' @param codec character, video codec used.  See details.
##' @param format character, pixel format used. See details.
##' @param lossless logical, use lossless H.264 encoding if
##'   applicable.  Defaults to \code{FALSE}. See details.
##' @param fps_out integer, framerate of animation in frames per
##'   second.
##' @param overwrite logical, overwrite existing output file?
##'
##' @param ffmpeg A file path (characer) to FFmpeg executable. This
##'   argument is only needed if ffmpeg is not added to your system
##'   path. For Windows machines, path must point to 'ffmpeg.exe',
##'   located in the bin subfolder within the ffmpeg folder.  For
##'   example on Windows machines,
##'   "C:/Users/Username/Documents/ffmpeg-3.4.1-win64-static/bin/ffmpeg.exe").
##'   On Mac, path must point to 'ffmpeg' within the 'bin' subfolder
##'   "/home/directory/Documents/bin/ffmpeg".
##'   
##' @param diagnostic_mode Logical (default = FALSE). If true, return value
##'  is a character vector with FFMPEG output.
##' 
##' @details \strong{This function is DEPRECATED and is being maintained
##'   temporarily for backward compatibility with glatos v. 0.4.0 and earlier.}
##'   \emph{Use \link[glatos]{make_video} instead.} This function was previously
##'   named \code{make_video}. Starting with glatos v. 0.4.1,
##'   \link[glatos]{make_video} was changed to simplify inputs and to use the
##'   \code{av} package function \link[=encoding]{av::av_encode_video} instead
##'   of external program ffmpeg. For backward-compatibility, any calls to
##'   \link[glatos]{make_video} with arguments from the earlier version of the
##'   function will be redirected here (\code{make_video_ffmpeg}).
##' 
##' @details \code{make_video_ffmpeg} is based on \code{mapmate::ffmpeg}.
##'   More information about \code{mapmate} package is found at
##'   \code{https://github.com/leonawicz/mapmate}.  This function
##'   converts R syntax into command line call that is submitted to
##'   \code{ffmpeg}.  \code{ffmpeg} must be installed and able to be
##'   accessed by the system command line prior to running
##'   \code{make_video_ffmpeg}.  See \code{https://www.ffmpeg.org} for
##'   information on installing ffmpeg.
##'
##' @details The FFmpeg multimedia framework provides extensive
##'   flexibility and options for converting and manipulating video
##'   and audio content.  \code{make_video_ffmpeg} provides a small subset
##'   of options useful for creating simple animated videos.  If
##'   additional flexibility or options are needed, the user may need
##'   to develop or modify \code{make_video_ffmpeg} or submit calls
##'   directly to FFmpeg using the system command line.
##'
##' @details Sequenced image files are input into FFmpeg using a file name
##'   convention which requires specifying the entire, non-changing file name
##'   with a consecutive integer numbering component.  The integer number
##'   component indicates the maximum number of digits of all the images.  For
##'   example, \code{\%04d.png} represents the file numbering \code{0000.png,
##'   0001.png, 0002.png, ..., 9999.png} and \code{\%02d.png} represents images
##'   numbered from \code{00.png, 01.png, 02.png, ..., 10.png}.  File names of
##'   image sequences input to FFmpeg may have a prefix provided that all image
##'   files have the sequence. For example an image sequence of
##'   \code{myfile001.png, myfile002.png, myfile003.png, ..., myfile999.png} is
##'   represented as \code{myfile\%03d.png}.  \code{pattern} only accepts this
##'   pattern of image file names.
##'
##' @details Function can create .mp4, .mov, .mkv, .gif, .wmv, or
##'   .mpeg animations.  Format of created animation is determined by
##'   file extension of \code{output}.
##' @details \code{make_video_ffmpeg} allows
##'   user to specify input framerate \code{fps_in} of image sequence
##'   and output \code{fps_out} framerate of animation.
##'
##' @details \code{start_frame} specifies starting frame in animation
##'   and \code{end_frame} specifies end frame in animation.  If
##'   \code{start_frame} and \code{end_frame} are specified, only
##'   images within range are used in animation.  Specifying
##'   \code{start_frame} and \code{end_frame} allows user to skip
##'   images.
##'
##' @details If \code{size} is not set to \code{"source"}, the output video is
##'   scaled.  \code{size} can be a character string of dimensions in length by
##'   height format such as \code{"720x480"} or an abbreviated standard such as
##'   \code{"ntsc"}.  See
##'   \href{http://ffmpeg.org/ffmpeg-utils.html#Video-size}{FFmpegstandard video
##'   sizes} for common dimensions and available abbreviations.
##' 
##' @details Presets provide a certain encoding speed to compression
##'   ratio.  Available presets include \code{ultrafast},
##'   \code{superfast}, \code{veryfast}, \code{faster}, \code{fast},
##'   \code{medium}, \code{slow}, \code{slower}, \code{veryslow}.
##'   Faster preset i.e.(\code{ultrafast}) corresponds to greater file
##'   size, lower animation quality, and faster computation
##'   times. Slower speeds \code{veryslow} corresponds to smaller
##'   file, higher quality animation and slower computation times.
##'   See \url{http://trac.ffmpeg.org/wiki/Encode/H.264}
##'
##'  @details \code{codec} is ignored if the file name in
##'   \code{pattern} ends with \code{.gif}.  For other video output
##'   file types a default codec is used depending on the file
##'   extension of output animation.These
##'   can be overridden with options like \code{codec="h264"},
##'   \code{"libx264"}, \code{"libvpx"}, \code{"prores"},
##'   \code{"qtrle"}, etc., but the user needs to be knowledgeable
##'   regarding which codecs can be used for which output types or
##'   errors will be thrown.
##' 
##' @details \code{format} is ignored if the file name in
##'   \code{pattern} ends with \code{.gif}.  The default is
##'   \code{"yuv420p"}, which performs 4:2:0 chroma subsampling.  This
##'   pixel format can reduce video quality, but it is the default
##'   because it ensures compatibility with most media players. For
##'   valid alternatives, run \code{system("ffmpeg -pix_fmts")}.
##'
##' @details \code{lossless} is ignored except for relevant
##'   \code{codec} settings, e.g., \code{h264} or \code{libx264}.  If
##'   \code{TRUE}, recommended \code{preset} values are
##'   \code{ultrafast} or \code{veryslow}. See
##'   \url{https://trac.ffmpeg.org/wiki/Encode/H.264} for more
##'   information.
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
##' # make sure ffmpeg is on system path (see \code{make_frames} and details)
##' make_video_ffmpeg(dir = frames, pattern = "%02d.png", output = "animation.mp4")
##'
##' # make .wmv video
##' make_video(dir=frames, pattern = "%02d.png", output = "animation.wmv")
##'
##' # start animation on frame 10, end on frame 20
##' make_video_ffmpeg(dir=frames, pattern = "%02d.png", start_frame = 10, 
##'            end_frame = 20, output = "animation_2.mp4")
##'
##' # resize output video to 720x480
##' make_video_ffmpeg(dir=frames, pattern = "%02d.png", size = "720x480", 
##'            output = "animation_3.mp4" )
##'
##' # change ffmpeg preset
##' make_video(dir=frames, pattern = "%02d.png", preset = "ultrafast", 
##'            output = "animation_4.mp4")
##'
##' # change input framerate
##' make_video_ffmpeg(dir=frames, fps_in = 1, pattern = "%02d.png", 
##'            preset = "ultrafast", output = "animation_5.mp4")
##'
##' 
##' # add path to ffmpeg (windows)
##' make_video_ffmpeg(dir = frames, pattern = "%02d.png", output = "animation.mp4", 
##'            ffmpeg = "c://path//to//windows//ffmpeg.exe")
##'
##' # add path to ffmpeg (mac)
##' make_video_ffmpeg(dir = frames, pattern = "%02d.png", output = "animation.mp4", 
##'            ffmpeg = "/path/to/ffmpeg")
##' }
##'
##' @export

make_video_ffmpeg  <- function(
  dir = getwd(),
  pattern,
  output = "animation.mp4",
  output_dir = getwd(),
  fps_in = 30,
  start_frame = 1,
  end_frame = NULL,
  size = "source",
  preset = "medium",
  codec = "default",
  format = "yuv420p",
  lossless = FALSE,
  fps_out = 30,
  overwrite = FALSE,
  ffmpeg = NA,
  diagnostic_mode = FALSE){
  
  
  # test ffmpeg and get path
  ffmpeg <- glatos:::get_ffmpeg_path(ffmpeg)
  
  #check if dir exists
  if(!dir.exists(dir)) stop(paste0("Input dir '", dir , "' not found."), 
    .call = FALSE)
  
  #make output directory if it does not already exist
  if(!dir.exists(output_dir)) dir.create(output_dir)  
  
  cmd <- ifelse(is.na(ffmpeg), 'ffmpeg', ffmpeg)
  
  input <- file.path(dir, pattern)
  input <- paste0("-i ", "\"", input, "\"" )
  inrate <- paste("-framerate", fps_in)
  start <- paste("-start_number", start_frame)
  input <- paste0(paste(inrate, start, input), collapse = " ")
  ext <- strsplit(output, "\\.")[[1]]
  ext_stop <- 
    "'output' must end in '.mp4', '.mov', '.mkv', '.gif', 'wmv', 'mpeg'"
  if (length(ext) == 1) {
    stop(ext_stop)
  } else { ext <- utils::tail(ext, 1) }
  
  if (!ext %in% c("mp4", "mov", "mkv", "gif", "wmv", "mpeg")) stop(ext_stop)
  
  output_file <- file.path(output_dir, output)
  output <- paste0("\"", output_file, "\"")
  
  if (!is.null(end_frame)){
    nframes <- end_frame - start_frame
    output <- paste("-vframes", nframes, output)
  }
  
  format <- paste0("format=", format)
  
  if (size == "source") {
    size <- ""
  } else if (ext != "gif") {
    size <- paste0(",scale=", size, ",setsar=1:1")
  } else { size <- paste("-s", size) }
  
  if (ext == "gif") {
    vf <- size
  } else { vf <- paste0("-vf ", "\"", format, size, "\"")}
  
  output <- paste(vf, output)
  
  
  outrate <- paste("-r", fps_out)
  output <- paste(outrate, output, ifelse(overwrite, "-y", "-n"))
  
  if (ext == "gif") { vc <- " " 
  } else {
    if (codec == "default") 
      codec <- switch(ext, mp4 = "libx264", mov = "libx264", 
        mkv = "libx264", wmv = "libx264", mpeg = "libx264" )
    vc <- paste0(" -c:v ", codec, " -preset ", preset, " ")
    if (lossless & codec %in% c("h264", "libx264")) vc <- paste0(vc, "-qp 0 ")
  }
  
  x <- gsub("  ", " ", paste0(input, vc, output))
  
  #check if output file exists
  if(file.exists(output_file) & overwrite == FALSE) {
    warning("No video file written because output file already exists and ",
      "overwrite = FALSE.")
    return()
  }
  
  msg_i <-  system2(cmd, x, stdout = TRUE)
  
  if(diagnostic_mode) {
    message("[diagnostic mode]: See return object for ffmpeg output.")
    return(msg_i)
  }
  
  message("Video file written to ", output_file, ".")
  return(output_file)
}