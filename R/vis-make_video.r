##' Create video from sequence of still images 
##'
##' Stitch a sequence of images into a video animation using FFmpeg
##' software.
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
##' @details \code{make_video} is based on \code{mapmate::ffmpeg}.
##'   More information about \code{mapmate} package is found at
##'   \code{https://github.com/leonawicz/mapmate}.  This function
##'   converts R syntax into command line call that is submitted to
##'   \code{ffmpeg}.  \code{ffmpeg} must be installed and able to be
##'   accessed by the system command line prior to running
##'   \code{make_video}.  See \code{https://www.ffmpeg.org} for
##'   information on installing ffmpeg.
##'
##' @details The FFmpeg multimedia framework provides extensive
##'   flexibility and options for converting and manipulating video
##'   and audio content.  \code{make_video} provides a small subset
##'   of options useful for creating simple animated videos.  If
##'   additional flexibility or options are needed, the user may need
##'   to develop or modify \code{make_video} or submit calls
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
##' @details \code{make_video} allows
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
##' make_video(dir = frames, pattern = "%02d.png", output = "animation.mp4"  )
##'
##' # make .wmv video
##' make_video(dir=frames, pattern = "%02d.png", output = "animation.wmv" )
##'
##' # start animation on frame 10, end on frame 20
##' make_video(dir=frames, pattern = "%02d.png", start_frame = 10, 
##'            end_frame = 20, output = "animation_2.mp4")
##'
##' # resize output video to 720x480
##' make_video(dir=frames, pattern = "%02d.png", size = "720x480", 
##'            output = "animation_3.mp4" )
##'
##' # change ffmpeg preset
##' make_video(dir=frames, pattern = "%02d.png", preset = "ultrafast", 
##'            output = "animation_4.mp4")
##'
##' # change input framerate
##' make_video(dir=frames, fps_in = 1, pattern = "%02d.png", 
##'            preset = "ultrafast", output = "animation_5.mp4")
##'
##' 
##' # add path to ffmpeg (windows)
##' make_video(dir = frames, pattern = "%02d.png", output = "animation.mp4", 
##'            ffmpeg = "c://path//to//windows//ffmpeg.exe")
##'
##' # add path to ffmpeg (mac)
##' make_video(dir = frames, pattern = "%02d.png", output = "animation.mp4", 
##'            ffmpeg = "/path/to/ffmpeg")
##' }
##'
##' @export


make_video  <- function(input_dir = getwd(),
                        ext = "png",
                        output = "animation.mp4",
                        duration = NULL,
                        start_frame = 1,
                        end_frame = NULL,
                        size = NULL,
                        overwrite = FALSE,
                        verbose = FALSE,
                        ...){
  
  #parse ...
  in_args <- c(output = output, verbose = verbose, list(...))
  #for debugging, use code on next line instead of previous
  #in_args <- list(output = output, verbose = verbose) #none given
  #only ffmpeg arguments given
  #in_args <- list(output = output, verbose = verbose, 
  #                pattern = "%02d.png", preset = "ultrafast")
  #only av arguments given
  #in_args <- list(output = output, verbose = verbose, framerate = 30, 
  #                vfilter = "scale=320:240, setpts=10*PTS")
  
  #identify av-specific optional arguments in ...
  av_args = formals(av::av_encode_video)
  
  av_args_in = in_args[intersect(names(in_args), names(av_args))]
  
  av_args[names(av_args_in)] <- av_args_in
    
  #identify ffmpeg-specific (legacy) arguments in ...
  #for backward compatibility with glatos <= v.0.4.0
  ffmpeg_args <- alist(pattern = ,
                      output = "animation.mp4",
                      size = "source", 
                      preset = "medium", 
                      codec = "default", 
                      format = "yuv420p", 
                      lossless = FALSE, 
                      fps_out = 30, 
                      ffmpeg = NA, 
                      diagnostic_mode = FALSE)
  
  ffmpeg_args_in = intersect(names(in_args), names(ffmpeg_args))
  
  #identify arguments that could apply to either renderer
  both_args <- intersect(names(ffmpeg_args), names(av_args))
  
  #if any ffmpeg-specific args are given and no av-specific are given 
  # (ignore codec because it is not ffmpeg-specific)
  # then set renderer = "ffmpeg"
  use_ffmpeg <- ifelse(length(setdiff(ffmpeg_args_in, both_args)) > 0 & 
      length(setdiff(av_args_in, both_args)) == 0, TRUE, FALSE) 
        
  
  #if no ffmpeg-specific args are given and no av-specific args are given
  # then set renderer = "av" but ensure output matches default when 
  # renderer = "ffmpeg"
  use_av <- ifelse(length(setdiff(ffmpeg_args_in, both_args)) == 0 & 
      length(setdiff(av_args_in, both_args)) > 0, TRUE, FALSE)
  
  #check renderer
  if(missing(renderer)) {
    if(use_ffmpeg & !use_av) renderer <- "ffmpeg"
    if(!use_ffmpeg & use_av) renderer <- "av"
    if(!use_ffmpeg & !use_av) renderer <- "av"
    if(use_ffmpeg & use_av) stop("Input arguments for both av and ffmpeg",
      "were provided but renderer was not specified")
  }
  
  dir <- input_dir
  
  #make vector of images
  
  #strip . from ext if present
  ext <- gsub("^\\.", "", ext)
  
  #make regex string
  ext_regex <- paste0(".", eval(ext), "$")
  
  files <- list.files(dir, full.names = TRUE, pattern = ext_regex)
  
  #check if dir exists
  if(!dir.exists(dir)) stop(paste0("Input dir '", dir , "' not found."), 
                            .call = FALSE)
  
  # subset out frames if needed
  if(is.null(end_frame)) end_frame <- length(files)
  
  fls <- files[start_frame:end_frame]
  
  if(is.null(vfilter)){
    #set size only if vfilt is null; otherwise ignore size
    if(is.null(size)) { vfilt <- "scale=320:240" } else { vfilt <- "null" }
  } else { 
    vfilt <- vfilter }
  
  #calculate duration if not given
  if(is.null(duration)) { 
    duration <- length(fls) / framerate 
  } else {
    framerate <- round(length(fls) / duration)
  }
  
  #update input & framerate & vfilter
  av_args$input <- fls
  av_args$framerate <- framerate
  av_args$vfilter <- vfilter
  
  do.call(av::av_encode_video, av_args)
  
  message("Video file written to ", output, ".")  
  
  #warn if framerate is greater than 30
  if(framerate > 30) warning("Specified duration (", duration, " seconds) ", 
    "results in a framerate (", framerate, " fps) that may be too fast to see!")
                             
}


##' Create video from sequence of still images using ffmpeg.exe
##'
##' Stitch a sequence of images into a video animation using FFmpeg
##' software. NOTE: This functions is DEPRECATED and is being maintained 
##' temporarily for backward compatibility with glatos v. 0.4.0 and earlier.
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
##' @details \code{make_video} is based on \code{mapmate::ffmpeg}.
##'   More information about \code{mapmate} package is found at
##'   \code{https://github.com/leonawicz/mapmate}.  This function
##'   converts R syntax into command line call that is submitted to
##'   \code{ffmpeg}.  \code{ffmpeg} must be installed and able to be
##'   accessed by the system command line prior to running
##'   \code{make_video}.  See \code{https://www.ffmpeg.org} for
##'   information on installing ffmpeg.
##'
##' @details The FFmpeg multimedia framework provides extensive
##'   flexibility and options for converting and manipulating video
##'   and audio content.  \code{make_video} provides a small subset
##'   of options useful for creating simple animated videos.  If
##'   additional flexibility or options are needed, the user may need
##'   to develop or modify \code{make_video} or submit calls
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
##' @details \code{make_video} allows
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
##' make_video(dir = frames, pattern = "%02d.png", output = "animation.mp4"  )
##'
##' # make .wmv video
##' make_video(dir=frames, pattern = "%02d.png", output = "animation.wmv" )
##'
##' # start animation on frame 10, end on frame 20
##' make_video(dir=frames, pattern = "%02d.png", start_frame = 10, 
##'            end_frame = 20, output = "animation_2.mp4")
##'
##' # resize output video to 720x480
##' make_video(dir=frames, pattern = "%02d.png", size = "720x480", 
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


make_video_ffmpeg  <- function(dir = getwd(),
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