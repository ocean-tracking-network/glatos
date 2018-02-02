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
##' @param output_dir output directory, default is working directory.
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
##' @param ffmpeg A character string with path to install directory
##'   for ffmpeg.  This argument is only needed if ffmpeg has not been
##'   added to your path variable on your computer.  For Windows
##'   machines, path must point to ffmpeg.exe, typically located in
##'   "bin" folder.  For example,
##'   'c:\\path\\to\\ffmpeg\\bin\\ffmpeg.exe' or 'path/to/ffmpeg' on
##'   mac.
##' 
##' @details \code{animate_video} is based on \code{mapmate::ffmpeg}.
##'   More information about\code{mapmate} package is found at
##'   \code{https://github.com/leonawicz/mapmate}.  This function
##'   converts R syntax into command line call that is submitted to
##'   \code{ffmpeg}.  \code{ffmpeg} must be installed and able to be
##'   accessed by the system command line prior to running
##'   \code{animate_video}.  See \code{https://www.ffmpeg.org} for
##'   information on installing ffmpeg.
##'
##' @details The FFmpeg multimedia framework provides extensive
##'   flexibility and options for converting and manipulating video
##'   and audio content.  \code{animate_video} provides a small subset
##'   of options useful for creating simple animated videos.  If
##'   additional flexibility or options are needed, the user may need
##'   to develop or modify \code{animate_video} or submit calls
##'   directly to FFmpeg using the system command line.
##'
##' @details Sequenced image files are input into FFmpeg using a file
##'   name convention which requires specifying the entire,
##'   non-changing file name with a consecutive integer numbering
##'   component.  The integer number component indicates the maximum
##'   number of digits of all the images.  For example, \code{\%04d.png}
##'   represents the file numbering \code{0000.png, 0001.png, 0002.png, ..., 9999.png}
##'   and \code{\%02d.png} represents images numbered from \code{00.png, 01.png,
##'   02.png, ..., 10.png}.  File names of image sequences input to FFmpeg may
##'   have a prefix provided that all image files have the
##'   sequence. For example an image sequence of \code{myfile001.png,
##'   myfile002.png, myfile003.png, ..., myfile999.png} is represented
##'   as \code{myfile\%03d.png}.  \code{pattern} only accepts this pattern
##'   of image file names.
##'
##' @details Function can create .mp4, .mov, .mkv, .gif, .wmv, or
##'   .mpeg animations.  Format of created animation is determined by
##'   file extension of \code{output}.
##' @details \code{animate_video} allows
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
##' @details If \code{size} is not set to \code{"source"}, the output
##'   video is scaled.  \code{size} can be a character string of
##'   dimensions in length by height format such as \code{"720x480"} or
##'   an abbreviated standard such as \code{"ntsc"}.  See
##'   \href{http://ffmpeg.org/ffmpeg-utils.html#Video-size}{FFmpeg
##'   standard video sizes} for common dimensions and available
##'   abbreviations.
##' 
##'@details Presets provide a certain encoding speed to compression
##'   ratio.  Available presets include \code{ultrafast},
##'   \code{superfast}, \code{veryfast}, \code{faster}, \code{fast},
##'   \code{medium}, \code{slow}, \code{slower}, \code{veryslow}.
##'   Faster preset i.e.(\code{ultrafast}) corresponds to greater file
##'   size, lower animation quality, and faster computation
##'   times. Slower speeds \code{veryslow} corresponds to smaller
##'   file, higher quality animation and slower computation times.
##'   See \href{http://trac.ffmpeg.org/wiki/Encode/H.264}
##'
##' @details \code{codec} is ignored if the file name in
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
##' make_video(dir=frames, pattern = "%02d.png", start_frame = 10, end_frame = 20, output = "animation_2.mp4")
##'
##' # resize output video to 720x480
##' make_video(dir=frames, pattern = "%02d.png", size = "720x480", output = "animation_3.mp4" )
##'
##' # change ffmpeg preset
##' make_video(dir=frames, pattern = "%02d.png", preset = "ultrafast", output = "animation_4.mp4")
##'
##' # change input framerate
##' make_video(dir=frames, fps_in = 1, pattern = "%02d.png", preset = "ultrafast", output = "animation_5.mp4")
##'
##' \dontrun{
##' # add path to ffmpeg (windows)
##' make_video(dir = frames, pattern = "%02d.png", output = "animation.mp4", ffmpeg = "c://path//to//windows//ffmpeg.exe")
##'
##' # add path to ffmpeg (mac)
##' make_video(dir = frames, pattern = "%02d.png", output = "animation.mp4", ffmpeg = "/path/to/ffmpeg")
##' }
##'
##' @export


make_video  <- function(dir = ".",
                        pattern,
                        output = "animation.mp4",
                        output_dir = ".",
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
                        ffmpeg = NA){
  
  # try calling ffmpeg

  # ffmpeg command must execute ffmpeg
  # (i.e., "c://path//to//windows//ffmpeg.exe" or "path/to/ffmpeg" in Mac/linux)
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

  input <- file.path(dir, pattern)
  input <- paste("-i", input)
  inrate <- paste("-framerate", fps_in)
  start <- paste("-start_number", start_frame)
  input <- paste0(paste(inrate, start, input), collapse = " ")
  ext <- strsplit(output, "\\.")[[1]]
  ext_stop <- "'output' must end in '.mp4', '.mov', '.mkv', '.gif', 'wmv', 'mpeg'"
  if (length(ext) == 1)
        stop(ext_stop)
    else ext <- utils::tail(ext, 1)
    if (!ext %in% c("mp4", "mov", "mkv", "gif", "wmv", "mpeg"))
        stop(ext_stop)
  output <- file.path(output_dir, output)

if (!is.null(end_frame)){
    nframes <- end_frame - start_frame
    output <- paste("-vframes", nframes, output)
    }
  
    format <- paste0("format=", format)
    if (size == "source") {
        size <- ""
    }
    else if (ext != "gif") {
        size <- paste0(",scale=", size, ",setsar=1:1")
    }
    else size <- paste("-s", size)
    
    if (ext == "gif") {
        vf <- size
    } else vf <- paste0("-vf ", "\"", format, size, "\"")
  
  output <- paste(vf, output)

  
    outrate <- paste("-r", fps_out)
    output <- paste(outrate, output, ifelse(overwrite, "-y", 
        "-n"))
    if (ext == "gif") {
        vc <- " "
    }
    else {
        if (codec == "default") 
            codec <- switch(ext, mp4 = "libx264", mov = "libx264", 
                mkv = "libx264", wmv = "libx264", mpeg = "libx264" )
        vc <- paste0(" -c:v ", codec, " -preset ", preset, " ")
        if (lossless & codec %in% c("h264", "libx264")) 
            vc <- paste0(vc, "-qp 0 ")
    }
  x <- gsub("  ", " ", paste0(input, vc, output))
    system2(cmd, x, stdout=FALSE)
   return(paste(cmd, x))
}
