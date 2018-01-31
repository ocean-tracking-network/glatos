##' Create video from image sequence
##'
##' Create video from directory of sequenced images (frames) using FFmpeg.
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param dir 
##' @param pattern 
##' @param output 
##' @param output_dir 
##' @param fps.in 
##' @param start.frame 
##' @param end.frame 
##' @param size 
##' @param preset 
##' @param codec 
##' @param format 
##' @param lossless 
##' @param min.rate 
##' @param fps.out 
##' @param overwrite 
##' @param ffmpeg 
##' @return 
##' @author Todd Hayden
##'
##'
##' 


## #library(mapmate)
animate_video(dir = "~/Documents/bug_squash/hornsby/Frames2/", pattern = "%d.png",  output_dir = "~/Desktop", fps.in = 30, start.frame = 1, end.frame = NULL, size = "source", preset = "ultrafast", codec = "default", format = "yuv420p", lossless = FALSE, min.rate = 10, fps.out = 30, overwrite = TRUE, ffmpeg = NA)



animate_video  <- function(dir = ".", pattern, output = "animation.mp4", output_dir = ".", fps.in = 30, start.frame = 1, end.frame = NULL, size = "source", preset = "ultrafast", codec = "default", format = "yuv420p", lossless = FALSE, min.rate = 10, fps.out = 30, overwrite = FALSE, ffmpeg = NA){
  
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
  
  input <- file.path(dir, pattern)
  input <- paste("-i", input)
  inrate <- paste("-framerate", fps.in)
  start <- paste("-start_number", start.frame)
  input <- paste0(paste(inrate, start, input), collapse = " ")
  ext <- strsplit(output, "\\.")[[1]]
  ext_stop <- "'output' must end in '.mp4', '.mov', '.mkv', '.webm', '.gif', or 'wmv'"
  if (length(ext) == 1) 
        stop(ext_stop)
    else ext <- utils::tail(ext, 1)
    if (!ext %in% c("mp4", "mov", "mkv", "webm", "gif", "wmv")) 
        stop(ext_stop)
    output <- file.path(output_dir, output)
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

  if (!is.null(end.frame)){
    nframes <- end.frame - start.frame
    output <- paste("-vframes", nframes)
    }

    outrate <- paste("-r", max(fps.out, min.rate))
    output <- paste(outrate, output, ifelse(overwrite, "-y", 
        "-n"))
    if (ext == "gif") {
        vc <- " "
    }
    else {
        if (codec == "default") 
            codec <- switch(ext, mp4 = "libx264", mov = "libx264", 
                mkv = "libx264", webm = "libvpx")
        vc <- paste0(" -c:v ", codec, " -preset ", preset, " ")
        if (lossless & codec %in% c("h264", "libx264")) 
            vc <- paste0(vc, "-qp 0 ")
    }
  x <- gsub("  ", " ", paste0(input, vc, output))
    system2(cmd, x, stdout=FALSE)
  #  return(paste(cmd, x))
}





