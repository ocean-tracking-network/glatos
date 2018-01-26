
dir = "~/Documents/bug_squash/hornsby/Frames2/"
pattern = "%d.png"
output = "test.mp4"
output_dir = "~/Desktop"
rate = "ntsc"
delay = 1
start = 1
size = "source"
preset = "ultrafast" 
codec = "default"
format = "yuv420p"
lossless = FALSE
min.rate = 10
fps.out = rate
alpha = 1
overwrite = FALSE 
glob = FALSE


library(mapmate)
make_frames("~/Documents/bug_squash/hornsby/Frames2/", pattern = "%d.png",  output_dir = "~/Desktop", output = "test.mp4")


make_frames <- function(dir = ".", pattern, output, output_dir = ".", rate = "ntsc",
     delay = 1, start = 1, size = "source", preset = "ultrafast",
     codec = "default", format = "yuv420p", lossless = FALSE,
     min.rate = 10, fps.out = rate, alpha = 1, overwrite = FALSE,
     glob = FALSE, ffmpeg = NA){

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
  
if (!missing(rate) && !missing(delay)) {stop("specify 'rate' or 'delay' but not both")}
    if (!missing(delay)) 
        rate <- round(1/delay)
    linux <- .Platform$OS.type == "linux"
    iglob <- "-pattern_type glob -i "
    input <- file.path(dir, pattern)
    blend <- if (length(input) == 1) 
        FALSE
    else TRUE
    input <- if (linux & glob) 
        paste0(iglob, "\"", input, "\"")
    else paste("-i", input)
    inrate <- paste("-framerate", rate)
    start <- paste("-start_number", start)
    input <- paste0(paste(start, inrate, input), collapse = " ")
    ext <- strsplit(output, "\\.")[[1]]
    ext_stop <- "'output' must end in '.mp4', '.mov', '.mkv', '.webm', or '.gif'"
    if (length(ext) == 1) 
        stop(ext_stop)
    else ext <- utils::tail(ext, 1)
    if (!ext %in% c("mp4", "mov", "mkv", "webm", "gif")) 
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
    if (blend) {
        blend_prefix <- "-filter_complex \"blend=all_mode='overlay':all_opacity="
        if (ext == "gif") {
            vf <- paste0(blend_prefix, alpha, "\"")
        }
        else {
            vf <- paste0(blend_prefix, alpha, ",", format, size, 
                "\"")
        }
    }
    else if (ext == "gif") {
        vf <- size
    }
    else vf <- paste0("-vf ", "\"", format, size, "\"")
    output <- paste(vf, output)
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
}
