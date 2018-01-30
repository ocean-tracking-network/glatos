## scale_factor = 0.5
## input = "~/Desktop/test.mp4"
## output_dir = "~/Desktop/"
## output = "test1.mp4"
## overwrite = FALSE
## ffmpeg = NA


#adjust_playback(scale_factor = 2, input = "~/Desktop/test.mp4", output_dir = "~/Desktop", output = "test2.wmv", overwrite = TRUE, ffmpeg = NA)


adjust_playback <- function(scale_factor, input = ".", output_dir = ".", output = "test.mp4", overwrite = FALSE, ffmpeg = NA ){

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


    ffcall <- sprintf('-i %s -r 30 -filter:v "setpts=%f*PTS" %s/%s %s', input, scale_factor, output_dir, output, (ifelse(overwrite, "-y", "-n")))


    system2(cmd, ffcall, stdout = FALSE)

}
