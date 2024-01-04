#' @export
check_dependencies <- function(){
  
  # check for ffmpeg
  message("Checking for ffmpeg...")
  ffmpeg <- tryCatch(list(found = TRUE, value = glatos:::get_ffmpeg_path(NA)), 
                     error = function(e) list(found = FALSE, value = e$message))
                         
  
  # print message with result
  if(ffmpeg$found) { 
    message(" OK... FFmpeg is installed at \n  ", ffmpeg$value, ".", "\n") 
  } else {
    message(ffmpeg$value, "\n")
  }
  
}
