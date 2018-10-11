##' install ffmpeg
##' @details checks for and installs ffmpeg into package directory for making animations
##' @return value
##' @author Todd Hayden
##' @export

# install ffmpeg to GLATOS package
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}


install_ffmpeg <- function(){
    # determine what OS
    os <- get_os()

    # download windows build of ffmpeg
    if(os == "windows"){

        url <- "http://ffmpeg.zeranoe.com/builds/win64/static/ffmpeg-latest-win64-static.zip"
    }

    # download osx build of ffmpeg
    if(os == "osx"){
        url <- "http://ffmpeg.zeranoe.com/builds/macos64/static/ffmpeg-latest-macos64-static.zip"
    }
   
    # find package files on system
    tmp <- tempdir()
    destfile <- file.path(tmp, "ffmpeg.zip")
    download.file(url, destfile = destfile, mode = "wb")
    pkg <- find.package("glatos")
    utils::unzip(destfile, exdir = pkg)
    fls <- list.files(pkg)
    file.rename(file.path(pkg, fls[grep("ffmpeg-*", fls) ]), file.path(pkg, "ffmpeg"))
}


# shoule be able to use "system.file" to call ffmpeg
#system.file("ffmpeg/bin/ffmpeg.exe", package = "glatos")
#install_ffmpeg()
