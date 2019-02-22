##' Install external FFmpeg software into `glatos` package
##' 
##' Downloads and installs external ffmpeg into `glatos` package
##' directory system to enable creation animated videos of fish
##' movements
##'
##' @details \code{install_ffmpeg} determines operating system
##'   (Windows or Mac) and downloads the latest stable version of
##'   FFmpeg software.  FFmpeg software is installed within glatos
##'   package and may be accessed using \code{system.file}
##'
##' 
##' @return message is returned to console signalling successful installation
##' @author Todd Hayden
##'
##' @examples
##'
##'\dontrun{
##'
##' # call function
##' install_ffmpeg()
##'
##' # determine version of FFmpeg installed and if install was successful
##' fle <- system.file("bin", "ffmpeg.exe", package = "glatos")
##' system2(fle, "-version")
##' 
##'}
##' @export

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
  pkg <- find.package("glatos", lib.loc = .libPaths())
  utils::unzip(destfile, exdir = tmp)
  fls <- list.files(tmp, full.names = TRUE, recursive = TRUE, 
                    pattern = "^ffmpeg$|ffmpeg.exe$")
  if(!dir.exists(file.path(pkg, "bin"))) dir.create(file.path(pkg, "bin"))
  fl_dest <- file.path(pkg, "bin", basename(fls))
  file.rename(fls, fl_dest)
  if(os == "osx") system2("chmod", c("+x", fl_dest))

}



# query OS
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



#get path to ffmpeg and test
get_ffmpeg_path <- function(ffmpeg){
  
  os <- get_os()
  
  #use path to ffmpeg exe in user lib if exists
  if(is.na(ffmpeg)) {
    #check for local user lib
    pkg <- find.package("glatos", lib.loc = .libPaths())
    ffmpeg_file <- list.files(file.path(pkg, "bin"), 
      recursive = TRUE, full.names = TRUE, pattern = "^ffmpeg$|ffmpeg.exe$")
    if(length(ffmpeg_file) > 0){
        ffmpeg <- ifelse(file.exists(ffmpeg_file), ffmpeg_file, NA)
    }
  }
  
  if(os == "windows") cmd <- ifelse(is.na(ffmpeg), 'ffmpeg.exe', ffmpeg)
  if(os == "osx") cmd <- ifelse(is.na(ffmpeg), 'ffmpeg', ffmpeg)
  
  ffVers <- suppressWarnings(system2(cmd, "-version", stdout=F)) #call ffmpeg
  if(ffVers == 127)
    stop(paste0('"ffmpeg" was not found.\n',
      "See install_ffmpeg() to install into the package directory.\n",
      "or ",
      'ensure it is installed add added to system PATH variable\n',
      "or specify path using input argument 'ffmpeg'\n\n",
      'FFmpeg is available from:\n https://ffmpeg.org/\n',
      'You may create the individual frames and then combine them\n',
      'into an animation manually using video editing software\n', 
      '(e.g., Windows Movie Maker or iMovie) by setting the animate\n',
      'argument to FALSE.'),
      call. = FALSE)
  
  return(ffmpeg)
}