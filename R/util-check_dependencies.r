##' Check system dependencies necessary for package 'glatos' 
##'
##' Checks to make sure external dependencies required by functions in
##' 'glatos' are installed and available to R.
##'
##' @details \code{check_dependencies} checks that the Geospatial Data
##'   Abstraction Library (GDAL) and ffmpeg (cross platform software
##'   for manipulating video content) software are installed on your
##'   computer and accessible to R.  GDAL is used by the
##'   \code{make_transition} function to create a transition layer
##'   required by \code{interpolate_path} for non-linear
##'   interpolation.  FFmpeg is required to create or modify video
##'   animations of fish movement using the \code{make_frames},
##'   \code{make_video}, and \code{adjust_video_playback} functions.
##'
##' @details When \code{check_dependencies} is executed, R attempts to
##'   sequentially access the external libraries.  If the libraries
##'   are installed and accessible, a message is returned to the
##'   terminal stating that the check was successful. Failed attempts
##'   to access the external libraries are printed to the terminal.
##'
##' @details Installation of the GDAL library and a number of other
##'   open-source programs useful for working with spatial data on
##'   windows is accomplished by downloading the network installer for
##'   your appropriate windows computer (32 or 64 bit) at
##'   \url{https://trac.osgeo.org/osgeo4w/}. Installation of the GDAL
##'   library on Mac is possible using Homebrew or KyngChaos 3rd party
##'   repositories.  Alternatively, the GDAL library is incorporated
##'   in QGIS (open source desktop GIS) software and may be obtained
##'   by installing QGIS.  Standalone installers for QGIS are
##'   available (Windows, Mac, and Linux) at
##'   \url{https://qgis.org/en/site/forusers/download.html}.
##'
##' @details Installation of the ffmpeg library on windows is
##'   accomplished by downloading the recent 'static' build from
##'   \url{http://ffmpeg.zeranoe.com/builds/}.  After the download is
##'   complete, use your favorite compression utility to extract the
##'   downloaded folder. Decompress the package and store contents on
##'   your computer.  Last, Edit your system path variable to include
##'   the path to the directory containing ffmpeg.exe
##'
##' @details Installation of ffmpeg on Mac is similar to
##'   windows. First, download most recent build from
##'   \url{http://www.evermeet.cx/ffmpeg/}.  The binary files are
##'   compressed with 7zip so may need toinstall an unarchiving
##'   utility (\url{http://wakaba.c3.cx/s/apps/unarchiver.html}) to
##'   extract the program folder. After the folder is extracted, copy
##'   the ffmpeg folder to /usr/local/bin/ffmpeg on your machine.
##' 
##' @return results of checks to your terminal
##' 
##' @author Todd Hayden
##'
##' @examples
##'\dontrun{
##' # run check
##' check_dependencies() 
##' }
##'
##' @export


check_dependencies <- function(){
  
  # check for gdal
  message("Checking for gdal...")
  suppressWarnings(gdalUtils::gdal_setInstallation())
  valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
  if(valid_install){
    message(sprintf("OK... gdal version %s is installed",
                    getOption("gdalUtils_gdalPath")[[1]]$version))
  } else {
    message(paste0("gdal not found.\n",
                   "To install gdal, see:\n",
                   "\t- http://www.gdal.org\n",
                   "\t- https://trac.osgeo.org/osgeo4w\n",
                   "\t- https://trac.osgeo.org/gdal/wiki/DownloadingGdalBinaries\n",
                   "or to install QGIS (gis software), see:\n",
                   "\t- https://qgis.org/en/site/forusers/download.html"))
  }
  
  # check for ffmpeg installation
  message("Checking for ffmpeg...") 
  # check for FFmpeg
  ffmpeg <-  Sys.which("ffmpeg")
  if(ffmpeg != ""){
    message("OK... FFmpeg is installed and on system PATH.")
  } else {
    message(paste0("FFmpeg not found.\n",
                   "To install and set system PATH variable, see:\n",
                   "\t- https://www.ffmpeg.org\n",
                   "\t- ?check_dependencies"))
  }
}
