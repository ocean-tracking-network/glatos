#' @title Convert Vemco VRL file(s) to CSV format (detection data only)
#' 
#' @description
#' Convert detection data from a VEMCO VRL file(s) to comma-separated-values 
#'   (CSV) format by invoking a system command in VUE (> 2.06; courtesy of Tim 
#'   Stone, Vemco). 
#'
#' @param vrl A character scalar or vector with names of VRL files (or single 
#'   file if scalar) or a single directory containing VRL files. 
#' @param vueExePath A character scalar with path to VUE.exe (default may not 
#'   always be correct).
#'
#' @details
#' If \code{vrl} is a directory, then all VRL files in that directory will be 
#' converted to CSV. Otherwise, only those files specified in \code{vrl} will 
#' be converted. Each output CSV file will have same name as its source VRL 
#' file. 
#' 
#' @return A character vector with output file names.
#'
#' @note
#' Receiver event data are not exported because that functionality was not 
#' supported by the VUE system command at time of writing. \cr\cr
#' To get the path to VUE.exe, right click on the icon, select "Properties", 
#' and then copy text in "Target" box. \cr\cr
#' To create a CSV for each time-corrected VRL file, first time-correct each 
#' file using the VRL editor in VUE (under Tools menu). To speed up that 
#' process, uncheck the "Import" checkbox next to each filename, then run 
#' \code{vrl2csv} to create a CSV for each edited (e.g. time corrected) VRL.
#' 
#' @author C. Holbrook (cholbrook@usgs.gov) 
#'
#' @examples
#' #get path to example VRL in this package
#' myVRL <- system.file("extdata", "VR2W_109924_20110718_1.vrl",package="glatos")
#' vrl2csv(dirname(myVRL)) #file name input
#' vrl2csv(myVRL) #directory input
#'
#' @export
vrl2csv <- function(vrl, vueExePath="C:\\Program Files (x86)\\Vemco\\VUE"){
  #check path to VUE.exe
  if( !("VUE.exe" %in% list.files(vueExePath))) stop("VUE.exe not found at specified path.")
  
  #if vrl is single directory, get list of vrl file names
  if(all(file.info(vrl)$isdir)) {
	if(!file.exists(vrl)) stop(paste0("File or folder ",vrl," not found."))
	if(length(vrl) > 1) stop("input argument 'vrl' cannot include more than one directory")
	if(length(vrl) == 0) stop(paste0("No VRL files found at ",vrl))
	if(length(vrl) == 1) vrl <- list.files(vrl,full.names=T, pattern=".vrl$|.VRL$|.Vrl$")
  } #end if
  
  #invoke vue command
  shelltxt <- paste0("cd \"",vueExePath,"\\\"", " & VUE --convert-files --output-format csv ", 
	paste(vrl,collapse=" "))
  shell(shelltxt)
  
  #return output vrl path(s) and file name(s)
  return(gsub(".vrl|.VRL|.Vrl",".ulfx",vrl))
}
