#' @title Convert Vemco VRL file(s) to CSV format (detection data only)
#' 
#' @description
#' Convert detection data from a VEMCO VRL file(s) to comma-separated-values 
#'   (CSV) format by invoking a system command in VUE (> 2.06; courtesy of Tim 
#'   Stone, Vemco). 
#'
#' @param vrl A character string or vector with names of VRL file(s) or a 
#'   single directory containing VRL files. 
#'   
#' @param outDir A character string directory where CSV files will be written. 
#'   If \code{NA} (default) then file(s) will be written to the current working 
#'   directory (e.g., \code{getwd()}).
#'   
#' @param overwrite Logical. If TRUE (default), output CSV file(s) will 
#'   overwrite existing CSV file(s) with same name in \code{outDir}. When FALSE,
#'   '_n' (i.e., _1, _2, etc.) will be appended to names of output files that
#'   already exist in \code{outDir}.
#'   
#' @param vueExePath An optional character string with directory containing
#'   VUE.exe. If NA (default) then the path to VUE.exe must be added to the 
#'   PATH environment variable of your system. See Note below.
#'   
#'
#' @details
#' If \code{vrl} is a directory, then all VRL files in that directory will be 
#' converted to CSV. Otherwise, only those files specified in \code{vrl} will 
#' be converted. Each output CSV file will have same name as its source VRL 
#' file. 
#' 
#' @return A character vector with output directory and file name(s). 
#'
#' @note
#' Receiver event data are not exported because that functionality was not 
#' supported by the VUE system command at time of writing. 
#' 
#' @note
#' The path to VUE.exe must either be specified by \code{vueExePath} or 
#' added to the PATH environment variable of your system. To get the path to 
#' VUE.exe in Windows, right click on the icon, select "Properties", and 
#' then copy text in "Target" box. 
#' 
#' @note
#' To create a CSV for time-corrected VRL files, first time-correct each 
#' file using the VRL editor in VUE (under Tools menu). To speed up that 
#' process, uncheck the "Import" checkbox next to each filename, then run 
#' \code{vrl2csv} to create a CSV for each edited (e.g. time-corrected) VRL.
#' 
#' @note 
#' When using versions of VUE before 2.3, VUE can return an error code or 
#' warning message even if conversion was successful.
#' 
#' @author C. Holbrook (cholbrook@usgs.gov) 
#'
#' @examples
#' \dontrun{
#' 
#' #get path to example VRL in this package
#' myVRL <- system.file("extdata", "VR2W_109924_20110718_1.vrl",
#'  package="glatos")
#' vrl2csv(dirname(myVRL)) #directory input
#' vrl2csv(myVRL) #file name input
#'
#' #setting 'overwrite=FALSE' will make new file with '_n'added to name
#' vrl2csv(myVRL, overwrite=F)
#' }
#'
#' @export
vrl2csv <- function(vrl, outDir=NA, overwrite=TRUE, vueExePath=NA){
  #check path to VUE.exe if given
  if(!is.na(vueExePath)){
    if( !("VUE.exe" %in% list.files(vueExePath))) 
      stop("VUE.exe not found at specified path.")
    vuePath <- paste0(vueExePath,"\\VUE.exe")
  } else {
    vuePath <- "VUE" #if vueExePath not set
  }
  
  #if vrl is single directory, get list of vrl file names
  if(all(file.info(vrl)$isdir)) {
  	if(length(vrl) > 1) stop("input argument 'vrl' cannot include more than ",
                             "one directory")
  	if(length(vrl) == 1) vrl <- list.files(vrl, full.names = TRUE, 
  	                                       pattern = "vrl$", ignore.case = TRUE)
  	if(length(vrl) == 0) stop("No VRL files found.")
  } #end if
  
  #check if missing
  missing_vrls <- which(!file.exists(vrl))
  if(length(missing_vrls) > 0) stop("VRL files not found: \n ",
                        paste(basename(vrl)[missing_vrls], collapse = "\n "))
  
  #set output directory to working directory if not specified
  if(is.na(outDir)) outDir <- getwd()
  
  #set --overwrite-files option
  overwrite_file <- ifelse(overwrite,"--overwrite-file","")
  
  #invoke vue command for each file
  for(i in 1:length(vrl)){
    if(i == 1) {
      message("Converting ", length(vrl), " detection files...")
      pb <- txtProgressBar(0, max = length(vrl), style = 3)
    }
    foo_i <- system2(vuePath,c("--convert-files",
      paste("--output-path ", outDir, "", sep = '"'), overwrite_file, 
      paste(" --output-format csv ", vrl[i],"", sep = '"')))
    if(foo_i == '127') stop("VUE.exe was not found.\n",
      " Ensure that VUE is installed on your system and that either the \n",
      " PATH environment variable was set (see ?vrl2csv).\n",
      " or specify the path to VUE.exe using the 'vueExePath' argument.\n")
    
    setTxtProgressBar(pb, i)
  } #end i
  
  #return output path(s) and file name(s)
  outFName <- file.path(outDir, gsub("vrl$", "csv", basename(vrl), ignore.case = TRUE))
  fileCheck <- file.exists(outFName)
  
  cat('\n')
  return(outFName[fileCheck])
  
  # Warn if any files were not created.
  if(any(!fileCheck)){
    for(i in 1:length(fileCheck[!fileCheck])){
      warning(basename(outFName[!fileCheck][i]), " was not created.")
    }
  }
}
