#'Convert an Innovasea Vemco VRL file to a VUE CSV file
#'
#'Use Innovasea's command line program VUE.exe (distributed with VUE software)
#'to make a CSV file containing detection data from a VRL file (receiver event
#'data are not supported).
#'
#'@param src Character string with path and name of a detection file
#'  (\code{VRL}), a vector of file names, or a directory containing files. If
#'  only file name is given, then the file must be located in the working
#'  directory.
#'
#'@param out_dir Optional character string with directory where CSV files will
#'  be written. If \code{NULL} (default) then each file will be written to the
#'  same directory as its source file.
#'
#'@param overwrite Logical. If \code{TRUE}, output CSV file(s) will overwrite
#'  existing CSV file(s) with same name in \code{out_dir}. If \code{FALSE}
#'  (default), any output files that already exist in \code{out_dir} will be
#'  skipped, with warning.
#'
#'@param recursive Logical. If \code{TRUE} and \code{src} is a directory, then
#'  all VRL files in all subdirectories of \code{src} will be converted. Default
#'  is \code{FALSE}. Ignored if \code{src} is a not directory.
#'
#'@param vue_exe_path The full path to \code{VUE.exe}. If \code{NULL} (default)
#'  then the path to VUE.exe must be in the PATH environment variable of the
#'  system. See \code{\link{check_vue}}.
#'
#'@param skip_pattern A regular expression used to exclude files from
#'  processing. Default value \code{"-RLD_"} will exclude "RAW LOG" files.
#'  Ignored if \code{src} contains file names.
#'
#'@param show_progress Logical. Indicates if progress bar should be shown.
#'
#'@param diagn Logical. Indicates if errors or warnings message (from vue.exe)
#'  should be displayed (default = \code{FALSE}).
#'
#'@details If \code{src} is a directory, then all source files in that directory
#'  (including all subdirectories if \code{recursive = TRUE}) with supported
#'  extension (\code{"vrl"}) will be converted to CSV. Otherwise, only those
#'  files specified in \code{src} will be converted. Each output CSV file will
#'  have same name as its source file, except extension will be \code{csv}.
#'
#'@details Conversion is done by system call to the Innovasea program
#'  \code{VUE.exe} (included with Innovasea's VUE software; available at
#'  \url{https://support.fishtracking.innovasea.com/s/downloads}). VUE.exe must
#'  be available at the location specified by \code{vue_exe_path} or via system
#'  PATH environment variable. See also \code{\link{check_vue}}.
#'
#'@section Output: A comma-separated-values (CSV) text file in Innovasea's
#'  default VUE Export format for each input VRL file. Each CSV is named the
#'  same (except for extension) as the source file (e.g.,
#'  \code{VR2W_109924_20110718_1.csv}).
#'
#'@return A character string or vector with the full path and name of each
#'  output file, including files that were skipped (when output file exists and
#'  \code{overwrite = FALSE}).
#'
#'@note Tested on VUE 2.8.1.
#'
#'@note Only detection records are written. Receiver event data are not exported
#'  because that functionality was not supported by the VUE system command at
#'  time of writing. See \code{\link{vue_convert}} to extract detection and
#'  event data.
#'
#'@note VUE does not automatically correct timestamps for clock skew. To create
#'  a CSV file with corrected timestamps using VUE's linear time correction
#'  method, first time-correct each file using the VRL editor in VUE (under
#'  Tools menu). To speed up that process, uncheck the "Import" checkbox next to
#'  each filename, then run \code{vue_convert} to create a CSV for each edited
#'  (e.g. time-corrected) VRL.
#'
#'@author C. Holbrook (cholbrook@@usgs.gov)
#'
#' @examples
#'
#' \dontrun{
#'
#' # Check vue.exe
#' check_vue()
#'
#' #all examples below assume path to VUE.exe is in system PATH environment
#' # variable. If not (you get an error), add input argument 'vue_exe_path'
#' # with path directory with VUE.exe.
#' # e.g.,
#' #vue_convert(vrl_files,
#' #            vue_exe_path = "C:/Program Files (x86)/Vemco/VUE")
#'
#' #get path to example VRL files in glatos
#' vrl_files <- system.file("extdata", "detection_files_raw",
#'                         c("VR2W_109924_20110718_1.vrl",
#'                           "VR2W180_302187_20180629_1.vrl",
#'                           "VR2AR_546310_20190613_1.vrl",
#'                           "VR2Tx_480022_20190613_1.vrl"),
#'                         package = "glatos")
#'
#' #copy to temp_dir
#' temp_dir <- tempdir()
#' vrl_files2 <- file.path(temp_dir, basename(vrl_files))
#' file.copy(vrl_files, vrl_files2)
#' 
#' #uncomment to open in file browser
#' #utils::browseURL(temp_dir)
#'
#' #call VUE.exe; default args
#' vue_convert(vrl_files2)
#'
#' #run again and overwrite
#' vue_convert(vrl_files2, overwrite = TRUE)
#' 
#' #run again without progress bars
#' vue_convert(vrl_files2, overwrite = TRUE, show_progress = FALSE)
#'
#'
#' #change output directory
#' new_dir <- file.path(temp_dir, "testdir")
#' if(!dir.exists(new_dir)) dir.create(new_dir)
#'
#' #write to new directory
#' vue_convert(vrl_files2, out_dir = new_dir)
#'
#'
#' #multiple source folders
#' #make new folder for each vrl file inside temp directory
#' new_dir2 <- file.path(temp_dir, 
#'                      "testdir2", 
#'                       seq_along(vrl_files2))
#' for(i in 1:length(new_dir2)){
#'   if(!dir.exists(new_dir2[i])) dir.create(new_dir2[i], recursive = TRUE)
#' }
#'
#' #redistribute files
#' vrl_files3 <- file.path(new_dir2, basename(vrl_files2))
#' file.copy(vrl_files2, vrl_files3)
#'
#' #write each CSV file to same location as corresponding VRL (full path input)
#' vue_convert(vrl_files3)
#' 
#' #same but use input dir only and overwrite = TRUE
#' vue_convert(dirname(vrl_files3), overwrite = TRUE)
#' 
#' #same but write all CSV files to new location
#' new_dir3 <- file.path(temp_dir, "testdir3")
#' if(!dir.exists(new_dir3)) dir.create(new_dir3)
#'
#' vue_convert(vrl_files3, out_dir = new_dir3)
#'
#' #same but use input dir only and recursive = TRUE
#' vue_convert(src = file.path(temp_dir, "testdir2"), 
#'              out_dir = new_dir3,
#'              overwrite = TRUE,
#'              recursive = TRUE)
#'
#' }
#'
#'
#'@export
vue_convert <- function(src, 
                        out_dir = NULL,
                        overwrite = FALSE, 
                        recursive = FALSE,
                        vue_exe_path = NULL,
                        skip_pattern = "-RLD_",
                        show_progress = TRUE,
                        diagn = FALSE){
 
  # Supported input file extensions (not case sensitive)
  supported_ext <- "vrl"
  
  # Basic input checks
  stopifnot("Input 'src' must be character" = is.character(src))
  stopifnot("Input 'out_dir' must be character" = 
              is.character(out_dir) | is.null(out_dir))
  stopifnot("Input 'overwrite' must be TRUE or FALSE" = is.logical(overwrite))
  stopifnot("Input 'recursive' must be TRUE or FALSE" = is.logical(recursive))
  stopifnot("Input 'vue_exe_path' must be character or NULL" = 
              is.character(vue_exe_path) | is.null(vue_exe_path))
  stopifnot("Input 'skip_pattern' must be character" =  
              is.character(skip_pattern) | is.null(skip_pattern))
  stopifnot("Input 'show_progress' must be TRUE or FALSE" = 
              is.logical(show_progress))
  stopifnot("Input 'diagn' must be TRUE or FALSE" = is.logical(diagn))
  
  # Check path to vue.exe and get (valid) command arg for system2 call
  vue_cmd <- check_vue(vue_exe_path)
  
  # Check if src exists and stop if any missing
  #  note file.exists checks files and folders (does not distinguish)
  src_in <- src
  src <- normalizePath(src, mustWork = FALSE)
  src_exists <- file.exists(src) 
  
  if(any(src_exists == FALSE)) stop("Input 'src' not found: \n ",
                                    paste(src[src_exists == FALSE], 
                                          collapse = "\n "))
  
  # Determine src input type (dir or file)
  #  if not dir, assume file since previous step confirms existence
  src_type <- ifelse(dir.exists(src), "dir", "file")
  
  
  # Stop if mix of dir and file
  if(all(c("file", "dir") %in% src_type)){
    stop("Input arg 'src' must contain one or more ",
         "files or directories, but not both.", 
         call. = FALSE)  
  }  
  

  # Check or set output directory
  out_dir_in <- out_dir
  
  #  if null, set to same directory of src input
  if(is.null(out_dir)){
    
    out_dir <- if(all(src_type == "file")) dirname(src)  else (src) 
               
    #  if specified, check length, existence, type  
  } else {
    
    # Check length
    #  must either be NULL (use source dir for each file)
    #  scalar (same for all input files), 
    #  or same length at src (specific to each input file or folder)
    if(!(length(out_dir) == 1 | 
         length(out_dir) == length(src))){
      stop("Input 'out_dir' must be NULL, length one, or same length at 'src'.")
    }
    
    # Check out_dir existence
    out_dir <- normalizePath(out_dir, mustWork = FALSE)
    out_dir_exists <- file.exists(out_dir) 
    
    if(any(out_dir_exists == FALSE)) stop("'out_dir' not found: \n ",
                                          paste(out_dir[out_dir_exists == FALSE], 
                                                collapse = "\n "))
    
    # Check out_dir type (dir or file)
    if(any(dir.exists(out_dir) == FALSE)) stop("'out_dir' cannot contain ",
                                               "full paths to files; ",
                                               "only directories.")
    
    # Replicate if scalar to match length of src
    #  for convenience later, esp. when src is dir and recursive = TRUE
    if(length(out_dir) == 1) out_dir <- rep(out_dir, length(src))
    
  }  
  
  
  # Get file names if src is a directory and assemble "files to process"
  if(all(src_type == "dir")){
    
    ftp <- data.table::data.table(
      src_dir = src,
      out_dir = out_dir)
    
    ftp <- ftp[, 
               list(
                 src_file = list.files(src_dir, 
                                       full.names = TRUE, 
                                       recursive = recursive, 
                                       pattern = paste(
                                         paste0("\\.", supported_ext,"$"),
                                         collapse = "|"),
                                       ignore.case = TRUE)),
               by = c("src_dir", "out_dir")]
  
    # Remove files containing skip_pattern
    ftp <- ftp[grepl(skip_pattern, basename(src_file)) == FALSE]    

    # Warn and return empty if no files found 
    if(nrow(ftp) == 0) {
      warning(paste0("No VRL files were ",
                     "found in 'src': \n ",
                     paste(src, 
                           collapse = "\n ")))
      
      return(NA_character_)
    }
    
  } else {
    
    ftp <- data.table::data.table(
      src_dir = dirname(src),
      src_file = src,
      out_dir = out_dir)
    
  }

  # Set output path(s) and file name(s)
  ftp[ , out_file := file.path(out_dir, 
                               gsub(paste(paste0("\\.", supported_ext, "$"),
                                          collapse = "|"),
                                    ".csv", 
                                    basename(src_file),
                                    ignore.case = TRUE))]
  
  
  # Ignore existing files if overwrite is false
  ftp[ , out_file_exists := (file.exists(out_file))]
  ftp[ , src_to_convert := !out_file_exists | overwrite]
  
  
  # Convert files
  
  # Loop through files so that progress can be displayed
  message("Converting ", sum(ftp$src_to_convert), 
          " VRL file(s) to VUE CSV...")
  
  if(show_progress & sum(ftp$src_to_convert) > 0){
    pb <- txtProgressBar(max = sum(ftp$src_to_convert), style = 3)
  }
  
  # Preallocate vector for system messages
  ftp[, `:=`(vue_error = NA_character_,
             vue_status = NA_character_)]
  
  for(i in 1:nrow(ftp)){
    
    if(!ftp$src_to_convert[i]) next
    
    #if file exists, remove folder
    if(ftp$out_file_exists[i]) rem_i <- file.remove(ftp$out_file[i])
    
    #invoke VUE command
    
    #set overwrite option (VUE only)
    overwrite_file <- ifelse(overwrite,"--overwrite-file","")
    
    vue_call <- with(ftp, 
                     c("--convert-files",
                       paste0("--output-path ", out_dir[i]),
                       overwrite_file,  
                       paste0(" --output-format csv ", src_file[i])))
    
    # suppressing warnings here because error messages are returned as 
    #  strings with warning; we want to capture both the error msg and warning
    msg_i <- suppressWarnings(system2(command = vue_cmd, 
                                      args = vue_call, 
                                      stdout = TRUE, 
                                      stderr = TRUE))
    
    # if warning; make new warning with error message
    if(!is.null(attr(msg_i, "status"))){
      ftp[i , `:=`(vue_error = msg_i[1],
                   vue_status = attr(msg_i, "status"))]
      
      if(diagn){
        
        warning("\nError converting ", basename(ftp$src_file[i]), " :\n",
                "\trunning command'", vue_cmd, " ", vue_call, 
                " had status ", ftp$vue_status[i], ":\n",
                ftp$vue_error[i])
      }
    }
    
    if(show_progress) setTxtProgressBar(pb, 
                                        value = sum(ftp$src_to_convert[1:i]))
    
  } #end i
  
  message("\n")
  
  # Confirm files were written and message summary
  ftp[src_to_convert == TRUE, 
      written := file.exists(out_file)]
  
  message("Done. ",
          sum(ftp[src_to_convert == TRUE]$written, na.rm = TRUE), " of ", 
          nrow(ftp), 
          " file(s) written to disk.")
  
  # Make message for skipped files
  if(any(ftp$src_to_convert == FALSE)){
    with(ftp, 
         message("\n! ", sum(src_to_convert == FALSE), 
                 " file(s) skipped (already exists &",
                 " 'overwrite' = ", overwrite, "):\n   ",
                 paste(basename(out_file[src_to_convert == FALSE]), 
                       collapse = "\n   "),
                 "\n"))
  } 
  
  # Make message for any files not written that should have been
  if(any(ftp$written == FALSE, na.rm = TRUE)){
    with(ftp[written == FALSE], 
         message("\n! ", sum(written == FALSE), 
                 " file(s) not written due to errors:\n   ",
                 paste(
                   paste0(basename(src_file), 
                          ": (status ", vue_status, 
                          ") ", vue_error), 
                   collapse = "\n   "), 
                 "\n"))
  } 
  
  return(ftp$out_file) 
}


#' Check path to Innovasea program VUE.exe
#' 
#' @param vue_exe_path The full path to \code{VUE.exe}. If \code{NULL}
#'  (default) then the path to VUE.exe must be in the PATH environment variable
#'  of the system. 
#'  
#' @returns Character string with command for calling VUE.exe via
#'   \code{system2}'s \code{\link{command}} argument.
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' #use Windows system PATH variable
#' check_vue()
#' 
#' 
#' #use path to directory containing VUE.exe
#' check_vue(vue_exe_path = "C:/Program Files (x86)/VEMCO/VUE")
#' 
#'
#' #use full path to VUE.exe
#' check_vue(vue_exe_path = "C:/Program Files (x86)/VEMCO/VUE/VUE.exe")
#' 
#' }
#'
#' @export
check_vue <- function(vue_exe_path = NULL){
  
  if(is.null(vue_exe_path)) {
    vue_cmd <- "VUE"
    
    if(Sys.which(vue_cmd) == "") stop("VUE.exe not found in system PATH ",
                                       "variable.", 
                                      call. = FALSE)
    
    # Test
    tryCatch(system2(vue_cmd, "--help", stdout = TRUE), 
             error = function(e) stop("VUE.exe could not be called from R.\n",
                                      "Add it to your system PATH variable or ",
                                      "specify the full path via the input ",
                                      "argument 'vue_exe_path'."), 
                                      call. = FALSE)
    
  } else {
    
    # Remove vue.exe from vue_exe_path if present
    vue_exe_dir <- ifelse(grepl("vue.exe$", 
                                vue_exe_path, 
                                ignore.case = TRUE),
                          dirname(vue_exe_path),
                          vue_exe_path)
    
    vue_exe_file <- file.path(vue_exe_dir, "VUE.exe")
    
    # Check path to VUE.exe
    if(!file.exists(vue_exe_file)) stop("VUE.exe not found at specified ",
                                        "path.", call. = FALSE)
    
    vue_cmd <- vue_exe_file
    
    # Test
    tryCatch(system2(vue_cmd, "--help", stdout = TRUE), 
             error = function(e) stop("VUE.exe was found but could not be ",
                                      "called from R. (Reason unknown.)", 
                                      call. = FALSE)
    )
  }
  
  return(vue_cmd)
}

#' Get version of local installation of Innovasea program VUE.exe
#' 
#' @param vue_exe_path The full path to \code{VUE.exe}. If \code{NULL}
#'  (default) then the path to VUE.exe must be in the PATH environment variable
#'  of the system. See \code{\link{check_vue}}.
#'   
#' @returns 
#' A list with \code{version} (version number) and \code{long_version} (full 
#' string returned by VUE.exe).
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' #use if VUE.exe in Windows system PATH variable
#' get_local_vue_version()
#'
#' #or specify path to VUE.exe
#' get_local_vue_version(vue_exe_path = 
#'                         "C:/Program Files (x86)/Vemco/VUE")
#'                         
#' }
#' 
#' @export
get_local_vue_version <- function(vue_exe_path = NULL){
  
  # Check path to vue.exe and get (valid) command arg for system2 call
  vue_cmd <- check_vue(vue_exe_path)
  
  # Get version via system call (Windows only)
  os <- Sys.info()['sysname']
  
  if(os == "Windows"){
    
    exe_path <- normalizePath(Sys.which(vue_cmd))
    
    ps_command <- paste0("(Get-Command -ErrorAction Stop '",
                         exe_path,
                         "').FileVersionInfo.FileVersion")
    
    vue_version <- system2("powershell.exe", 
                           args = c("-NoProfile", 
                                    "-ExecutionPolicy", 
                                    "Bypass", 
                                    "-Command", 
                                    ps_command), 
                           stdout = TRUE)
    
  } else {
    stop("VUE version information is not available for operating system \"", 
         os, "\".")
  }
  
  vue_version_out <- list(version = strsplit(vue_version, " \\(")[[1]][1],
                             long_version = vue_version)

  
  return(vue_version_out)
}
