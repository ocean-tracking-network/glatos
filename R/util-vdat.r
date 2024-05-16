#' Convert an Innovasea VRL or VDAT file to a Fathom CSV file
#'
#' Use Innovasea's VDAT command line program VDAT.exe (distributed with Fathom
#' Connect software) to make a CSV file containing data from a VRL or VDAT file
#' in Fathom CSV format.
#'
#' @param src Character string with path and name of a detection file
#'  (\code{VDAT} or \code{VRL}), a vector of file names, or a directory
#'  containing files. If only file name is given, then the file must be located
#'  in the working directory.
#'
#' @param out_dir Optional character string with directory where CSV files will
#'  be written. If \code{NULL} (default) then each file will be written to the
#'  same directory as its source file.
#'
#' @param output_format Character string with output format. Options are:
#'  \code{"csv.fathom"} (default) writes a single CSV file (for each input file)
#'  with multiple record types interleaved; \code{"csv.fathom.split"} writes a
#'  folder (for each input file) containing a separate CSV for each record type.
#'
#' @param overwrite Logical. If \code{TRUE}, output CSV file(s) will overwrite
#'  existing CSV file(s) with same name in \code{out_dir}. If \code{FALSE}
#'  (default), any output files that already exist in \code{out_dir} will be
#'  skipped, with warning.
#'
#' @param recursive Logical. If \code{TRUE} and \code{src} is a directory, then
#'  all VRL/VDAT files in all subdirectories of \code{src} will be converted.
#'  Default is \code{FALSE}. Ignored if \code{src} is a not directory.
#'
#' @param export_settings (NOT YET IMPLEMENTED). Placeholder for future
#'  specification of other options available via Fathom Data Export app. (E.g.,
#'  'Data Types to Include', 'Data Filter', 'Filename Suffix', 'Time Offset in
#'  Hours', 'Split CSV by UTC Day'.)
#'
#' @param vdat_exe_path The full path to \code{VDAT.exe}. If \code{NULL}
#'  (default) then the path to VDAT.exe must be in the PATH environment variable
#'  of the system. See \code{\link{check_vdat}}.
#'
#' @param skip_pattern A regular expression used to exclude files from
#'  processing. Default value \code{"-RLD_"} will exclude "RAW LOG" files.
#'  Ignored if \code{src} contains file names.
#'
#' @param show_progress Logical. Indicates if progress bar should be shown.
#'
#' @param diagn Logical. Indicates if errors or warnings message (from vdat.exe)
#'  should be displayed (default = \code{FALSE}).
#'
#' @details If \code{src} is a directory, then all source files in that directory
#'  (including all subdirectories if \code{recursive = TRUE}) with supported
#'  extensions (currently \code{"vrl"} and \code{"vdat"}) will be converted to
#'  CSV. Otherwise, only those files specified in \code{src} will be converted.
#'
#' @details Conversion is done by system call to the Innovasea program
#'  \code{VDAT.exe} (included with Innovasea's Fathom Connect software;
#'  available at \url{https://support.fishtracking.innovasea.com/s/downloads}).
#'  VDAT.exe must be available at the location specified by \code{vdat_exe_path}
#'  or via system PATH environment variable. See also
#'  \code{\link{check_vdat}}.
#'
#' @section Output:
#'
#'  Output depends on \code{output_format}:
#'
#'  If \code{output_format = "csv.fathom"}: A comma-separated-values (CSV) text
#'  file in Innovasea's Fathom CSV format for each input VRL/VDAT file. Each CSV
#'  is named the same (except for extension) as the source file (e.g.,
#'  \code{VR2W_109924_20110718_1.csv}).
#'
#'  If \code{output_format = "csv.fathom.split"}: A directory containing a set
#'  of CSV files for each input VRL/VDAT file. Each CSV file contains data for
#'  one record type in Innovasea's Fathom CSV format and each file name matches
#'  the corresponding record type (e.g, \code{BATTERY.csv}, \code{DET.csv},
#'  \code{HEALTH_VR2W.csv}). Each directory is named the same (except for
#'  extension) as the source file (e.g.,
#'  \code{VR2W_109924_20110718_1.csv-fathom-split}).
#'
#' @return A character string or vector with the full path and name of each
#'  output file, including files that were skipped (when output file exists and
#'  \code{overwrite = FALSE}).
#'
#' @note Tested on VDAT version 3.4.1.
#'
#' @author C. Holbrook, \email{cholbrook@@usgs.gov}
#'
#' @examples
#' \dontrun{
#'
#' # Check vdat.exe
#' check_vdat()
#'
#' # all examples below assume path to VDAT.exe is in system PATH environment
#' # variable. If not (you get an error), add input argument 'vdat_exe_path'
#' # with path directory with VDAT.exe.
#' # e.g.,
#' # vdat_convert(vrl_files,
#' #             vdat_exe_path = "C:/Program Files/Innovasea/Fathom")
#'
#' # get path to example VRL files in glatos
#' vrl_files <- system.file("extdata", "detection_files_raw",
#'   c(
#'     "VR2W_109924_20110718_1.vrl",
#'     "VR2W180_302187_20180629_1.vrl",
#'     "VR2AR_546310_20190613_1.vrl",
#'     "VR2Tx_480022_20190613_1.vrl"
#'   ),
#'   package = "glatos"
#' )
#'
#' # copy to temp_dir
#' temp_dir <- tempdir()
#' vrl_files2 <- file.path(temp_dir, basename(vrl_files))
#' file.copy(vrl_files, vrl_files2)
#'
#' # uncomment to open in file browser
#' # utils::browseURL(temp_dir)
#'
#' # call VDAT.exe; default args
#' vdat_convert(vrl_files2)
#'
#' # run again and overwrite
#' vdat_convert(vrl_files2, overwrite = TRUE)
#'
#' # run again without progress bars
#' vdat_convert(vrl_files2, overwrite = TRUE, show_progress = FALSE)
#'
#' # use split output format
#' vdat_convert(vrl_files2, output_format = "csv.fathom.split")
#'
#' # change output directory
#' new_dir <- file.path(temp_dir, "testdir")
#' if (!dir.exists(new_dir)) dir.create(new_dir)
#'
#' # write to new directory
#' vdat_convert(vrl_files2, out_dir = new_dir)
#'
#'
#' # multiple source folders
#' # make new folder for each vrl file inside temp directory
#' new_dir2 <- file.path(
#'   temp_dir,
#'   "testdir2",
#'   seq_along(vrl_files2)
#' )
#' for (i in 1:length(new_dir2)) {
#'   if (!dir.exists(new_dir2[i])) dir.create(new_dir2[i], recursive = TRUE)
#' }
#'
#' # redistribute files
#' vrl_files3 <- file.path(new_dir2, basename(vrl_files2))
#' file.copy(vrl_files2, vrl_files3)
#'
#' # write each CSV file to same location as corresponding VRL (full path input)
#' vdat_convert(vrl_files3)
#'
#' # same but use input dir only and overwrite = TRUE
#' vdat_convert(dirname(vrl_files3), overwrite = TRUE)
#'
#' # same but write all CSV files to new location
#' new_dir3 <- file.path(temp_dir, "testdir3")
#' if (!dir.exists(new_dir3)) dir.create(new_dir3)
#'
#' vdat_convert(vrl_files3, out_dir = new_dir3)
#'
#' # same but use input dir only and recursive = TRUE
#' vdat_convert(
#'   src = file.path(temp_dir, "testdir2"),
#'   out_dir = new_dir3,
#'   overwrite = TRUE,
#'   recursive = TRUE
#' )
#' }
#'
#' @export
vdat_convert <- function(src,
                         out_dir = NULL,
                         output_format = "csv.fathom",
                         overwrite = FALSE,
                         recursive = FALSE,
                         vdat_exe_path = NULL,
                         skip_pattern = "-RLD_",
                         show_progress = TRUE,
                         diagn = FALSE,
                         export_settings = NULL) {
  # Supported input file extensions (not case sensitive)
  supported_ext <- c("vrl", "vdat")

  # Basic input checks
  stopifnot("Input 'src' must be character" = is.character(src))
  stopifnot(
    "Input 'out_dir' must be character" =
      is.character(out_dir) | is.null(out_dir)
  )
  output_format <- match.arg(output_format,
    choices = c("csv.fathom", "csv.fathom.split")
  )
  stopifnot("Input 'overwrite' must be TRUE or FALSE" = is.logical(overwrite))
  stopifnot("Input 'recursive' must be TRUE or FALSE" = is.logical(recursive))
  stopifnot(
    "Input 'vdat_exe_path' must be character or NULL" =
      is.character(vdat_exe_path) | is.null(vdat_exe_path)
  )
  stopifnot(
    "Input 'show_progress' must be TRUE or FALSE" =
      is.logical(show_progress)
  )
  stopifnot("Input 'diagn' must be TRUE or FALSE" = is.logical(diagn))

  # Check path to vdat.exe and get (valid) command arg for system2 call
  vdat_cmd <- check_vdat(vdat_exe_path)

  # Check if src exists and stop if any missing
  #  note file.exists checks files and folders (does not distinguish)
  src_in <- src
  src <- normalizePath(src, mustWork = FALSE)
  src_exists <- file.exists(src)

  if (any(src_exists == FALSE)) {
    stop(
      "Input 'src' not found: \n ",
      paste(src[src_exists == FALSE],
        collapse = "\n "
      )
    )
  }

  # Determine src input type (dir or file)
  #  if not dir, assume file since previous step confirms existence
  src_type <- ifelse(dir.exists(src), "dir", "file")


  # Stop if mix of dir and file
  if (all(c("file", "dir") %in% src_type)) {
    stop("Input arg 'src' must contain one or more ",
      "files or directories, but not both.",
      call. = FALSE
    )
  }


  # Check or set output directory
  out_dir_in <- out_dir

  #  if null, set to same directory of src input
  if (is.null(out_dir)) {
    out_dir <- if (all(src_type == "file")) dirname(src) else (src)

    #  if specified, check length, existence, type
  } else {
    # Check length
    #  must either be NULL (use source dir for each file)
    #  scalar (same for all input files),
    #  or same length at src (specific to each input file or folder)
    if (!(length(out_dir) == 1 |
      length(out_dir) == length(src))) {
      stop("Input 'out_dir' must be NULL, length one, or same length at 'src'.")
    }

    # Check out_dir existence
    out_dir <- normalizePath(out_dir, mustWork = FALSE)
    out_dir_exists <- file.exists(out_dir)

    if (any(out_dir_exists == FALSE)) {
      stop(
        "'out_dir' not found: \n ",
        paste(out_dir[out_dir_exists == FALSE],
          collapse = "\n "
        )
      )
    }

    # Check out_dir type (dir or file)
    if (any(dir.exists(out_dir) == FALSE)) {
      stop(
        "'out_dir' cannot contain ",
        "full paths to files; ",
        "only directories."
      )
    }

    # Replicate if scalar to match length of src
    #  for convenience later, esp. when src is dir and recursive = TRUE
    if (length(out_dir) == 1) out_dir <- rep(out_dir, length(src))
  }

  # Get file names if src is a directory and assemble "files to process"
  if (all(src_type == "dir")) {
    ftp <- data.table::data.table(
      src_dir = src,
      out_dir = out_dir
    )

    ftp <- ftp[,
      list(
        src_file = list.files(src_dir,
          full.names = TRUE,
          recursive = recursive,
          pattern = paste(
            paste0("\\.", supported_ext, "$"),
            collapse = "|"
          ),
          ignore.case = TRUE
        )
      ),
      by = c("src_dir", "out_dir")
    ]

    # Remove files containing skip_pattern
    ftp <- ftp[grepl(skip_pattern, basename(src_file)) == FALSE]

    # Warn and return empty if no files found
    if (nrow(ftp) == 0) {
      warning(paste0(
        "No VRL or VDAT files were ",
        "found in 'src': \n ",
        paste(src,
          collapse = "\n "
        )
      ))

      return(NA_character_)
    }
  } else {
    ftp <- data.table::data.table(
      src_dir = dirname(src),
      src_file = src,
      out_dir = out_dir
    )
  }

  # File extension (and type) depends on output_format
  output_file_ext <-
    data.table::fcase(
      output_format == "csv.fathom", "csv",
      output_format == "csv.fathom.split", "csv-fathom-split"
    )

  # Set output path(s) and file name(s)
  ftp[, out_file := file.path(
    out_dir,
    gsub(
      paste(paste0("\\.", supported_ext, "$"),
        collapse = "|"
      ),
      paste0(".", output_file_ext),
      basename(src_file),
      ignore.case = TRUE
    )
  )]


  # Ignore existing files if overwrite is false
  ftp[, out_file_exists := (file.exists(out_file))]
  ftp[, src_to_convert := !out_file_exists | overwrite]


  # Convert files

  # Loop through files so that progress can be displayed
  message(
    "Converting ", sum(ftp$src_to_convert),
    " VRL/VDAT file(s) to Fathom CSV..."
  )

  if (show_progress & sum(ftp$src_to_convert) > 0) {
    pb <- txtProgressBar(max = sum(ftp$src_to_convert), style = 3)
  }

  # Preallocate vector for system messages
  ftp[, `:=`(
    vdat_error = NA_character_,
    vdat_status = NA_character_
  )]

  for (i in 1:nrow(ftp)) {
    if (!ftp$src_to_convert[i]) next

    # if file exists, remove
    if (ftp$out_file_exists[i]) rem_i <- file.remove(ftp$out_file[i])

    # invoke fathom command

    # Need to catch warnings and errors here.
    # in some cases warning (...had status 1) is produced by R, in others,
    # an error message is returned to console (!! ERROR: Invalid Argument)
    # or both
    vdat_call <- with(
      ftp,
      paste0(
        "convert --format=", output_format, " \"",
        src_file[i],
        "\" --timec=default --output \"", out_dir[i],
        "\""
      )
    )

    # suppressing warnings here because error messages are returned as
    #  strings with warning; we want to capture both the error msg and warning
    msg_i <- suppressWarnings(system2(
      command = vdat_cmd,
      args = vdat_call,
      stdout = TRUE,
      stderr = TRUE
    ))

    # if warning; make new warning with error message
    if (!is.null(attr(msg_i, "status"))) {
      ftp[i, `:=`(
        vdat_error = msg_i[1],
        vdat_status = attr(msg_i, "status")
      )]

      if (diagn) {
        warning(
          "\nError converting ", basename(ftp$src_file[i]), " :\n",
          "\trunning command'", vdat_cmd, " ", vdat_call,
          " had status ", ftp$vdat_status[i], ":\n",
          ftp$vdat_error[i]
        )
      }
    }

    if (show_progress) {
      setTxtProgressBar(pb,
        value = sum(ftp$src_to_convert[1:i])
      )
    }
  } # end i

  message("\n")

  # Confirm files were written and message summary
  ftp[
    src_to_convert == TRUE,
    written := file.exists(out_file)
  ]

  message(
    "Done. ",
    sum(ftp[src_to_convert == TRUE]$written, na.rm = TRUE), " of ",
    nrow(ftp),
    " file(s) written to disk."
  )

  # Make message for skipped files
  if (any(ftp$src_to_convert == FALSE)) {
    with(
      ftp,
      message(
        "\n! ", sum(src_to_convert == FALSE),
        " file(s) skipped (already exists &",
        " 'overwrite' = ", overwrite, "):\n   ",
        paste(basename(out_file[src_to_convert == FALSE]),
          collapse = "\n   "
        ),
        "\n"
      )
    )
  }

  # Make message for any files not written that should have been
  if (any(ftp$written == FALSE, na.rm = TRUE)) {
    with(
      ftp[written == FALSE],
      message(
        "\n! ", sum(written == FALSE),
        " file(s) not written due to errors:\n   ",
        paste(
          paste0(
            basename(src_file),
            ": (status ", vdat_status,
            ") ", vdat_error
          ),
          collapse = "\n   "
        ),
        "\n"
      )
    )
  }

  return(ftp$out_file)
}


#' Check path to Innovasea program VDAT.exe
#'
#' @param vdat_exe_path The full path to \code{VDAT.exe}. If \code{NULL}
#'  (default) then the path to VDAT.exe must be in the PATH environment variable
#'  of the system.
#'
#' @returns Character string with command for calling VDAT.exe via
#'   \code{system2}'s \code{\link{command}} argument.
#'
#' @examples
#' \dontrun{
#'
#' # use Windows system PATH variable
#' check_vdat()
#'
#'
#' # use path to directory containing VDAT.exe
#' check_vdat(vdat_exe_path = "C:/Program Files/Innovasea/Fathom")
#'
#'
#' # use full path to VDAT.exe
#' check_vdat(vdat_exe_path = "C:/Program Files/Innovasea/Fathom/VDAT.exe")
#' }
#'
#' @export
check_vdat <- function(vdat_exe_path = NULL) {
  if (is.null(vdat_exe_path)) {
    vdat_cmd <- "VDAT"

    if (Sys.which(vdat_cmd) == "") {
      stop("VDAT.exe not found in system PATH ",
        "variable.",
        call. = FALSE
      )
    }
  } else {
    # remove VDAT.exe from vdat_exe_path if present
    vdat_exe_dir <- ifelse(grepl("vdat.exe$",
      vdat_exe_path,
      ignore.case = TRUE
    ),
    dirname(vdat_exe_path),
    vdat_exe_path
    )

    vdat_exe_file <- file.path(vdat_exe_dir, "VDAT.exe")

    # Check path to VDAT.exe
    if (!file.exists(vdat_exe_file)) {
      stop("VDAT.exe not found at specified ",
        "path.",
        call. = FALSE
      )
    }

    vdat_cmd <- vdat_exe_file

    # Check if path can be reached via system call
    if (Sys.which(vdat_cmd) == "") {
      stop("VDAT.exe found but could not be ",
        "reached via system call.",
        call. = FALSE
      )
    }
  }

  return(vdat_cmd)
}


#' Get version of local installation of Innovasea program VDAT.exe
#'
#' @param vdat_exe_path The full path to \code{VDAT.exe}. If \code{NULL}
#'  (default) then the path to VDAT.exe must be in the PATH environment variable
#'  of the system. See \code{\link{check_vdat}}.
#'
#' @returns
#' A list with \code{version} (version number) and \code{long_version} (full
#' string returned by VDAT.exe).
#'
#' @examples
#' \dontrun{
#'
#' # use if VDAT.exe in Windows system PATH variable
#' get_local_vdat_version()
#'
#' # or specify path to VDAT.exe
#' get_local_vdat_version(
#'   vdat_exe_path =
#'     "C:/Program Files/Innovasea/Fathom/VDAT.exe"
#' )
#' }
#'
#' @export
get_local_vdat_version <- function(vdat_exe_path = NULL) {
  # Check path to vdat.exe and get (valid) command arg for system2 call
  vdat_cmd <- check_vdat(vdat_exe_path)

  # invoke VDAT.exe
  vdat_call <- "--version"

  vdat_version <- system2(vdat_cmd, vdat_call, stdout = TRUE)

  if (grepl("^vdat-", vdat_version)) {
    vdat_version_out <- list(
      version = strsplit(vdat_version, "-")[[1]][2],
      long_version = vdat_version
    )
  } else {
    vdat_version_out <- NULL
  }

  return(vdat_version_out)
}


#' Get schema from local installation of Innovsea program VDAT.exe
#'
#' @param vdat_exe_path The full path to \code{VDAT.exe}. If \code{NULL}
#'  (default) then the path to VDAT.exe must be in the PATH environment variable
#'  of the system. See \code{\link{check_vdat}}.
#'
#' @returns
#' Schema (template) of VDAT CSV produced by installed version of VDAT.exe.
#'
#' @examples
#' \dontrun{
#'
#' # use if VDAT.exe in Windows system PATH variable
#' get_local_vdat_schema()
#'
#' # or specify path to VDAT.exe
#' get_local_vdat_schema(
#'   vdat_exe_path =
#'     "C:/Program Files/Innovasea/Fathom/VDAT.exe"
#' )
#' }
#'
#' @export
get_local_vdat_schema <- function(vdat_exe_path = NULL) {
  # Check path to vdat.exe and get (valid) command arg for system2 call
  vdat_cmd <- check_vdat(vdat_exe_path)

  # Invoke VDAT.exe
  vdat_call <- "--format=csv.fathom template"

  vdat_schema <- system2(vdat_cmd, vdat_call, stdout = TRUE)

  vdat_schema_names <- lapply(vdat_schema, function(x) strsplit(x, ",")[[1]][1])
  vdat_schema_list <- lapply(vdat_schema, function(x) strsplit(x, ",")[[1]][-1])

  # Drop _DESC suffix and assign names to each element
  names(vdat_schema_list) <- gsub("_DESC$", "", vdat_schema_names)

  # Drop RECORD TYPE element
  vdat_schema_list["RECORD TYPE"] <- NULL

  return(vdat_schema_list)
}
