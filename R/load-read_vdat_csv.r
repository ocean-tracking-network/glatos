#' Read data from an Innovasea Fathom VDAT CSV file
#'
#' Read data from an Innovasea Fathom VDAT CSV file
#'
#' @param src A character string with path and name of an Innovasea VDAT CSV
#'   detection file. If only file name is given, then the file must be located
#'   in the working directory.
#'
#' @param record_types An optional vector of character strings with names of
#'   record types to read from the file. E.g., "DET" for detection records.
#'   Default (\code{NULL}) will read all record types present in input CSV
#'   \code{src}.
#'
#' @param show_progress Optional argument passed to \code{data.table::fread}'s
#'   \code{showProgress}.
#'
#' @details Reading is done via \code{\link[data.table]{fread}}.
#'
#' @details All timestamp columns are assumed to be in UTC and are assigned
#'   class \code{POSIXct}. The internal value of timestamps will include
#'   fractional seconds but the printed value (i.e., displayed or written to
#'   file) will be truncated according to \code{options()$digits.secs}. By
#'   default (\code{options()$digits.secs = NULL}), values are truncated (i.e.,
#'   rounded down) to the nearest second. To maintain the full resolution
#'   present in the input Fathom CSV file, set \code{options(digits.secs = 6)}.
#'
#' @return A list of class \code{vdat_list} with one named element for each
#'   record type and attributes: \code{fathom_csv_format_version} with version
#'   of the input Fathom CSV format; \code{source} with version of
#'   VDAT.exe used to create the input file.
#'
#'
#' @author C. Holbrook (cholbrook@@usgs.gov)
#'
#' @examples
#' 
#' # Example 1. Read a single file
#' 
#' vrl_file <- system.file("extdata", "detection_files_raw", 
#'   "VR2W_109924_20110718_1.vrl", package="glatos")
#'
#' temp_dir <- tempdir()
#'
#' csv_file <- vdat_convert(vrl_file, out_dir = temp_dir)
#'
#' #utils::browseURL(temp_dir)
#'
#' #read all record types
#' vdat <- read_vdat_csv(csv_file)
#'
#' #read only one record type
#' vdat <- read_vdat_csv(csv_file, record_types = c("DET"))
#'  
#'
#' # Example 2. Read and combine detection records from multiple files
#' 
#' # get two example files
#' vrl_files <- system.file("extdata", "detection_files_raw",
#'   c("VR2W_109924_20110718_1.vrl", 
#'     "HR2-180 461396 2021-04-20 173145.vdat"), 
#'   package="glatos")
#' 
#' csv_files <- vdat_convert(vrl_files, out_dir = temp_dir)
#'
#' 
#' # using dplyr
#' 
#' library(dplyr)
#' 
#' # basic steps: import each to list element, subset DET records, 
#'                add column with source file name, combine.
#'                
#' det2_tbl <- csv_files %>% 
#'              lapply(
#'                function(x) {
#'                  read_vdat_csv(x, record_types = "DET")$DET %>% 
#'                  as_tibble %>%
#'                  mutate(source_file = basename(x))
#'                }) %>% 
#'              bind_rows
#' 
#' 
#' #using data.table
#' 
#' library(data.table)
#' 
#' det2_dt <- rbindlist(
#'              lapply(csv_files, 
#'                function(x) {
#'                 read_vdat_csv(x, record_types = "DET")$DET[, 
#'                   source_file := basename(x)]
#'              }))
#'              
#' \dontrun{
#' 
#' # get current version of digits.secs
#' op_digits.secs <- options()$digits.secs
#'   
#' # set digits.secs = NULL (default, truncates to nearest second)
#' options(digits.secs = NULL)
#' 
#' # note truncation to nearest second
#' vdat$DET$Time[2]
#' 
#' # set digits.secs to see fractional seconds
#' options(digits.secs = 6)
#' 
#' # note fractional seconds
#' vdat$DET$Time[2]
#' 
#' # return to default values
#' options(digits.secs = op_digits.secs)
#' 
#' # or specify via format %OSn e.g., when writing to disk or external database
#' # see ?strptime
#' format(vdat$DET$Time[2], format = "%Y-%m-%d %H:%M:%OS6")
#' 
#' }
#'
#' @export
read_vdat_csv <- function(src, 
                          record_types = NULL,
                          show_progress = FALSE){
  
  #Check if exists
  if(!file.exists(src)){
    warning("File not found: ", src)
    return()
  }
  
  #Identify vdat csv format version and vdat.exe version that created input csv
  vdat_header <- data.table::fread(file = src, nrows = 1L, header = FALSE)
  
  #Check if fathom csv format (error if looks like VUE export format)
  if(vdat_header$V1[1] == "VEMCO DATA LOG") {
    # Set column names
    src_version <- data.table::data.table(fathom_csv = vdat_header$V2[1],
                                          vdat_exe = vdat_header$V3[1])
  } else if (all(c("Receiver", 
                   "Transmitter", 
                   "Transmitter Name", 
                   "Transmitter Serial", 
                   "Sensor Value", 
                   "Sensor Unit", 
                   "Station Name", 
                   "Latitude", 
                   "Longitude") %in% 
             as.character(vdat_header))) {
      stop("Input file appears to be in VUE Export format, which is not ",
           "supported.\n Only Fathom CSV format is supported. \n",
           " Perhaps you want read_vue_detection_csv()?")
    }
  
  #Read all data into character vector (like readLines)
  vdat_txt <- data.table::fread(file = src, skip = 2, header = FALSE, 
                                sep = NULL, col.names = "txt", 
                                showProgress = show_progress)
  
  #Identify record type of each row
  vdat_txt[ , record_type := data.table::fread(file = src, 
                                               skip = 2, 
                                               header = FALSE, 
                                               sep = ",", 
                                               select = 1, 
                                               fill = TRUE, 
                                               showProgress = show_progress)]

  #Drop _DESC from headers
  vdat_txt[ , record_type := gsub("_DESC$", "", record_type)]  
  
  
  #Get record identifiers from csv file
  csv_record_types <- unique(vdat_txt$record_type)


  if(is.null(record_types)) {
    
    record_types <- csv_record_types 

  } else {
    
    #Check if any record_types are not in csv
    unknown_record_types <- setdiff(record_types, csv_record_types)
    
    if(length(unknown_record_types) > 0) stop("The following input ",
                                       "'record_types' ",
                                       "were not found in CSV file: \n\t",
                                       paste(unknown_record_types, 
                                             collapse = ", "))
  }  
    
  #Drop data types not requested by user
  vdat_txt <- vdat_txt[record_type %in% record_types]
  
  #Split into list elements by record type
  vdat_list <- split(vdat_txt, 
                    by = "record_type",
                    keep.by = FALSE)
  
  data(vdat_csv_schema)
  
  vdat_csv_schema <- vdat_csv_schema[[paste0("v", src_version$fathom_csv)]]
  
 
  # Preallocate list; element = record type
  vdat <- setNames(object = vector("list", length(vdat_list)),
                   nm = names(vdat_list))
  
  for(i in 1:length(vdat)){ 
    
    # fread has issues with numerical precision (e.g., 'Time Correction (s)')
    #  so read all columns as character then coerce 
    vdat[[i]] <- data.table::fread(
                   text = paste0(c(vdat_list[[i]]$txt,""), 
                                 collapse = "\n"), 
                   sep = ",", na.strings = "",
                   colClasses = "character",
                   header = TRUE,
                   drop = 1, 
                   showProgress = show_progress)
    
    # Coerce to class
    schema_i <- vdat_csv_schema[[names(vdat[i])]]
    
    # numeric
    numeric_cols <- schema_i$name[schema_i$type == "numeric"]
    
    if(length(numeric_cols) > 0) {
      vdat[[i]][, (numeric_cols) := lapply(.SD, as.numeric), 
                .SDcols = numeric_cols]
    }
    
    # POSIXct
    timestamp_cols <- schema_i$name[schema_i$type == "POSIXct"]
    
    if(length(timestamp_cols) > 0){
      vdat[[i]][, (timestamp_cols) := lapply(.SD, 
                                             function(x) 
                                               lubridate::fast_strptime(
                                                 x, 
                                                 format = "%Y-%m-%d %H:%M:%OS",                     
                                                 lt = FALSE)), 
                .SDcols = timestamp_cols] 
    }
    
    # Assign class
    new_class <- c(paste0("vdat_", names(vdat[i])), class(vdat[[i]]))
    data.table::setattr(vdat[[i]], "class", new_class)
    
  } # end i
  
  #Assign class and other attributes
  vdat_list <- structure(vdat, 
                         class = c("vdat_list", class(vdat)),
                         fathom_csv_version = src_version$fathom_csv,
                         source = src_version$vdat_exe)
  
  return(vdat_list)
}  
  