#' @title 
#' Read data from a GLATOS workbook
#' 
#' @description
#' Read data from a GLATOS workbook (xlsm file) and save it as a glatos_workbook object--
#' a named list with data about the workbook (metadata), receivers and 
#' transmitters (instruments), and tagged animals.
#'
#' @param workbook A character string with path and name of workbook in 
#'  standard GLATOS format (*.xlsm). If only file name is given, then it is 
#'  assumed that file is located in the working directory.
#'  
#' @param wb_version An optional character string with 
#'  the workbook version number. If NULL (default value) then version will be
#'  determined by evaluating workbook structure. The only allowed values 
#'  are \code{NULL} and \code{"1.3"}. Any other values will trigger an error.
#'
#' @param read_all If TRUE, then all columns and sheets
#'  (e.g., user-created "project-specific" columns or sheets) in the workbook
#'  will be imported. If FALSE (default value) then only columns in the 
#'  standard GLATOS workbook will be imported (project-specific columns will 
#'  be ignored.)
#'
#' @details
#' If \code{read_all = TRUE} then the type of data in each user-defined 
#' column will be 'guessed' by read_excel; this may throw some warnings.
#' 
#' @return A list of class \code{glatos_workbook} with three elements::
#' \describe{
#'   \item{metadata}{A list (project code, principal investigator, contact)}
#'   \item{instruments}{A data frame with instrument data (receivers, 
#'     transmitters)}
#'   \item{animals}{A data frame with animal data}
#' }
#'
#' @author C. Holbrook (cholbrook@usgs.gov) 
#'
#' @examples
#' #get path to example GLATOS Data Workbook
#' wb_file <- system.file("extdata", 
#'   "SMRSL_GLATOS_20140828.xlsm",package="glatos")
#' wb <- read_glatos_workbook(wb_file)
#'
#' @export
read_glatos_workbook <- function(workbook, wb_version=NULL, read_all=FALSE) {

  #Function to set timezone of POSIXct objects loaded with read_excel
  # -read_excel assumes all timestamps are in UTC, so POSIX data in other 
  # -timezones will be wrong. Use this function to correct time zone errors.
  fix_tz <- function(x,tz) {
    #check class
    if(!inherits(x, "POSIXct")) stop(paste0(x," is not a POSIXct object."))
    #check time zone; must be UTC for this conversion to be correct
    if(attr(x,"tzone") != "UTC") stop(paste0(x," x tzone must be UTC."))
    
    #coerce to text, then back to POSIX with correct tz
    x <- as.POSIXct(format(x), tz)
    return(x)
  }
  

  #Function to define excel cell ranges to import based on number of columns
  #define cell range to import; see range arg in read_excel
  get_excel_column_range <- function (n) {
    col_range <- cellranger::cell_cols(paste0("A:",
                                       paste(LETTERS[c(n%/%26,n%%26)],
                                             collapse="")))
  }
  
  
  #Read workbook-----------------------------------------------------------
  
  #Get version-specific workbook specifications
  data(workbook_specs)
  
  
  #Get sheet names
  sheets <- tolower(readxl::excel_sheets(workbook))
  
    
  #Identify workbook version
  ##TODO: expand workbook version matching to use column names in each sheet
  id_workbook_version <- function(workbook, sheets){
    if(all(names(workbook_specs$v1.3) %in% sheets)) { 
      return("1.3") 
    } else {
      stop("Workbook version could not be identified.")
    }
  }
  
  if(is.null(wb_version)) {
    wb_version <- id_workbook_version(workbook, sheets)
  } else if (!(paste0("v",wb_version) %in% names(workbook_specs))) {
    stop(paste0("Workbook version ",wb_version," is not supported."))
  }

  #define column range for each sheet
  col_range <- list()
  col_range[names(workbook_specs[[paste0("v",wb_version)]])] <- NA 
  col_range <- lapply(col_range, function(x) NULL)
  
  if(!read_all) {
    col_range <- lapply(workbook_specs[[paste0("v",wb_version)]], 
                        function(x) get_excel_column_range(nrow(x)))
  }
  
  wb <- list() #preallocate
  
  if(read_all)  wb[sheets] <- NA #add element for each sheet
  
  if (wb_version == "1.3") {
      wb[names(workbook_specs$v1.3)] <- NA
      
      #Get project data
      tmp <- readxl::read_excel(workbook, sheet = "Project", skip = 1, 
                                col_names=FALSE, col_types = "text")
      tmp <- data.frame(tmp, stringsAsFactors=F)
      
      wb$project <- list(project_code = tmp[1,2],
                          principle_investigator = tmp[2,2],
                          pi_email = tmp[3,2],
                          source_file=basename(workbook),
                          wb_version="1.3",
                          created=Sys.time())      
  
      #Read all sheets except project
      if(read_all) { 
        sheets_to_read <- sheets
      } else {
        sheets_to_read <- names(workbook_specs[[paste0("v", wb_version)]])
      }
      sheets_to_read <- setdiff(sheets_to_read, "project") #exclude project
      
      for(i in 1:length(sheets_to_read)){
        spec_i <- workbook_specs[[paste0("v", wb_version)]][[sheets_to_read[i]]]
        if(read_all) {
          #read one row to get dimension
          tmp <- readxl::read_excel(workbook, 
            sheet = match(sheets_to_read[i], tolower(sheets)), 
            skip = 1, n_max = 1, 
            range = col_range[[sheets_to_read[i]]])
          #identify new columns to add
          if (ncol(tmp) > nrow(spec_i)) {
            new_cols <- (nrow(spec_i) + 1):ncol(tmp)
            spec_i[new_cols,] <- NA #add new
            spec_i$type[new_cols] <- rep("guess", length(new_cols))
            spec_i$name[new_cols] <- names(tmp)[new_cols]
          }
        }
      
        # - note 'date' substituted for 'timestamp' in type column
        tmp <- readxl::read_excel(workbook, 
          sheet = match(sheets_to_read[i], tolower(sheets)), 
          skip = 1, 
          range = col_range[[sheets_to_read[i]]],
          col_types = gsub("timestamp","date", spec_i$type))
        date_cols <- which(spec_i$type == "date")
        for(j in date_cols) tmp[,j] <- as.Date(data.frame(tmp)[,j])
        names(tmp) <- tolower(names(tmp))
        wb[[sheets_to_read[i]]] <- data.frame(tmp)
   
      }
      
      #fix time zone errors in deployment
      dep_timezones <- unique(wb$deployment$glatos_timezone)
      for(i in 1:length(dep_timezones)){ 
        rows_i <- which(wb$deployment$glatos_timezone == dep_timezones[i])
        
        #glatos_deploy_date_time
        wb$deployment$glatos_deploy_date_time[rows_i] <- 
            fix_tz( wb$deployment$glatos_deploy_date_time[rows_i],
            paste0("US/",dep_timezones[i]))
      }
 
      #fix time zone errors in recovery
      rec_timezones <- unique(wb$recovery$glatos_timezone)
      for(i in 1:length(rec_timezones)){ 
        rows_i <- which(wb$recovery$glatos_timezone == rec_timezones[i])
        
        #glatos_recover_date_time 
        wb$recovery$glatos_recover_date_time[rows_i] <- 
          fix_tz( wb$recovery$glatos_recover_date_time[rows_i],
            paste0("US/",rec_timezones[i]))
      }     

      #fix time zone errors in tagging
      tag_timezones <- unique(wb$tagging$glatos_timezone)
      for(i in 1:length(tag_timezones)){ 
        rows_i <- which(wb$tagging$glatos_timezone == tag_timezones[i])
        
        #glatos_release_date_time
        wb$tagging$glatos_release_date_time[rows_i] <- 
          fix_tz( wb$tagging$glatos_release_date_time[rows_i],
            paste0("US/",tag_timezones[i]))
      }          
  }

  #TO DO: cerce to glatos_workbook (e.g., as_glatos_workbook) instead of next
  class(wb) <- c("glatos_workbook","list")
  
  return(wb)
}

