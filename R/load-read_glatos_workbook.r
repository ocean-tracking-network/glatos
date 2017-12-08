#' @title 
#' Read data from a GLATOS workbook
#' 
#' @description
#' Read data from a GLATOS workbook (xlsm file) and return a list of class 
#' \code{glatos_workbook}.
#'
#' @param wb_file A character string with path and name of workbook in 
#'  standard GLATOS format (*.xlsm). If only file name is given, then the 
#'  file must be located in the working directory.
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
#' @return A list of class \code{glatos_workbook} with six elements:
#' \describe{
#'   \item{metadata}{A list with data about the project.}
#'   \item{animals}{A data frame with data about tagged animals.}
#'   \item{receivers}{A data frame with data about receiver stations.}
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
read_glatos_workbook <- function(wb_file, wb_version = NULL, 
  read_all = FALSE) {

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
  data(glatos_workbook_schema)
  
  
  #Get sheet names
  sheets <- tolower(readxl::excel_sheets(wb_file))
  
    
  #Identify workbook version
  ##TODO: expand workbook version matching to use column names in each sheet
  id_workbook_version <- function(wb_file, sheets){
    if(all(names(glatos_workbook_schema$v1.3) %in% sheets)) { 
      return("1.3") 
    } else {
      stop("Workbook version could not be identified.")
    }
  }
  
  if(is.null(wb_version)) {
    wb_version <- id_workbook_version(wb_file, sheets)
  } else if (!(paste0("v",wb_version) %in% names(glatos_workbook_schema))) {
    stop(paste0("Workbook version ",wb_version," is not supported."))
  }

  #define column range for each sheet
  col_range <- list()
  col_range[names(glatos_workbook_schema[[paste0("v",wb_version)]])] <- NA 
  col_range <- lapply(col_range, function(x) NULL)
  
  if(!read_all) {
    col_range <- lapply(glatos_workbook_schema[[paste0("v",wb_version)]], 
                        function(x) get_excel_column_range(nrow(x)))
  }
  
  wb <- list() #preallocate
  
  if(read_all)  wb[sheets] <- NA #add element for each sheet

  #-Workbook v1.3--------------------------------------------------------------  
  if (wb_version == "1.3") {
    wb[names(glatos_workbook_schema$v1.3)] <- NA
    
    #Get project data
    tmp <- readxl::read_excel(wb_file, sheet = "Project", skip = 1, 
                              col_names=FALSE, col_types = "text")
    tmp <- data.frame(tmp, stringsAsFactors=F)
    
    wb$project <- list(project_code = tmp[1,2],
                        principle_investigator = tmp[2,2],
                        pi_email = tmp[3,2],
                        source_file=basename(wb_file),
                        wb_version="1.3",
                        created=Sys.time())      

    #Read all sheets except project
    if(read_all) { 
      sheets_to_read <- sheets
    } else {
      sheets_to_read <- names(glatos_workbook_schema[[paste0("v", wb_version)]])
    }
    sheets_to_read <- setdiff(sheets_to_read, "project") #exclude project
    
    for(i in 1:length(sheets_to_read)){
      schema_i <- glatos_workbook_schema[[
                  paste0("v", wb_version)]][[sheets_to_read[i]]]
      if(read_all) {
        #read one row to get dimension
        tmp <- readxl::read_excel(workbook, 
          sheet = match(sheets_to_read[i], tolower(sheets)), 
          skip = 1, n_max = 1, 
          range = col_range[[sheets_to_read[i]]])
        #identify new columns to add
        if (ncol(tmp) > nrow(schema_i)) {
          new_cols <- (nrow(schema_i) + 1):ncol(tmp)
          schema_i[new_cols,] <- NA #add new
          schema_i$type[new_cols] <- rep("guess", length(new_cols))
          schema_i$name[new_cols] <- names(tmp)[new_cols]
        }
      }
    
      # - substitute base R type names for readxl column type names
      schema_i$type <- gsub("character", "text", schema_i$type)
      tmp <- readxl::read_excel(wb_file, 
        sheet = match(sheets_to_read[i], tolower(sheets)), 
        skip = 1, 
        range = col_range[[sheets_to_read[i]]],
        col_types = gsub("Date|POSIXct", "date", schema_i$type))
      tmp <- data.frame(tmp)
      
      #coerce dates to Date
      date_cols <- which(schema_i$type == "Date")
      for(j in date_cols) tmp[,j] <- as.Date(data.frame(tmp)[,j])
      names(tmp) <- tolower(names(tmp))
      
      #fix time zone errors
      timestamp_cols <- which(schema_i$type == "POSIXct")
      refcol_cols <- timestamp_cols[grep("REFCOL", schema_i$arg[timestamp_cols])]
      #function to construct time zone string from reference column tmp
      REFCOL <- function(x) {
        col_x <- gsub("^REFCOL\\(|)$", "", x)
        x2 <- tmp[, col_x]
        utc_rows <- tolower(x2) %in% c("utc", "gmt")
        x2[utc_rows] <- "UTC"
        x2[!utc_rows] <- with(tmp, paste0("US/", x2[!utc_rows]))
        return(x2)
      }
      for(j in refcol_cols) {
        tz_cmd <- gsub("^tz=","",schema_i$arg[j])
        tzone_j <- REFCOL(tz_cmd)
        tzones <- unique(tzone_j)
        for(k in tzones){
          tmp[k == k, j] <- fix_tz(tmp[k == k, j], k)
        } #end k
        attr(tmp[, j], "tzone") <- "UTC" 
      } #end j
      
      wb[[sheets_to_read[i]]] <- data.frame(tmp)
    }
          
    
    #merge to glatos_workbook list object
    ins_key <- list(by.x = c("glatos_project", "glatos_array", "station_no",
        "consecutive_deploy_no", "ins_serial_no"), 
      by.y = c("glatos_project", "glatos_array", "station_no", 
        "consecutive_deploy_no", "ins_serial_number"))
    wb2 <- with(wb, list(
                  metadata = project,
                    animals = tagging,
                  receivers = merge(deployment,
                    recovery[, unique(c(ins_key$by.y, 
                      setdiff(names(recovery), names(deployment))))],
                    by.x = c("glatos_project", "glatos_array", "station_no",
                      "consecutive_deploy_no", "ins_serial_no"), 
                    by.y = c("glatos_project", "glatos_array", "station_no", 
                      "consecutive_deploy_no", "ins_serial_number"), 
                    all.x=TRUE, all.y=TRUE)
                  ))
    #add location descriptions
    wb2$receivers <- with(wb2, merge(receivers, wb$locations,
        by = "glatos_array"))
      
  }

  #-end v1.3----------------------------------------------------------------
  
  #TO DO: cerce to glatos_workbook (e.g., as_glatos_workbook) instead of next
  class(wb2) <- c("glatos_workbook","list")
  
  return(wb2)
}

