#' Read data from a GLATOS project workbook
#'
#' Read data from a GLATOS project workbook (xlsm or xlsx file) and return a
#' list of class `glatos_workbook`.
#'
#' @param wb_file A character string with path and name of workbook in standard
#'   GLATOS format (*.xlsm). If only file name is given, then the file must be
#'   located in the working directory. File must be a standard GLATOS file
#'   (e.g., *xxxxx_GLATOS_YYYYMMDD.xlsm*) submitted via GLATOSWeb Data Portal
#'   <http://glatos.glos.us>.
#'
#' @param wb_version An optional character string with the workbook version
#'   number. If NULL (default value) then version will be determined by
#'   evaluating workbook structure. Currently, the only allowed values are
#'   `NULL` and `"1.3"`. Any other values will trigger an error.
#'
#' @param read_all If TRUE, then all columns and sheets (e.g., user-created
#'   "project-specific" columns or sheets) in the workbook will be imported. If
#'   FALSE (default value) then only columns and sheets in the standard GLATOS
#'   workbook will be imported (project-specific columns will be ignored.)
#'
#' @details In the standard glatos workbook (v1.3), data in workbook sheets
#'   'Deployment', 'Recovery', and 'Location' are merged on columns
#'   'GLATOS_PROJECT', 'GLATOS_ARRAY', 'STATION_NO', 'CONSECUTIVE_DEPLOY_NO',
#'   AND 'INS_SERIAL_NO' to produce the output data frame `receivers`. Data in
#'   workbook sheets 'Project' and 'Tagging' are passed through to new data
#'   frames named 'project' and 'animals', respectively, and data from workbook
#'   sheet 'Proposed' is not included in result. If `read_all = TRUE` then each
#'   sheet in workbook will be included in result.
#'
#' @details Data are read from the input file using
#'   [read_excel][readxl::read_excel] in the 'readxl' package. If `read_all =
#'   TRUE` then the type of data in each user-defined column (and sheet) will be
#'   'guessed' by [read_excel][readxl::read_excel]. Therefore, if `read_all =
#'   TRUE` then the structure of those columnns should be carefully reviewed in
#'   the result. See [read_excel][readxl::read_excel] for details.
#'
#' @details Column `animal_id` is considered a required column by many other
#'   functions in this package, so it will be created if any records are `NULL`.
#'   When created, it will be constructed from `tag_code_space` and
#'   `tag_id_code`, separated by '-'.
#'
#' @details Timezone attribute of all timestamp columns (class `POSIXct`) in
#'   output will be "UTC" and all 'glatos-specific' timestamp and timezone
#'   columns will be omitted from result.
#'
#' @note ***On warnings and errors about date and timestamp formats.*** Date and
#'   time columns are sometimes stored as text in Excel. When those records are
#'   loaded by this function, there are two possible outcomes. \cr \cr 1. If the
#'   records are formatted according to the GLATOS Data Dictionary specification
#'   (e.g., "YYYY-MM-DD" for dates and "YYYY-MM-DD HH:MM" for timestamps; see
#'   <https:\\glatos.glos.us>) those records should be properly loaded into R,
#'   but the user is encouraged to verify that they were loaded correctly, so a
#'   warning points the user to those records in the workbook. Users may want to
#'   format as custom date in the workbook to avoid warnings in the future. \cr
#'   \cr 2. If the format of a date-as-text column is not consistent with GLATOS
#'   specification, then no data will be loaded and an error will alert the user
#'   to this condition. \cr \cr
#'   ***On cells with locked formatting in Excel:*** Occasionally the
#'   format of a cell in Excel will be locked. In those cases, it is sometimes
#'   possible to force date formatting in Excel by (1) highlighting the columns
#'   that need reformatting, (2) select 'Text-to-columns' in the 'Data' menu,
#'   (3) select 'Delimited' and 'next', (4) uncheck all delimiters and 'next',
#'   (5) choose 'Date: YMD' in the 'Column data format' box, and (6) 'Finish'.
#'
#' @return A list of class `glatos_workbook` with three elements (described
#'   below) containing data from the standard GLATOS Workbook sheets. If
#'   `read_all = TRUE`, then additional elements will be added with names
#'   corresponding to non-standard sheet names.
#' \describe{
#'   \item{metadata}{A list with data about the project and workbook.}
#'   \item{animals}{A data frame of class `glatos_animals` with data about
#'   tagged animals.}
#'   \item{receivers}{A data frame of class `glatos_receivers` with data
#'   about telemetry receivers.}
#' }
#'
#' @author C. Holbrook \email{cholbrook@usgs.gov}
#'
#' @seealso [read_excel][readxl::read_excel]
#'
#' @examples
#' #get path to example GLATOS Data Workbook
#' wb_file <- system.file("extdata",
#'   "walleye_workbook.xlsm", package = "glatos")
#'
#' #note that code above is needed to find the example file
#' #for real glatos data, use something like below
#' #wb_file <- "c:/path_to_file/HECWL_GLATOS_20150321.csv"
#'
#' wb <- read_glatos_workbook(wb_file)
#'
#' @export
read_glatos_workbook <- function(wb_file, read_all = FALSE, 
  wb_version = NULL) {

  #Read workbook-----------------------------------------------------------
  
  #see version-specific file specifications
  #internal glatos_workbook_spec in R/sysdata.r
  
  
  #Get sheet names
  sheets <- tolower(readxl::excel_sheets(wb_file))
    
  #Identify workbook version (based on sheet names)
  id_workbook_version <- function(wb_file, sheets){
    if(all(names(glatos:::glatos_workbook_schema$v1.3) %in% sheets)) {
      return("1.3") 
    } else {
      stop(paste0("Workbook version could not be identified. Double check ",
                  "that you are using a standard GLATOS Workbook file. The ",
                   "names of sheets must match standard file."))
    }
  }
  
  #Check version if specified
  if(is.null(wb_version)) {
    wb_version <- id_workbook_version(wb_file, sheets)
  } else if (!(paste0("v",wb_version) %in% 
             names(glatos:::glatos_workbook_schema))) {
    stop(paste0("Workbook version ", wb_version, " is not supported."))
  }
  
  wb <- list() #preallocate
  
  if(read_all)  wb[sheets] <- NA #add element for each sheet

  #-Workbook v1.3--------------------------------------------------------------  
  if (wb_version == "1.3") {
    wb[names(glatos:::glatos_workbook_schema$v1.3)] <- NA
    
    #Get project data
    tmp <- tryCatch(readxl::read_excel(wb_file, sheet = "Project",
                                     col_names = FALSE,
                                     .name_repair = "minimal"), 
                    error = function(e){
      if(e$message == 
          "Expecting a single string value: [type=character; extent=0]."){
        stop("There was a problem reading from input file specified. It may ",
          "be protected.\n  Try again after opening, saving, and closing the ",
          "file.")
      } else {stop(e)}
    })
    
    tmp <- as.data.frame(tmp)
    wb$project <- list(project_code = tmp[1,2],
                        principle_investigator = tmp[2,2],
                        pi_email = tmp[3,2],
                        source_file=basename(wb_file),
                        wb_version = "1.3",
                        created = file.info(wb_file)$ctime)      

    #Read all sheets except project
    if(read_all) { 
      sheets_to_read <- sheets
      extra_sheets <- setdiff(sheets, names(glatos:::glatos_workbook_schema[[
        paste0("v", wb_version)]]))
    } else {
      sheets_to_read <- names(glatos:::glatos_workbook_schema[[
                                                paste0("v", wb_version)]])
    }
    sheets_to_read <- setdiff(sheets_to_read, "project") #exclude project
    
    for(i in 1:length(sheets_to_read)){
      
      schema_i <- glatos:::glatos_workbook_schema[[
                  paste0("v", wb_version)]][[sheets_to_read[i]]]

      if(is.null(schema_i)){ xl_start_row <- 1 } else { xl_start_row <- 2 }
        
      #read one row to get dimension and column names
      tmp <- readxl::read_excel(wb_file, 
                            sheet = match(sheets_to_read[i], tolower(sheets)), 
                            skip = xl_start_row - 1, na = c("", "NA")) 
      
      tmp <- as.data.frame(tmp, stringsAsFactors = FALSE)
        
      if(!is.null(schema_i)){
      
        #check that sheet i contains all names in schema
        missing_cols <- setdiff(schema_i$name, tolower(colnames(tmp)))
        if(length(missing_cols) > 0){ 
          stop(paste0("The following columns were not found in sheet named '", 
               sheets_to_read[i],"': ",
               paste(missing_cols, collapse = ", ")))
        }
        
        if(!read_all){
          #subset only columns in schema (by name)
          # - use match so that first column with each name is selected if > 1
          tmp <- tmp[ , match(schema_i$name, tolower(colnames(tmp)))]
        } else {
   
            #identify project-specific fields
            extra_cols <- colnames(tmp)[- match(schema_i$name, tolower(colnames(tmp)))]
            
            #identify new columns to add
            if (length(extra_cols) > 0) {
              
              #count column names to identify and rename any conflicting
              col_counts <- table(tolower(colnames(tmp)))
              conflict_cols <- col_counts[col_counts > 1]
              
              if(length(conflict_cols) > 0) {
                #rename conflict cols
                for(k in 1:length(conflict_cols)) {
                  name_k <- names(conflict_cols)[k]
                  extra_names_k <- c(name_k, 
                                paste0(name_k, "_x", 1:(conflict_cols[k] - 1)))
                  names(tmp)[tolower(colnames(tmp)) == name_k] <- extra_names_k
                    
                  warning(paste0("Non-standard (project-specific) columns ",
                    "were found with names matching standard \n  column names ",
                    "in sheet '", sheets_to_read[i],"'.\n\n  The following ",
                    "column names were assigned to avoid conflicts:", 
                    "\n    ", paste0(extra_names_k, collapse = ", "), "."))
                }
              } #end if
            }
          } #end if else
  
        #make column names lowercase
        names(tmp) <- tolower(names(tmp))
        
        #set classes; by column name since conflicts resolved above
        
        # character
        char_cols <- with(schema_i, name[type == "character"])
        for(j in char_cols) tmp[ , j] <- as.character(data.frame(tmp)[ , j])
   
        # numeric
        num_cols <- with(schema_i, name[type == "numeric"])
        for(j in num_cols) tmp[ , j] <- as.numeric(data.frame(tmp)[ , j])
   
        # POSIXct
        posixct_cols <- with(schema_i, name[type == "POSIXct"])
        for(j in posixct_cols) {
          schema_row <- match(j, schema_i$name)
  
          #Get time zone
          #function to construct time zone string from reference column tmp
          REFCOL <- function(x) {
            col_x <- gsub(")$", "", strsplit(x, "REFCOL\\(")[[1]][2])
            x2 <- tmp[, col_x]
            utc_rows <- tolower(x2) %in% c("utc", "gmt")
            x2[utc_rows] <- "UTC"
            x2[!utc_rows] <- with(tmp, paste0("US/", x2[!utc_rows]))
            return(x2)
          }
          
          #get timezone for this column        
          tz_cmd <- gsub("^tz = |^tz=|\"","", schema_i$arg[schema_row])         
          
          if(grepl("REFCOL", tz_cmd)) { 
            tzone_j <- REFCOL(tz_cmd)
            tz_cmd <- unique(tzone_j)
          } else { tzone_j <- tz_cmd }
          
          if(nrow(tmp) > 0){
          
            #Handle mixture of timestamps as date and char
            
            #identify timestamps that can be numeric; assume others character
            posix_na <- is.na(tmp[, j]) #identify missing first
            
            if(inherits(tmp[, j], "POSIXct")){
              posix_as_num <- tmp[, j]
            } else {
              posix_as_num <- suppressWarnings(as.numeric(tmp[, j]))
              
              #convert excel number to posix
              posix_as_num <- as.POSIXct("1899-12-30", tz = "GMT") + 
                                         (posix_as_num * 86400)
            }
            
            posix_as_char <- !posix_na & is.na(posix_as_num)
            
            if(any(posix_as_char)) {
              bad_pc_rows <- which(posix_as_char) + 2
              bad_pc_rows <- ifelse(length(bad_pc_rows) < 10, 
                              paste0(bad_pc_rows, collapse = ", "),
                              paste0(paste(bad_pc_rows[1:10], collapse = ", "), 
                                "... +", length(bad_pc_rows) - 10, " more.",
                                collapse = " "))
              warning(paste0("Some records (see below) ",
              "in '", sheets_to_read[i], "` were not recognized as Excel ",
              "datetime objects.\n  These should have imported correctly if ",
              "formatted as 'YYYY-MM-DD HH:MM',\n  but see 'Note' in ",
              "help(\"read_glatos_workbook\") to avoid this warning.\n\n ",
              "Column: '", j, "'\n   Rows:  ", bad_pc_rows, "\n "))
            }
    
            #handle multiple time zones
            for(k in 1:length(tz_cmd)){
              rows_k <- tzone_j %in% tz_cmd[k] #get rows with kth tz
              #round to nearest minute and force to correct timezone
              posix_as_num[rows_k] <- as.POSIXct(round(posix_as_num[rows_k], 
                                                       "mins"), 
                                                 tz = tz_cmd[k])
      
              #do same for posix_as_char and insert into posix_as_num
              if(any(posix_as_char[rows_k])){
                posix_as_num[posix_as_char & rows_k] <- 
                  as.POSIXct(tmp[posix_as_char & rows_k , j], 
                             tz = tz_cmd[k])
              }
            } # end k
            
            tmp[ , j] <- posix_as_num
          } else {
            tmp[ , j] <- as.POSIXct(NA, tz = "UTC")[0]
          }
    
          attr(tmp[, j], "tzone") <- "UTC"
        
        } #end j
        
        # Date
        date_cols <- with(schema_i, name[type == "Date"])
        for(j in date_cols) {
          schema_row <- match(j, schema_i$name)
          
          
          #identify date that can be numeric; assume others character
          date_na <- is.na(tmp[, j]) #identify missing 
          
          if(inherits(tmp[, j], "POSIXct")){
            date_as_num <- suppressWarnings(as.Date(tmp[, j]))
          } else {
            #convert excel number to R date
            date_as_num <- suppressWarnings(as.numeric(tmp[, j]))
            date_as_num <- as.Date("1899-12-30") + date_as_num
          }
            
          date_as_char <- !date_na & is.na(date_as_num)         

          #do same for date_as_char and insert into ate_as_num
          if(any(date_as_char)){

            bad_dc_rows <- which(date_as_char) + 2
            bad_dc_rows <- ifelse(length(bad_dc_rows) < 10, 
              paste0(bad_dc_rows, collapse = ", "),
              paste0(paste(bad_dc_rows[1:10], collapse = ", "), 
                "... +", length(bad_dc_rows) - 10, " more.",
                collapse = " "))
            date_as_num[date_as_char] <- tryCatch(as.Date(tmp[date_as_char , j]), 
              error = function(e) {
                if(e$message == "character string is not in a standard unambiguous format"){
                  stop(paste0("At least one of the records identified below in '",
                    sheets_to_read[i], "`\n  could not be coerced to Date because ",
                    "the format was invalid.\n  Dates stored as ",
                    "text in GLATOS Workbooks must be formatted \n  ",
                    "'YYYY-MM-DD'. See 'Note' in ",
                    "help(\"read_glatos_workbook\") \n  about formatting ",
                    "dates and times ", 
                    "in GLATOS Workbooks.\n\n ",
                    "Column: '", j, "'\n   Row:  ", bad_dc_rows, "\n"))
                } else{ return(e) }
              }
            )
          }
          
          #warn user if no error
          if(any(date_as_char)) warning(paste0("Some records (see below) in '",
            sheets_to_read[i], "` were not recognized as Excel date objects.\n",
            "  These should have imported correctly if formatted as ",
            "'YYYY-MM-DD',\n  but see 'Note' in ",
            "help(\"read_glatos_workbook\") to avoid this warning.\n\n ",
            "Column: '", j, "'\n    Row:  ", bad_dc_rows, "\n"))
          
          
          tmp[ , j] <- date_as_num
          
        } #end j
      
      } #end if
        
      wb[[sheets_to_read[i]]] <- tmp
      
    } #end i
          
    
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
    
    #Drop unwanted columns from receivers
    
    #coalesce deploy_date_time and glatos_deploy_date_time
    attr(wb2$receivers$glatos_deploy_date_time, "tzone") <- "UTC"
    ddt_na <- is.na(wb2$receivers$deploy_date_time)
    wb2$receivers$deploy_date_time[ddt_na] <- 
                                wb2$receivers$glatos_deploy_date_time[ddt_na]
    
    #coalesce recover_date_time and glatos_recover_date_time
    attr(wb2$receivers$glatos_recover_date_time, "tzone") <- "UTC"
    rdt_na <- is.na(wb2$receivers$recover_date_time)
    wb2$receivers$recover_date_time[rdt_na] <- 
      wb2$receivers$glatos_recover_date_time[rdt_na]    
    
    drop_cols_rec <- c("glatos_deploy_date_time", "glatos_timezone",
                   "glatos_recover_date_time")
    wb2$receivers <- wb2$receivers[ , -match(drop_cols_rec, 
                                             names(wb2$receivers))]
    
    #sort rows by deploy_date_time
    wb2$receivers <- wb2$receivers[with(wb2$receivers, 
            order(deploy_date_time, glatos_array, station_no)), ]
    row.names(wb2$receivers) <- NULL
    
    #Drop unwanted columns from animals
    
    #coalesce release_date_time and utc_release_date_time
    attr(wb2$animals$glatos_release_date_time, "tzone") <- "UTC"
    ardt_na <- is.na(wb2$animals$utc_release_date_time)
    wb2$animals$utc_release_date_time[ardt_na] <- 
      wb2$animals$glatos_release_date_time[ardt_na]  
    
    drop_cols_anim <- c("glatos_release_date_time", "glatos_timezone")
    wb2$animals <- wb2$animals[ , -match(drop_cols_anim, 
                                         names(wb2$animals))]
    
    #sort animals
    #sort rows by deploy_date_time
    wb2$animals <- wb2$animals[with(wb2$animals, 
      order(utc_release_date_time, animal_id)), ]
    row.names(wb2$animals) <- NULL
    
    #create animal_id if missing
    anid_na <- is.na(wb2$animals$animal_id)
    wb2$animals$animal_id[anid_na] <- with(wb2$animals[anid_na, ], 
            paste0(tag_code_space, "-", tag_id_code))
    
    #Append new sheets if required
    if(read_all) {
      for(i in 1:length(extra_sheets)){
        wb2[extra_sheets[i]] <- wb[extra_sheets[i]]
      }
    }
  }

  #-end v1.3----------------------------------------------------------------
  
  #assign classes
  wb2$animals <- as_glatos_animals(wb2$animals)
  wb2$receivers <- as_glatos_receivers(wb2$receivers)
  wb2 <- glatos:::glatos_workbook(wb2)
  
  return(wb2)
}

