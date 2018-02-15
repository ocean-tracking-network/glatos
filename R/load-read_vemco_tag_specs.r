#' @title 
#' Read telemetry transmitter (tag) specification data from a Vemco file
#' 
#' @description
#' Read telemetry transmitter (tag) specification data from a file and 
#' return a list with tag specifications and tag operating schedule.
#'
#' @param tag_file A character string with path and name of file in 
#'  a supported standard format in quotes. If only file name is given, then the 
#'  file must be located in the working directory. 
#'  
#' @param file_format A character string with the tag spec file format
#'  in quotes. If NULL (default value) then version will be
#'  determined by evaluating file structure. The only allowed values 
#'  are \code{NULL} and \code{"vemco_xls"}. Any other values will trigger an 
#'  error.
#'
#' @details
#' The file format \code{vemco_xls} is a MS Excel file provided to tag 
#' purchasers by Vemco.
#' 
#' @details
#' This function is not endorsed or supported by any transmitter manufacturer. 
#'  
#' @return A list containing two data frames with tag specifications and 
#' tag operating schedule.  
#' 
#' A list element called \code{specs} is a data frame contains tag specifications data in 
#' 17 columns:  
#' \describe{
#' \item{serial_number}{}        
#' \item{manufacturer}{}         
#' \item{model}{}             
#' \item{id_count}{}             
#' \item{code_space}{}           
#' \item{id_code}{}              
#' \item{n_steps}{}              
#' \item{sensor_type}{}          
#' \item{sensor_range}{}         
#' \item{sensor_units}{}         
#' \item{sensor_slope}{}       
#' \item{sensor_intercept}{}     
#' \item{accel_algorithm}{}      
#' \item{accel_sample_rate}{}    
#' \item{sensor_transmit_ratio}{}
#' \item{est_battery_life_days}{}
#' \item{battery_life_stat}{} 
#' }
#' 
#' A list element called \code{schedule} is a data frame containing tag
#' operating shedule data in 11 columns:
#' 
#' \describe{
#' \item{serial_number}{}
#' \item{code_space}{}    
#' \item{id_code}{}           
#' \item{step}{}         
#' \item{next_step}{}         
#' \item{status}{}            
#' \item{duration_days}{}     
#' \item{power}{}             
#' \item{min_delay_secs}{}    
#' \item{max_delay_secs}{}    
#' \item{accel_on_time_secs}{}
#' 
#' }
#' 
#' @author C. Holbrook, \email{cholbrook@usgs.gov}
#'
#' @examples
#' #get path to example Vemco tag spec file
#' spec_file <- system.file("extdata", 
#'   "lamprey_tag_specs.xls", package = "glatos")
#' my_tags <- read_vemco_tag_specs(spec_file, file_format = "vemco_xls")
#'
#' @export
read_vemco_tag_specs <- function(tag_file, file_format = NULL) {

  #Read file-----------------------------------------------------------
  
  #see version-specific file specifications
  #internal data object; i.e., in R/sysdata.r
  
  
  #Get sheet names
  sheets <- tolower(readxl::excel_sheets(tag_file))
  
  
  #Identify file version
  id_file_format <- function(schema, sheets){
    if(all(names(schema$vemco_xls) %in% sheets)) { 
      return("vemco_xls") 
    } else {
      stop("Tag spec file format could not be identified.")
    }
  }
  
  if(is.null(file_format)) {
    file_format <- id_file_format(vemco_tag_spec_schema, sheets)
  } else if (!(paste0(file_format) %in% names(vemco_tag_spec_schema))) {
    stop(paste0("File format '", file_format, "' is not supported."))
  }

  wb <- list() #preallocate
  
  #-vemco_xls----------------------------------------------------------------  
  if (file_format == "vemco_xls") {
    wb[names(vemco_tag_spec_schema$vemco_xls)] <- NA
    
  #Read all sheets
    sheets_to_read <- names(vemco_tag_spec_schema$vemco_xls)

    for(i in 1:length(sheets_to_read)){
      spec_i <- vemco_tag_spec_schema$vemco_xls[[sheets_to_read[i]]]

      if(length(spec_i) > 1){
         #read all columns as character/text because it seems they are often 
         # formatted as text in files from Vemco
         tmp <- readxl::read_excel(tag_file, 
           sheet = match(sheets_to_read[i], tolower(sheets)), 
           col_types = "text")
      
         #coerce to data frame and add to wb
         wb[[sheets_to_read[i]]] <- data.frame(tmp, check.names = FALSE)
         
        # tmp <- readxl::read_excel(tag_file, 
        #   sheet = match(sheets_to_read[i], tolower(sheets)), 
        #   col_types = gsub("character", "text", spec_i$type))
        # wb[[sheets_to_read[i]]] <- data.frame(tmp, check.names = FALSE)
        
        
        #trim leading and trailing white space from column names
        names(wb[[sheets_to_read[i]]]) <- 
          trimws(names(wb[[sheets_to_read[i]]]))
        
        #coerce numeric columns
        for(j in spec_i$name[spec_i$type == "numeric"]) 
          wb[[sheets_to_read[i]]][ , j] <- 
            as.numeric(wb[[sheets_to_read[i]]][ , j])
      }
    }
    
    #coerce to class tag_spec
    spec_out <- with(wb$`tag summary`, data.frame(
      sales_order = `Sales Order`,
      serial_number = `Serial No.`,
      manufacturer = "Vemco",
      model = `Tag Family`,
      id_count = `# of ID's`,
      code_space = sapply(`VUE Tag ID`, 
                              function(x) paste0(
                                strsplit(x, "-")[[1]][1:2], collapse = "-")),
      id_code = sapply(`VUE Tag ID`, function(x) strsplit(x, "-")[[1]][3]),
      n_steps = apply(data.frame(`Step 1 Status`, 
                                         `Step 2 Status`,
                                         `Step 3 Status`,
                                         `Step 4 Status`), 
                                   1, 
                                   function(x) which.max(tolower(x) == "on")),
      sensor_type = `Sensor type`,
      sensor_range = `Range`,
      sensor_units = `Units`,
      sensor_slope = `Slope`, 
      sensor_intercept = `Intercept`, 
      accel_algorithm = `Accelerometer Algorithm`,
      accel_sample_rate =`Accelerometer Samples (/sec)`,
      sensor_transmit_ratio = `Sensor Transmit Ratio`,
      est_battery_life_days = `95% Est Battery Life (days)`,
      battery_life_stat = "95%",
      stringsAsFactors = FALSE))
    
    row.names(spec_out) <- NULL #reset to default
    
    #compile operating schedule
    
    # expand from single row per id to mode_count rows per id
    expanded_rows <- rep(seq_len(nrow(spec_out)), rep(4,nrow(spec_out)))
    sched_out <- spec_out[expanded_rows, 
      c("serial_number", "code_space", "id_code")]
    
    row.names(sched_out) <- NULL #reset to default
       
    #add step number
    sched_out$step <- rep(1:4, nrow(spec_out))
    
    #add next step to accomodate "loop to"
    sched_out$next_step <- sched_out$step + 1
    sched_out$next_step[sched_out$step == 4] <- wb$`tag summary`$`Loop To`
      
    #function to convert Vemco's "dy hr:min:sec" format to decimal days
    to_dec_days <-{ function(x) sapply(x, 
      function(x) {
        x2 <- strsplit(x, " ")[[1]]
        x2_day <- as.numeric(x2[1])
        x2_time <- sum(as.numeric(strsplit(x2[[2]], ":")[[1]]) / 
            c(24, 1440, 86400))
        dec_day <- x2_day + x2_time
      })
    }
    
    
    #identify rows in wb that contain valid data for each step
    step_rows <- sapply(1:4, function(x) expanded_rows[sched_out$step == x], 
      simplify = FALSE)

    #preallocate columns
    sched_out <- within(sched_out, {
      accel_on_time_secs <- as.numeric(NA)
      max_delay_secs <- as.numeric(NA)
      min_delay_secs <- as.numeric(NA)
      power <- as.character(NA)
      duration_days <- as.numeric(NA)
      status <- as.character(NA)
    })
  
    sched_out <- within(sched_out, {
      #if step 1 exists
      if(any(step_rows[[1]])){
        
        status[step == 1] <- 
          wb$`tag summary`$`Step 1 Status`[step_rows[[1]]]        
        
        duration_days[step == 1] <- 
          to_dec_days(
            wb$`tag summary`$`Step 1 Time     (dy hr:min:sec)`[
              step_rows[[1]]]) 
        
        power[step == 1] <- 
          wb$`tag summary`$`Step 1 Power (L/H)`[step_rows[[1]]]
        
        accel_on_time_secs[step == 1] <- 
          wb$`tag summary`$`Step 1 Acc. On (sec)`[step_rows[[1]]]
        
        min_delay_secs[step == 1] <- 
          wb$`tag summary`$`Step 1 Min Delay (sec)`[step_rows[[1]]]
        
        max_delay_secs[step == 1] <- 
          wb$`tag summary`$`Step 1 Max Delay (sec)`[step_rows[[1]]]
      }

      #if step 2 exists
      if(any(step_rows[[2]])){
        
        status[step == 2] <- 
          wb$`tag summary`$`Step 2 Status`[step_rows[[2]]]           
        
        duration_days[step == 2] <- 
          to_dec_days(
            wb$`tag summary`$`Step 2 Time       (dy hr:min:sec)`[
              step_rows[[2]]]) 
        
        power[step == 2] <- 
          wb$`tag summary`$`Step 2 Power (L/H)`[step_rows[[2]]]
        
        accel_on_time_secs[step == 2] <- 
          wb$`tag summary`$`Step 2 Acc. On (sec)`[step_rows[[2]]]
        
        min_delay_secs[step == 2] <- 
          wb$`tag summary`$`Step 2 Min Delay (sec)`[step_rows[[2]]]
        
        max_delay_secs[step == 2] <- 
          wb$`tag summary`$`Step 2 Max Delay (sec)`[step_rows[[2]]]
      }
     
      #if step 3 exists
      if(any(step_rows[[3]])){
        
        status[step == 3] <- 
          wb$`tag summary`$`Step 3 Status`[step_rows[[3]]]   
        
        duration_days[step == 3] <- 
          to_dec_days(
            wb$`tag summary`$`Step 3 Time       (dy hr:min:sec)`[
              step_rows[[3]]]) 
        
        power[step == 3] <- 
          wb$`tag summary`$`Step 3 Power (L/H)`[step_rows[[3]]]
        
        accel_on_time_secs[step == 3] <- 
          wb$`tag summary`$`Step 3 Acc. On (sec)`[step_rows[[3]]]
        
        min_delay_secs[step == 3] <- 
          wb$`tag summary`$`Step 3 Min Delay (sec)`[step_rows[[3]]]
        
        max_delay_secs[step == 3] <- 
          wb$`tag summary`$`Step 3 Max Delay (sec)`[step_rows[[3]]]
      }
      
      #if step 4 exists
      if(any(step_rows[[4]])){
        
        status[step == 4] <- 
          wb$`tag summary`$`Step 4 Status`[step_rows[[4]]]   
        
        duration_days[step == 4] <- 
          to_dec_days(
            wb$`tag summary`$`Step 4 Time         (dy hr:min:sec)`[
              step_rows[[4]]]) 
        
        power[step == 4] <- 
          wb$`tag summary`$`Step 4 Power (L/H)`[step_rows[[4]]]
        
        accel_on_time_secs[step == 4] <- 
          wb$`tag summary`$`Step 4 Acc. On (sec)`[step_rows[[4]]]
        
        min_delay_secs[step == 4] <- 
          wb$`tag summary`$`Step 4 Min Delay (sec)`[step_rows[[4]]]
        
        max_delay_secs[step == 4] <- 
          wb$`tag summary`$`Step 4 Max Delay (sec)`[step_rows[[4]]]
      }
      
    })

    
  }
  #-end vemco_xls---------------------------------------------------------------
  
  return(list(specs = spec_out, schedule = sched_out))
}