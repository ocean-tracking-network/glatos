#' Plot detection locations of acoustic transmitters over time
#' 
#' Plot detection locations of acoustic transmitters over time.
#' 
#' @param det A \code{glatos_detections} object (e.g., produced by
#'   \link{read_glatos_detections}) containing detections to be plotted.
#'   
#'   \emph{OR} A data frame containing detection data with at least two columns, 
#'   one of which must be named 'detection_timestamp_utc', described below,  
#'   and another column containing a location grouping variable,
#'   whose name is specified by \code{location_col} (see below).
#'   
#'   The following column must appear in \code{det}: 
#'   \describe{
#'   \item{\code{detection_timestamp_utc}}{Detection timestamps; MUST be of 
#'   class POSIXct.}
#'   }
#'   
#' @param location_col A character string indicating the column name in
#'   \code{det} that will be used as the location grouping variable (e.g.
#'   "glatos_array"), in quotes.
#'   
#' @param locations An optional vector containing the locations
#'   \code{location_col} to show in the plot. Plot order corresponds to order
#'   in the vector (from bottom up). Should correspond to values in
#'   \code{location_col}, but can contain values that are not in the det
#'   data frame (i.e., can use this option to plot locations fish were
#'   not detected).
#'   
#' @param show_receiver_status A logical value indicating whether or not to
#'   display receiver status behind detection data (i.e., indicate when 
#'   receivers were in the water). If \code{show_receiver_status} == TRUE, then
#'   a receiver_history data frame (\code{receiver_history}) must be supplied.
#'   Default is FALSE.
#' 
#' @param receiver_history An optional \code{glatos_receivers} object
#'   (e.g., produced by \link{read_glatos_receivers}) containing receiver
#'   history data for plotting receiver status behind the detection data when 
#'   \code{show_receiver_status} == TRUE.
#'   
#'   \emph{OR} An optional data frame containing receiver history
#'   data for plotting receiver status behind the detection data when 
#'   \code{show_receiver_status} == TRUE. 
#'   
#'   The data frame must contain at least three columns: 
#'   1) 'deploy_date_time, 2) 'recover_date_time', and 3) a grouping
#'   column whose name is specified by \code{location_col} (see below). Columns 
#'   'deploy_date_time' and 'recover_date_time' must be of class POSIXct. The 
#'   grouping column must have the same name in both \code{det} and 
#'   \code{receiver_history}.
#'   
#' @param out_file An optional character string with the name (including 
#'   extension) of output image file to be created. File extension
#'   will determine type of file written. For example, \code{"abacus_plot.png"}
#'   will write a png file to the working directory. If \code{NULL} (default)
#'   then the plot will be printed to the default plot device.
#'   Supported extensions: png, jpeg, bmp, and tiff.
#'   
#' @param outFile Deprecated. Use \code{out_file} instead.
#'   
#' @param ... Other plotting arguments that pass to "points" function (e.g., col,
#'   lwd, type).
#'   
#' @details NAs are not allowed in any of the two required columns.
#'   
#' @details The locations vector is used to control which locations will
#'   appear in the plot and in what order they will appear. If no locations
#'   vector is supplied, the function will plot only those locations that
#'   appear in the \code{det} data frame and the order of locations on the 
#'   y-axis will be alphebetical from top to bottom.
#'   
#' @details By default, the function does not distinguish detections from
#'   different transmitters and will therefore plot all transmitters the same
#'   color. If more than one fish is desired in a single plot, a vector of
#'   colors must be passed to the function using the 'col =' argument. The color
#'   vector must be the same length as the number of rows in the detections data
#'   frame or the colors will be recycled.
#'   
#' @details Plotting options (i.e., line width and color) can be changed using
#'   optional graphical parameters 
#'   \url{http://www.statmethods.net/advgraphs/parameters.html} that are passed
#'   to "points" (see ?points).
#'   
#' @return An image to the default plot device or a file containing the 
#' image if \code{out_file} is specified.
#'   
#' @author T. R. Binder, edited by A. Dini
#'   
#' @examples
#' #get path to example detection file
#' det_file <- system.file("extdata", "walleye_detections.csv",
#'  package = "glatos")
#' det <- read_glatos_detections(det_file)
#' 
#' #subset one transmitter
#' det2 <- det[det$animal_id == 153, ]
#' 	
#' #plot without control table and main tile and change color to red
#' abacus_plot(det, locations=NULL, 
#'   main = "TagID: 32054", col = "red")
#' 	
#' #example with locations specified
#' abacus_plot(det, locations=c("DRF", "DRL", "FMP", "MAU", "PRS", "RAR",
#'    "DRM", "FDT"), main = "TagID: 32054", col = "red")
#'
#' #plot with custom y-axis label and lines connecting symbols
#' abacus_plot(det, main = "TagID: 32054", type = "l",
#' 	 col = "red")
#'   
#' @export

abacus_plot <- function(det,
                        location_col = 'glatos_array',
                        locations = NULL,
                        show_receiver_status = FALSE,
                        receiver_history = NULL,
                        out_file = NULL,
                        outFile = NULL,
                        ...){
  
  #check if outFile was given
  if(!is.null(outFile)){
    out_file <- outFile
    warning(paste0("Input argument 'outFile' is deprecated and will not be ",
                   "supported in the future. Use 'out_file' instead."), 
            call. = FALSE)
  }
  
  # Check that the specified columns appear in the detections data frame
  missingCols <- setdiff(c("detection_timestamp_utc", location_col), names(det))
  if (length(missingCols) > 0){
    stop(paste0("det is missing the following ",
      "column(s):\n", paste0("       '", missingCols, "'", collapse="\n")), 
      call. = FALSE)
  }
  
  # Rename column specified in location_col to "location"
  names(det)[which(names(det)==location_col)] = "location"
  
  # Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(det$detection_timestamp_utc))){
    stop(paste0("Column 'detection_timestamp_utc' in the det data frame must 
                be of class 'POSIXct'."),
      call. = FALSE)
  } 
  
  # Get output directory and check if exists
  if(!is.null(out_file)){
    outDir <- ifelse(dirname(out_file) == ".", getwd(), dirname(out_file)) 
    if(!dir.exists(outDir)) stop("Output directory '", outDir, 
      "' does not exist.", call. = FALSE)
  }
  
  # Perform checks related to show_receiver_status
  if(show_receiver_status){
      # Check that receiver_history data frame is passed to function
      if(is.null(receiver_history)){
        stop("Argument 'receiver_history' (i.e., data frame containing deploy 
             and recovery times for receievr locations) must be specified when 
             show_receiver_status == TRUE.")
      }
    
      # Check that the specified columns appear in the receiver history data frame
      missingCols2 <- setdiff(c("deploy_date_time", "recover_date_time", location_col), 
                              names(receiver_history))
      if (length(missingCols2) > 0){
        stop(paste0("receiver_history is missing the following ","column(s):\n", 
                    paste0("       '", missingCols2, "'", collapse="\n")), 
             call. = FALSE)
      }
      
      # Check that deploy_date_time is of class 'POSIXct'
      if(!('POSIXct' %in% class(receiver_history$deploy_date_time))){
        stop(paste0("Column 'deploy_date_time' in the receiver_history data 
                    frame must be of class 'POSIXct'."),
             call. = FALSE)
      }
      
      # Check that recover_date_time is of class 'POSIXct'
      if(!('POSIXct' %in% class(receiver_history$deploy_date_time))){
        stop(paste0("Column 'recover_date_time' in the receiver_history data 
                    frame must be of class 'POSIXct'."),
             call. = FALSE)
      }
      
      # Rename receiver_history column specified in location_col to "location"
      names(receiver_history)[which(names(receiver_history)==location_col)] = "location"
  }
  
  # Make a list of optional arguments passed through ... for use in parsing out 
  # arguments for plotting.
  arguments <- list(...)
  
  # If locations not supplied, create one data frame with unique values
  # (ordered alphebetically from top to bottom) of location_col values with
  # plot order appended. Otherwise append a column of plot order to locations.
  if(is.null(locations)){
    locations_table <- data.frame(
      location = sort(unique(det$location), decreasing = TRUE), 
      y_order = 1:length(unique(det$location)), 
      stringsAsFactors = FALSE)
  } else {
    locations_table <- data.frame(
      location = locations, 
      y_order = 1:length(locations), 
      stringsAsFactors = FALSE)
  }
  
  
  # Merge det and locations_table data frames
  # Keep only locations that appear in the locations_table data frame
  det <- merge(det, locations_table, by = "location", all.y = TRUE)
  
  # Sort by timestamp
  det <- det[order(det$detection_timestamp_utc), ] 
  
  # Prepare receiver_history data frame for plotting
  if(show_receiver_status){
    # Merge receiver_history and locations_table data frames
    # Keep only locations that appear in the locations_table data frame
    receiver_history <- merge(receiver_history, locations_table, by = "location", all.y = TRUE)
  }
  
  
  # Variable which scales the height of the y-axis depending on the number of 
  # labels to appear. 
  # Assumes 24 labels is the perfect spacing for height = 1000 px.
  pngHeight <- max((nrow(locations_table)/24)*1000, 500)
  
  # Calculate a y-axis label offset to accommodate grouping variables with 
  # different string lengths (e.g., "DRM" vs "DRM-001").
  YlabOffset <- (max(nchar(det$location)) - 3) / 3
  
  
  #get file extension
  file_type <- ifelse(is.null(out_file), NA, tools::file_ext(out_file))
  
  #check file extension is supported
  ext_supp <- c(NA, "png", "jpeg", "png", "bmp", "tiff")
  if(!(tolower(file_type) %in% ext_supp))
    stop(paste0("Image type '", file_type, "' is not supported."), 
      call. = FALSE)
  
  if(!is.na(file_type) & tolower(file_type) == 'png')
    png(out_file, height = pngHeight, width = 1000, pointsize = 22)
  if(!is.na(file_type) & tolower(file_type) == 'jpeg')
    jpeg(out_file, height = pngHeight, width = 1000, pointsize = 22)
  if(!is.na(file_type) & tolower(file_type) == 'bmp')
    bmp(out_file, height = pngHeight, width = 1000, pointsize = 22)
  if(!is.na(file_type) & tolower(file_type) == 'tiff')
    tiff(out_file, height = pngHeight, width = 1000, pointsize = 22)  
  
  
  # Set inner and outer margins
  par(mar = c(1, 1, 1.5, 2), oma = c(3, 4 + YlabOffset, 0, 0))
  
  # Plot detection data
  with(det,
       plot(NULL, 
            xlim = range(detection_timestamp_utc, na.rm = TRUE), 
            ylim = c(1,nrow(locations_table)), 
            yaxt = "n", xaxt = "n", ylab = "", 
            xlab = ""))
  
  if(show_receiver_status){
    with(receiver_history, 
         segments(deploy_date_time,
                  y_order, 
                  recover_date_time, 
                  y_order, 
                  lwd = 3,
                  col = "gray"))
  }

  with(det, 
       points(detection_timestamp_utc, y_order, ...))
  
  # Add custom axes
  axis(2, at = locations_table$y_order, 
    labels = locations_table$location, las = 1)
  xmaj <- seq(from = min(det$detection_timestamp_utc, na.rm = TRUE), 
    to = max(det$detection_timestamp_utc, na.rm = TRUE), length.out = 5)
  axis(1, at = xmaj, labels = format(xmaj, "%Y-%m-%d"), las = 1)
  
  # Add axes titles
  mtext(ifelse("xlab" %in% names(arguments), arguments$xlab, "Date"), 
        side = 1, line = 2.2, cex = 1.2)
  mtext(ifelse("ylab" %in% names(arguments), arguments$ylab, location_col),
        side = 2, line = 3.5 + YlabOffset, cex = 1.2)
  
  if(!is.na(file_type))	{
    dev.off()
    
    message(paste0("Output file is located in the following directory:\n", 
      outDir))	
  }
}
