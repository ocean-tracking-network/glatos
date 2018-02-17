#' Plot number of tagged animals or detections on a map
#'
#' Make bubble plots showing the number of fish detected and number of 
#'   detections across a telemetry receiver network.
#'
#' @param detections A data frame containing detection data with at least 
#'   5 columns containing 'location', 'animal', 'timestamp', 'latitude', 
#'   and 'longitude' data.
#' 
#' @param receiverLocs An optional data frame containing at least 5 columns with 
#'   receiver 'location', 'lat', 'lon', 'deploy_timestamp', and 
#'   'recover_timestamp'.
#'   
#' @param type A character string that contains the type of data that is being 
#'   passed in, for example, "OTN", "GLATOS", or "sample".
#'   
#' @param detColNames is defined as a list with names of required columns in 
#'   \code{detections}, defined by \code{type}: 
#' \itemize{
#'   \item \code{locationCol} is a character string with the name of the column 
#'   	 containing locations you wish to filter to ('glatos_array' for GLATOS 
#'   	 data, 'station' for OTN data, or 'location' for sample data).
#'   \item \code{animalCol} is a character string with the name of the column 
#' 		 containing the individual animal identifier ('animal_id' for GLATOS 
#' 		 data, 'catalognumber' for OTN data, or 'animal' for sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' 
#'     for OTN data, or 'time' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 
#'     'latitude' for OTN data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 
#'     'longitude' for OTN data, or 'longitude' for sample data).
#' }
#' 
#' @param map is an optional SpatialPolygonsDataFrame or other
#'   geo-referenced object to be plotted as the background for the plot. It is 
#'   defined by \code{type}.
#' 
#' @param recColNames is a list with names of required columns in 
#'   \code{receiverLocs}, defined by \code{type}: 
#' \itemize{
#'   \item \code{locationCol} is a character string with the name of the column 
#'   	 containing the locations that will be plotted ('glatos_array' for GLATOS 
#'   	 data, 'station' for OTN data, or 'location' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing the latitude of the receiver ('deploy_lat' for 
#'     GLATOS data, 'latitude' for OTN data, or 'latitude' for sample data). 
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 
#'     'latitude' for OTN data, or 'latitude' for sample data).
#'	 \item \code{deploy_timestampCol} is a character string with the name of 
#'     the column containing datetime stamps for receiver deployments (MUST be 
#'     of class 'POSIXct') ('deploy_date_time'for GLATOS data, 
#'     'deploy_date_time' for OTN data, or 'deploy_time' for sample data). 
#'	 \item \code{recover_timestampCol} is a character string with the name of 
#'     the column containing datetime stamps for receier recover (MUST be of 
#'     class 'POSIXct') ('recover_date_time'for GLATOS data, 
#'     'recover_date_time' for OTN data, or 'recover_time' for sample data). 
#' }
#' 
#' @param mapPars is a list of mapping parameters (with exact names 
#'   matching below) including:
#' \itemize{
#'   \item \code{xLimits} is a two-element numeric vector that defines 
#'     minimum and maximum extents of the viewable plot area along the x-axis.
#'   \item \code{yLimits} is a two-element numeric vector that defines 
#'     minimum and maximum extents of the viewable plot area along the y-axis.
#'   \item \code{symbolRadius} is a numeric scalar that sets the radius of 
#'     each "bubble" on the plot in units of percent of x-axis scale. Default 
#'     value = 1 (i.e., 1% of x-axis).
#'   \item \code{colGrad} A two-element character vector indicating the start 
#'     and end colors of the gradient scale used to color-code "bubbles".
#'   \item \code{showAll} A logical (default = FALSE) indicating whether to 
#'     plot all receiver groups (TRUE) or only those receiver groups on 
#'     which the fish were detected (FALSE).
#' } which are defined by \code{type}
#' 
#' @param outFile An optional character string with the name (including 
#'   extension) of output file created. File extension will determine type of 
#'   file written. For example, \code{"BubblePlot.png"} will write a png 
#'   file to the working directory. If \code{NULL} (default) then the plot will 
#'   be printed to the default plot device will be used. Supported extensions: 
#'   png, jpeg, bmp, and tiff.
#' 
#' @details If \code{mapPars$showAll} is TRUE then the plot will show all 
#'   receivers, including those that detected none of the transmitters in 
#'   \code{detections}. In that case, it will first be determined if a receiver 
#'   (or group of receivers, as defined by 'location') was in the water during 
#'   the time interval between the first and last detection in \code{detections}
#'   data frame. Receivers for which deployment-recovery intervals do not 
#'   with the time coverage of detections will not be included in the plot.
#'   
#' @details "ColGrad" is used in a call to \code{colorRampPalette()}, which 
#'   will accept a vector containing any two colors return by \code{colors()} 
#'   as character strings.
#' 
#' @return A list containing the following data frames and columns: 
#'   \item{summaryNumFish}{
#'     \itemize{
#' 		 	 \item \code{location}: user-specified 'location' labels
#' 		 	 \item \code{Summary}: number of unique 'animals' detected at 'location'
#'       \item \code{meanLat}: mean latitude of receivers at 'location'
#'       \item \code{meanLon}: mean longitude of receivers at 'location'}}
#'   \item{summaryNumDetections}{
#'     \itemize{
#' 		 	 \item \code{location}: user-specified 'location' labels
#' 		 	 \item \code{Summary}: number of unique detections at 'location'
#'       \item \code{meanLat}: mean latitude of receivers at 'location'
#'       \item \code{meanLon}: mean longitude of receivers at 'location'}}
#'
#' @return Two png files containing bubble plots for number of unique fish 
#'   detected ("BubblePlot_summaryNumFish.png") and total detections 
#'   ("BubblePlot_summaryNumDetections.png").
#'
#' @author T. R. Binder, edited by A. Dini
#' 
#' @examples
#' 
#' #get path to example detection file
#' det_file <- system.file("extdata", "walleye_detections.csv",
#'  package = "glatos")
#' det <- read_glatos_detections(det_file)
#'
#'
#' #call with defaults
#' detection_bubble_plot(det)
#' 
#' #get path to example receiver file
#' rec_file <- system.file("extdata", "sample_receivers.csv",
#'  package = "glatos")
#' rec <- read_glatos_receivers(rec_file)
#' 
#' #view example map background
#' library(sp) 
#' data(greatLakesPoly)
#' plot(greatLakesPoly)
#' 
#' detection_bubble_plot(det, receiverLocs = rec,
#'   mapParms = list(symbolRadius = 1.4,colGrad = c("white", "blue"), 
#'   showAll = T))
#'
#' @export

detection_bubble_plot <- function(det, location_col = "glatos_array", 
                                  receiver_locs = NULL,
                                  map = NULL,
                                  out_file = NULL,
                                  background_ylim = c(41.3, 49.0),
                                  background_xlim = c(-92.45, -75.87),
                                  symbol_radius = 1,
                                  col_grad = c("white", "red")){
  
  # Check that the specified columns appear in the det data frame
  missingCols <- setdiff(c("animal_id", "detection_timestamp_utc",
                           "deploy_lat", "deploy_long", location_col),
                         names(det))
  if (length(missingCols) > 0){
    stop(paste0("det is missing the following ",
                "column(s):\n", paste0("       '", missingCols, "'",
                                       collapse="\n")), 
         call. = FALSE)
  }
    
    if(is.null(map)) {
      #read example file from package
      data(greatLakesPoly)
      map <- greatLakesPoly
    }
  
  # Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(det$detection_timestamp_utc))){
    stop(paste0("Column detection_timestamp_utc in det data frame must be of
                class 'POSIXct'."),
      call. = FALSE)
  }  	
    
  # Call glatos::detection_summary to create summary data.
  det_summ <- glatos::summarize_detections(det, location_col = location_col,
                                   receiver_locs = receiver_locs,
                                   type = "location")
  
  # Re-order the summaries so that sites with detections plot on top of sites 
  #  without. Makes it easier to see detected locations when they are close 
  #  enough together that the bubbles overlap    
  det_summ <- det_summ[order(det_summ$num_fish), ]
  
  # Create labs with degrees symbol for plots
  xlabs <- round(seq(from = background_xlim[1], to = background_xlim[2], 
    length.out = 5), 2)
  ylabs <- round(seq(from = background_ylim[1], to = background_ylim[2], 
    length.out = 5), 2)
  
  # Define the color palette used to color-code the bubbles
  color <- c(colorRampPalette(col_grad)(101))
  
  # Calculate the location to plot the color scale
  scaleLoc <- c(background_xlim[1] + ((background_xlim[2] -background_xlim[1])
                                      *0.025),
                background_ylim[1] + ((background_ylim[2] - background_ylim[1])
                                      * 0.25),
                background_xlim[1] + ((background_xlim[2] - background_xlim[1])
                                      * 0.05),
                background_ylim[2] - ((background_ylim[2] - background_ylim[1])
                                      * 0.25))
  
  # Calculate great circle distance in meters of x and y limits.
  # needed to determine aspect ratio of the output
  linear_x = geosphere::distMeeus(c(background_xlim[1],background_ylim[1]),
                                  c(background_xlim[2],background_ylim[1]))
  linear_y = geosphere::distMeeus(c(background_xlim[1],background_ylim[1]),
                                  c(background_xlim[1],background_ylim[2]))
  
  # aspect ratio of image
  figRatio <- linear_y/linear_x
  
  #get file extension
  file_type <- ifelse(is.null(out_file), NA, tools::file_ext(out_file))
  
  #check file extension is supported
  ext_supp <- c(NA, "png", "jpeg", "png", "bmp", "tiff")
  if(!(tolower(file_type) %in% ext_supp))
    stop(paste0("Image type '", file_type, "' is not supported."), 
      call. = FALSE)

  if(!is.na(file_type) & tolower(file_type) == 'png')
    png(out_file, height = 1000 * figRatio, width = 1000, pointsize = 28)
  if(!is.na(file_type) & tolower(file_type) == 'jpeg')
    jpeg(out_file, height = 1000 * figRatio, width = 1000, pointsize = 28)
  if(!is.na(file_type) & tolower(file_type) == 'bmp')
    bmp(out_file, height = 1000 * figRatio, width = 1000, pointsize = 28)
  if(!is.na(file_type) & tolower(file_type) == 'tiff')
    tiff(out_file, height = 1000 * figRatio, width = 1000, pointsize = 28)  

  
  if(is.null(out_file)){
    if(Sys.info()["sysname"]=="Windows"){
      windows(height = 7 * figRatio, width = 7)
    }else if(Sys.info()["sysname"]=="Darwin"){
      quartz(height = 7 * figRatio, width = 7)
    }else{
      x11(height = 7 * figRatio, width = 7)
    }
  }
  
  # Set margins
  par(mar = c(1, 0, 0, 2), oma = c(3, 5, 1, 0))	    
  
  # Plot background image
  sp::plot(map, xlim = background_xlim, ylim = background_ylim, axes = T, asp = 1/figRatio, 
    xaxs = "i", lwd = 1.5, xaxt = 'n', yaxt = 'n', col = "White", 
    bg="WhiteSmoke")
  
  # Plot the bubbles
  symbols(det_summ$mean_lon, det_summ$mean_lat,
          circles = rep((background_xlim[2] -
                           background_xlim[1]) * symbol_radius / 100,
                        length(det_summ$mean_lon)),
          add = T, inches = FALSE,
          bg = color[round(det_summ$num_fish
                           / max(det_summ$num_fish) * 100, 0) + 1],
          fg = "black", lwd = 3)
  
  # Add color legend
  plotrix::color.legend(scaleLoc[1], scaleLoc[2], scaleLoc[3], scaleLoc[4], 
    paste0(" ", round(seq(from = 1, to = max(det_summ$num_fish), length.out = 6), 
      0)), color, gradient="y", family = "sans", cex = 0.5, align = 'rb')
  
  # Add x-axis and title
  axis(1, at = xlabs, labels = parse(text = paste0(format(xlabs,4), "*degree")), cex.axis = 1)
  mtext("Longitude", side = 1, line = 2.5, cex = 1)
  
  # Add y-axis and title
  axis(2, at = ylabs, labels = parse(text = paste0(format(ylabs,4), "*degree")), cex.axis = 1, 
    las = 1)
  mtext("Latitude", side = 2, line = 4, cex = 1)
  
  box()
  
  if(!is.na(file_type))	dev.off() 	# Close plot device 

  if(!is.na(file_type)) 
    message(paste0("Image files were written to the following directory:\n", 
      getwd(), "\n"))
  
  return(det_summ)
}		