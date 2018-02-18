#' Plot number of tagged animals or detections on a map
#'
#' Make bubble plots showing the number of fish detected and number of 
#'   detections across a telemetry receiver network.
#'
#' @inheritParams detection_events 
#' 
#' @param receiver_locs An optional data frame containing at least 5 columns with 
#'   receiver 'location', 'lat', 'lon', 'deploy_timestamp', and 
#'   'recover_timestamp'.
#'   
#' @param map An optional SpatialPolygonsDataFrame or other
#'   geo-referenced object to be plotted as the background for the plot. It is 
#'   defined by \code{type}. If NULL, then the example Great Lakes polygon 
#'   object (\code{data(greatLakesPoly)}) will be used.
#' 
#' @param out_file An optional character string with the name (including 
#'   extension) of output file created. File extension will determine type of 
#'   file written. For example, \code{"BubblePlot.png"} will write a png 
#'   file to the working directory. If \code{NULL} (default) then the plot will 
#'   be printed to the default plot device will be used. Supported extensions: 
#'   png, jpeg, bmp, and tiff.
#'   
#' @param background_xlim A two-element numeric vector that defines minimum and
#'   maximum extents of the viewable plot area along the x-axis (i.e.,
#'   latitude).
#'   
#' @param background_ylim  two-element numeric vector that defines minimum and
#'   maximum extents of the viewable plot area along the y-axis (i.e.,
#'   longitude).
#'   
#' @param symbol_radius Radius of each "bubble" on the plot in units of percent
#'   of x-axis scale. Default value = 1 (i.e., 1 percent of x-axis).
#'   
#' @param col_grad A two-element character vector indicating the start and end
#'   colors of the gradient scale used to color-code "bubbles".
#' 
#' @details Data are summarized using \link{summarize_detections}.
#'   
#' @details If \code{receiver_locs} is specified (not NULL) then the plot will
#'   show all receivers in \code{receiver_locs} including those that detected
#'   none of the transmitters in \code{det}. Although this is helpful to view
#'   locations where fish were \emph{not} detections, it will also show and
#'   summarize data based on receivers that were not deployed during the time
#'   that a fish was at large. Therefore, \code{receiver_locs} only represent a
#'   subset of receivers present during the time interval most relevant to the
#'   data in \code{det}.
#'   
#' @details "col_grad" is used in a call to \link[grDevices]{colorRampPalette},
#'   which will accept a vector containing any two colors return by
#'   \link[grDevices]{colors} as character strings.
#' 
#' @return A data frame produced by 
#'   \code{glatos::summarize_detections(det, location_col = location_col, 
#'   receiver_locs = receiver_locs, summ_type = "location")}
#'
#' @return If not out_file is specified, then an image is printed to the 
#'   default plot device. If out_file is specified, then an image of 
#'   specified type is written to \code{out_file}.
#'
#' @seealso \code{\link{summarize_detections}}
#'
#' @author T. R. Binder, edited by A. Dini
#' 
#' @examples
#' 
#' #get path to example detection file
#' det_file <- system.file("extdata", "walleye_detections.csv",
#'   package = "glatos")
#' det <- read_glatos_detections(det_file)
#' 
#' #call with defaults
#' detection_bubble_plot(det)
#' 
#' #change symbol size and color
#' detection_bubble_plot(det, symbol_radius = 2, col_grad = c("grey90", "grey10"))
#' 
#' #Add all receivers
#' 
#' # get path to example receiver file
#' rec_file <- system.file("extdata", "sample_receivers.csv",
#'   package = "glatos")
#' rec <- read_glatos_receivers(rec_file)
#' 
#' detection_bubble_plot(det, receiver_locs = rec)
#' 
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
                                   summ_type = "location")
  
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
      dev.new(noRStudioGD = TRUE, height = 7 * figRatio, width = 7)
  }
  
  # Set margins
  par(mar = c(1, 0, 0, 2), oma = c(3, 5, 1, 0))	    
  
  # Plot background image
  sp::plot(map, xlim = background_xlim, ylim = background_ylim, axes = T, 
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
  
  # Add 'X' to bubbles with no detections
  if(any(det_summ$num_fish == 0)){
    with(det_summ[det_summ$num_fish == 0,], 
      text(mean_lon, mean_lat, 
        "X", cex = 0.6 * symbol_radius))
  }
  
  # Calculate the location to plot the color scale
  scaleLoc <- c(par("usr")[1] + (par("usr")[2] - par("usr")[1])*0.02,
  par("usr")[3] + ((par("usr")[4] - par("usr")[3])* 0.25),
  par("usr")[1] + ((par("usr")[2] - par("usr")[1])* 0.04),
  par("usr")[4] - ((par("usr")[4] - par("usr")[3])* 0.25))
  
  # Add color legend
  plotrix::color.legend(scaleLoc[1], scaleLoc[2], scaleLoc[3], scaleLoc[4], 
    paste0(" ", round(seq(from = 1, to = max(det_summ$num_fish), length.out = 6), 
      0)), color, gradient="y", family = "sans", cex = 1, align = 'rb')
  
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