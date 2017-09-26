#' Plot detection locations of acoustic transmitters over time
#' 
#' @param detections A data frame containing at least two columns with names 
#'   specified by \code{detColNames} by \code{type}. The 'location' column
#'   contains the locations (typically 'glatos_array' or 'station' for GLATOS
#'   data) that will be plotted on the y-axis. The 'timestamp' column contains
#'   the datetime stamps for the detections (MUST be of class 'POSIXct').
#'   
#' @param type A character string that contains the type of data that is being
#'   passed in, for example, "OTN", "GLATOS", or "sample".
#'   
#' @details detColNames is a list of character strings with names of required
#'   columns in \code{detections}: \itemize{ \item \code{locationCol} is a
#'   character string with the name of the column containing locations you wish
#'   to filter to ('glatos_array' for GLATOS data, 'station' for OTN data, or
#'   'location' for sample data). \item \code{timestampCol} is a character
#'   string with the name of the column containing datetime stamps for the
#'   detections (MUST be of class 'POSIXct') ('detection_timestamp_utc' for
#'   GLATOS data, 'datecollected' for OTN data, or 'time' for sample data). }
#'   
#' @param controlTable An optional data frame with two columns, c('location',
#'   and 'y_order). The 'location' column is a character vector of locations to
#'   be plotted on the y-axis and the name of the 'location' column must match
#'   the 'location' column in \code{detections} data frame (set in
#'   \code{detColNames} by \code{type}). The 'y_order' column specifies what
#'   order the grouping variable will appear on the y-axis (y_order increases as
#'   you move away from the x-axis).
#'   
#' @param plotTitle An optional character string that will appear at the top of 
#'   the plot. Default is no title.
#'   
#' @param Ylab A character string indicating the y-axis label that will appear 
#'   on the figure (default will match \code{detColNames$locationCol}).
#'   
#' @param outFile An optional character string with the name (including 
#'   extension of output file created. File extension will determine type of 
#'   file written. For example, \code{"AbacusPlot.png"} will write a png 
#'   file to the working directory. If \code{NULL} (default) then the plot will 
#'   be printed to the default plot device will be used. Supported extensions: 
#'   png, jpeg, bmp, and tiff.
#'   
#' @param ... Other plotting arguments that pass to "plot" function (e.g., col,
#'   lwd, type).
#'   
#' @details NAs are not allowed in any of the three required columns of 
#'   \code{events}.
#'   
#' @details The control table is used to control which locations will appear in 
#'   the plot and in what order they will appear. If no controlTable is 
#'   supplied, the function will plot only those locations that appear in the 
#'   \code{detections} data frame and the order of locations on the y-axis will 
#'   correspond to the order in which each location appears in the data frame.
#'   
#' @details By default, the function does not distinguish detections from 
#'   different transmitters and will therefore plot all transmitters the same 
#'   color. If more than one fish is desired in a single plot, a vector of
#'   colors must be passed to the function using the 'col =' argument. The color
#'   vector must be the same length as the number of rows in the detections data
#'   frame.
#'   
#' @details #' Alternatively, plots for multiple individual fish can be created 
#'   by looping through and creating a separate plot on subsetted detections
#'   data. Plotting options (i.e., line width and color) can be changed using
#'   optional graphical parameters 
#'   \url{http://www.statmethods.net/advgraphs/parameters.html} that are passed
#'   to "segments" (see ?segments).
#'   
#' @return An image to the default plot device or a file containing the 
#' image if \code{outFile} is specified.
#'   
#' @author T. R. Binder, edited by A. Dini
#'   
#' @examples
#' library(glatos)
#' 
#' data("walleye_detections") #example data
#' 
#' head(walleye_detections)
#' 
#' #subset one transmitter
#' walleye_detections <-
#' 	 walleye_detections[walleye_detections$transmitter_id == 32123, ]
#' 	
#' #plot without control table
#' abacusPlot(walleye_detections, controlTable=NULL, 
#'   plotTitle = "TagID: 32123" col = "red")
#' 	
#' #get example control table
#' data("walleye_controlTable") #example dataset
#' walleye_controlTable
#' 
#' #plot with control table
#' abacusPlot(walleye_detections, controlTable=walleye_controlTable, 
#' 	 plotTitle = "TagID: 32123", col = "red")
#'
#' #plot with custom y-axis label and lines connecting symbols
#' abacusPlot(walleye_detections, controlTable=walleye_controlTable, 
#' 	 plotTitle = "TagID: 32123", Ylab="Location (GLATOS Array)",
#' 	 col = "red", type="o")
#'   
#' @export

abacusPlot <- function(detections, type = "GLATOS", detColNames = list(), 
  controlTable = NULL, plotTitle = "", Ylab=NA, 
  outFile = NULL, ...) {
  
  #Check if user has set column names
  if(length(detColNames) == 0) {
    if(type == "sample") { #Set column names for sample data
      detColNames = list(locationCol = "location", 
                         timestampCol = "time")
    } else if(type == "GLATOS") { #Set column names for GLATOS data
      detColNames = list(locationCol = "glatos_array", 
                       timestampCol = "detection_timestamp_utc")
    } else if(type=="OTN"){ #Set column names for OTN data
      detColNames = list(locationCol = "station", 
                         timestampCol = "datecollected")
    } else { #Other type
      stop(paste0("The type '", type, "' is not defined."), call. = FALSE)
    }
  }
  
	# Check that the specified columns appear in the detections data frame
	missingCols <- setdiff(unlist(detColNames), names(detections))
	if (length(missingCols) > 0){
		stop(paste0("Detections data frame is missing the following ",
			"column(s):\n", paste0("       '", missingCols, "'", collapse="\n")), 
			call. = FALSE)
	}
	
	# Subset detections with only user-defined columns and change names
	# this makes code more easy to understand (esp. ddply)
	detections <- detections[ ,unlist(detColNames)] #subset
	names(detections) <- c("location","timestamp")
		
	# Check that timestamp is of class 'POSIXct'
	if(!('POSIXct' %in% class(detections$timestamp))){
		stop(paste0("Column '", detColNames$timestampCol,
			"' in the detections data frame must be of class 'POSIXct'."),
			call. = FALSE)
	} 

	# If controlTable not supplied, create one - plotted in order locations 
	#  appear in the detections data frame	
	if(is.null(controlTable)){
		controlTable <- data.frame(
			location = unique(detections$location), 
			y_order = 1:length(unique(detections$location)), 
			stringsAsFactors = FALSE)
	} else {
    
		# Check that the specified columns are in the control table data frame
		missingCols <- setdiff(c(detColNames$locationCol, "y_order"), 
		  names(controlTable))
		if (length(missingCols) > 0){
			stop(paste0("Control table data frame is missing the following ",
				"column(s):\n", paste0("       '", missingCols, "'", collapse="\n")), 
				call. = FALSE)
		}
		
		#change name of location column in control table
		controlTable <- controlTable[ ,c(detColNames[[1]], "y_order")] #order cols
		names(controlTable)[1] <- "location"
	}
	
	#update Ylab value if NA
	if(is.na(Ylab)) 
	  Ylab <- detColNames$locationCol
		
	# Merge detections and controlTable data frames
	# Keep only locations that appear in the controlTable data frame
	detections <- merge(detections, controlTable, by = "location", all.y = TRUE)
	
	#sort by timestamp
	detections <- detections[order(detections$timestamp), ] 
   
	# Variable which scales the height of the y-axis depending on the number of 
	# labels to appear. 
	# Assumes 24 labels is the perfect spacing for height = 1000 px.
	pngHeight <- max((nrow(controlTable)/24)*1000, 500)
	
	# Calculate a y-axis label offset to accommodate grouping variables with 
	# different string lengths (e.g., "DRM" vs "DRM-001").
	YlabOffset <- (max(nchar(detections$location)) - 3) / 3
	

	#get file extension
  file_type <- ifelse(is.null(outFile), NA, tools::file_ext(outFile))
	
	#check file extension is supported
  ext_supp <- c(NA, "png", "jpeg", "png", "bmp", "tiff")
	if(!(tolower(file_type) %in% ext_supp))
	  stop(paste0("Image type '", file_type, "' is not supported."), 
	    call. = FALSE)
    
  if(!is.na(file_type) & tolower(file_type) == 'png')
	  png(outFile, height = pngHeight, width = 1000, pointsize = 22)
  if(!is.na(file_type) & tolower(file_type) == 'jpeg')
    jpeg(outFile, height = pngHeight, width = 1000, pointsize = 22)
  if(!is.na(file_type) & tolower(file_type) == 'bmp')
    bmp(outFile, height = pngHeight, width = 1000, pointsize = 22)
  if(!is.na(file_type) & tolower(file_type) == 'tiff')
    tiff(outFile, height = pngHeight, width = 1000, pointsize = 22)  
	
    
  # Set inner and outer margins
	par(mar = c(1, 1, 1.5, 2), oma = c(3, 4 + YlabOffset, 0, 0))
		
	# Plot detection data
	with(detections, 
			plot(timestamp, y_order, 
			  xlim = range(timestamp, na.rm = TRUE), 
				ylim = c(1,length(unique(location))), 
				pch = 16, main = plotTitle, yaxt = "n", xaxt = "n", ylab = "", 
				xlab = "", ...))
		
		# Add custom axes
	axis(2, at = controlTable$y_order, 
		labels = controlTable$location, las = 1)
	xmaj <- seq(from = min(detections$timestamp, na.rm = TRUE), 
		to = max(detections$timestamp, na.rm = TRUE), length.out = 5)
	axis(1, at = xmaj, labels = format(xmaj, "%Y-%m-%d"), las = 1)
		
	# Add axes titles
	mtext("Date", side = 1, line = 2.2, cex = 1.2)
	mtext(Ylab, side = 2, line = 3.5 + YlabOffset, cex = 1.2)

	if(!is.na(file_type))	{
	  dev.off()

  	#get output directory
  	outDir <- ifelse(dirname(outFile) == ".", getwd(), dirname(outFile)) 
  	message(paste0("Output file is located in the following directory:\n", 
  		outDir))	
	}
}