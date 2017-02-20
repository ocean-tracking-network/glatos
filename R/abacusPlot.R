#' Plot detection locations of acoustic transmitters over time
#'
#' Plot detection locations of acoustic transmitters over time
#'
#' @param detections A data frame containing at least two columns with names 
#'   specified by \code{detColNames}. The 'location' column contains the 
#'   locations (typically 'glatos_array' or 'station' for GLATOS data) that 
#'   will be plotted on the y-axis. The 'timestamp' column contains the 
#'   datetime stamps for the detections (MUST be of class 'POSIXct').
#' @param detColNames A list of column names: 
#'  	\item{locationCol}{A character scalar with the name (in quotes) of the 
#'      column containing the location codes to be plotted on the y-axis.
#'      The default value ("glatos_array") is consistent with GLATOS standard.} 
#'   	\item{timestampCol}{A character scalar with the name (in quotes) of the 
#'      column containing the timestamp data to be plotted on the x-axis.
#'      The default value ("detection_timestamp_utc") is is consistent with 
#'      GLATOS standard.}
#' @param controlTable Optional dataframe with two columns, c('location', and 
#'   'y_order). The 'location' column is a character vector of locations to be 
#'   plotted on the y-axis and the name of the 'location' column must match the 
#'   'location' colum in \code{detections} dataframe 
#'   (set by \code{detColNames}). The 'y_order' column specifies what order the 
#'   grouping variable will appear on the y-axis (y_order increases as you move 
#'   away from the x-axis).
#' @param plotTitle An optional character scalar that will apear at the top of 
#'   the plot. Default is no title.
#' @param outFile An optional character scalar with the name of the png file 
#'   created (including file extension; default = "AbacusPlot.png").
#' @param ... Other plotting arguments that pass to "plot" function 
#'   (e.g., col, lwd, type).
#'
#' @return A png file containing the abacus plot (called "AbacusPlot.png") is 
#'   written to the working directory.
#'
#' @details The control table is used to control which locations will appear in 
#' the plot and in what order they will appear. If no controlTable is 
#' supplied, the function will plot only those locations that appear in the 
#' detections dataframe. The order of locations on the y-axis will correspond 
#' to the order in which each location appears in the dataframe.\cr\cr
#' By default, the function does not distinguish detections from different 
#' transmitters and will therefore plot all transmitters the same color. 
#' If more than one fish is desired in a single plot, a vector of colors must 
#' be passed to the function using the 'col =' argument. The color vector must 
#' be the same length as the number of rows in the detections dataframe. 
#' Alternatively, plots for multiple individual fish can be created by looping 
#' through and creating a separate plot on subsetted detections data.\cr\cr
#' Plotting options (i.e., symbol size, plotting symbol, lines, and symbol 
#' colors) can be changed using optional graphical parameters 
#' (http://www.statmethods.net/advgraphs/parameters.html) that are passed to 
#' "plot" (see ?plot for more information).
#'
#' @author T. R. Binder
#'
#' @examples
#' data("walleye_detections") #xample data
#'
#' #subset one transmitter
#' walleye_detections <- 
#'   walleye_detections[walleye_detections$transmitter_id == 32123,  ]
#'
#' abacusPlot(walleye_detections, controlTable=NULL, plotTitle = "TagID: 32123", 
#'  	outFile="AbacusPlot_tag32123.png", col = "red")
#'
#' @export

abacusPlot <- function(detections, detColNames=list(locationCol="glatos_array", 
	timestampCol="detection_timestamp_utc"), controlTable = NULL, 
	  plotTitle = "", outFile = "AbacusPlot.png", ...){
		
	# Check that the specified columns appear in the detections dataframe
	missingCols <- setdiff(unlist(detColNames), names(detections))
	if (length(missingCols) > 0){
		stop(paste0("Detections dataframe is missing the following ",
			"column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
			call.=FALSE)
	}
	
	# Check that timestamp is of class 'POSIXct'
	if(!('POSIXct' %in% class(detections[[detColNames$timestampCol]]))){
		stop(paste0("Column '",detColNames$timestampCol,
			"' in the detections dataframe must be of class 'POSIXct'."),
			call.=FALSE)
	} 

	# If controlTable not supplied, create one - plotted in order locations 
	#  appear in the detections dataframe	
	if(is.null(controlTable)){
		controlTable <- data.frame(
			unique(detections[[detColNames$locationCol]]), 
			1:length(unique(detections[[detColNames$locationCol]])), 
			stringsAsFactors = FALSE)
		names(controlTable) <- c(detColNames$locationCol, "y_order")
	}
    
	# Check that the specified columns are in the control table dataframe
	missingCols <- setdiff(c(detColNames$locationCol, "y_order"), names(controlTable))
	if (length(missingCols) > 0){
		stop(paste0("Control table dataframe is missing the following ",
			"column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
			call.=FALSE)
	}		
		
	# Merge detections and controlTable dataframes
	# Keep only locations that appear in the controlTable dataframe
	detections <- merge(detections, controlTable, by = detColNames$locationCol, 
		all.y = TRUE)
	#sort by timestamp
	detections <- detections[order(detections[[detColNames$timestampCol]]),] 
   
	# Variable which scales the height of the y-axis depending on the number of 
	# labels to appear. 
	# Assumes 24 labels is the perfect spacing for height = 1000 px.
	pngHeight <- c(nrow(controlTable)/24*1000)
	
	# Calculate a y-axis label offset to accomodate grouping variables with 
	# different string lengths (e.g., "DRM" vs "DRM-001").
	YlabOffset <- (max(nchar(detections[[detColNames$locationCol]]))-3)/3
	
	# Create a png file containing the events plot
	png(outFile, height = pngHeight, width = 1000, pointsize = 22)
			# Set inner and outer margins
			par(mar = c(1,1,1,2), oma = c(3,4 + YlabOffset,0,0))
			# Plot detection data
			plot(detections[[detColNames$timestampCol]], detections$y_order, 
				xlim = c(min(detections[[detColNames$timestampCol]], na.rm = TRUE), 
					max(detections[[detColNames$timestampCol]], na.rm = TRUE)), 
				ylim = c(1,length(unique(detections[[detColNames$locationCol]]))), 
				pch = 16, main = plotTitle, yaxt = "n", xaxt = "n", ylab = "", 
				xlab = "", ...)
			# Add custom axes
			axis(2, at = controlTable$y_order, 
				labels = controlTable[[detColNames$locationCol]], las = 1)
			axis(1, at = seq(from = min(detections[[detColNames$timestampCol]], 
				na.rm = TRUE), to = max(detections[[detColNames$timestampCol]], 
					na.rm = TRUE), length.out = 5), 
				labels = format(seq(from = min(detections[[detColNames$timestampCol]], 
					na.rm = TRUE), to = max(detections[[detColNames$timestampCol]], 
					na.rm = TRUE), length.out = 5), "%Y-%m-%d"), 
				las = 1)
			# Add axes titles
			mtext("Date", side = 1, line = 2.2, cex = 1.2)
			mtext(detColNames$locationCol, side = 2, line = 3.5 + YlabOffset, cex = 1.2)
	dev.off()

	message(paste0("Output file is located in the following directory:\n", 
		getwd()))	
}