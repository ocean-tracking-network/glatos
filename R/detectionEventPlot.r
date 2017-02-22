#' Plot detection event data
#'
#' Create an abacus-like plot for discrete detection event data with start 
#' and end times. Suitable for data like detection events 
#' (i.e., output from the glatos package \code{glatos::detectionEventFilter} 
#' function) and receiver histories (i.e., based on receiverLocations.csv 
#' GLATOS export).
#'
#' @param events A data frame containing at least three columns with 'location',
#'   'eventStart', and 'eventEnd' data. Default column names match those 
#'   produced by \cr 
#'   \code{glatos::detectionEventFilter}, but non-standard column names can be 
#'   specified with \code{eventsColNames}.  
#' @param eventColNames An optional list of character strings with names of
#'   required columns in \code{events}. 
#' \itemize{
#'   \item \code{locationCol} is a character string with the name of the column 
#'   	 containing the locations that will be plotted on the y-axis.
#'   \item \code{eventStartCol} is is a character string with the name of the 
#'     column containing the date-time stamps for the start of each event. Must
#'     be of class POSIXct.
#'   \item \code{eventEndCol} is is a character string with the name of the 
#'     column containing the date-time stamps for the end of each event. Must
#'     be of class POSIXct.
#' }
#' @param controlTable An optional dataframe with two columns: 'location' and 
#'   'y_order'. The 'location' column is a character vector of locations to be 
#'   plotted on the y-axis and the name of the 'location' column must match the 
#'   'location' colum in \code{detections} dataframe 
#'   (set by \code{detColNames}). The 'y_order' column specifies what order the 
#'   grouping variable will appear on the y-axis (y_order increases as you move 
#'   away from the x-axis).
#' @param plotTitle An optional character scalar that will apear at the top of 
#'   the plot. Default is no title.
#' @param Ylab A character scalar indicating the y-axis label that will appear 
#'   on the figure (default will match \code{eventColNames$locationCol}).
#' @param outFile An optional character scalar with the name of the png file 
#'   created (including file extension; default = "eventPlot.png").
#' @param ... Other plotting arguments that pass to "plot" function 
#'   (e.g., col, lwd, type).
#' 
#' @details NAs are not allowed in any of the three required columns of 
#'   \code{events}.
#'
#' @details The control table is used to control which locations will appear in 
#' the plot and in what order they will appear. If no controlTable is 
#' supplied, the function will plot only those locations that appear in the 
#' \code{events} data frame and the order of locations on the y-axis will  
#' correspond to the order in which each location appears in the data frame.
#'
#' @details By default, the function does not distinguish detections from  
#' different transmitters and will therefore plot all transmitters the same  
#' color. If more than one fish is desired in a single plot, a vector of colors  
#' must be passed to the function using the 'col =' argument. The color vector  
#' must be the same length as the number of rows in the events data frame. 
#'
#' @details #' Alternatively, plots for multiple individual fish can be created 
#' by looping through and creating a separate plot on subsetted events data.
#' Plotting options (i.e., line width and color) can be changed using optional 
#' graphical parameters \url{http://www.statmethods.net/advgraphs/parameters.html} 
#' that are passed to "segments" (see ?segments).
#'
#' @return A png file containing the events plot (default name "EventPlot.png")  
#'   is written to the working directory.
#'
#' @author T. R. Binder
#' 
#' @examples
#' library(glatos)
#'
#' #make detection event file using detectionEventFilter function
#' data(walleye_detections)
#' #subset one transmitter
#' walleye_detections <-
#' 	 walleye_detections[walleye_detections$transmitter_id == 32123, ]
#'
#' #make events based on 7-d time threshold
#' walleye_events_7d <- detectionEventFilter(walleye_detections, 
#'	 timeSep = 604800)
#'
#' #plot with defaults
#' detectionEventPlot(walleye_events_7d)
#'
#' #example control table
#' data(walleye_controlTable)
#' head(walleye_controlTable)
#' names(walleye_controlTable)[1] <- "Location" #to match events
#'
#' #plot with controlTable and customized axes
#' detectionEventPlot(walleye_events_7d, controlTable = walleye_controlTable, 
#'   plotTitle = "Receiver History", Ylab = "Location", col = "red", lwd = 3)
#' @export

detectionEventPlot <- function(events, 
	eventColNames = list(locationCol="Location", eventStartCol="FirstDetection", 
		eventEndCol="LastDetection"),
	controlTable = NULL, 
	plotTitle = "", Ylab = NA, outFile = "EventPlot.png", ...){
    
	# Check that the specified columns appear in the events data frame
	missingCols <- setdiff(unlist(eventColNames), names(events))
	if (length(missingCols) > 0){
		stop(paste0("Events data frame is missing the following ",
			"column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
			call.=FALSE)
	}

	# Subset events with only user-defined columns and change names
	# this makes code more easy to understand (esp. ddply)
	events <- events[,unlist(eventColNames)] #subset
	names(events) <- c("location","eventStart","eventEnd")

	# Check that timestamp is of class 'POSIXct'
	if(!('POSIXct' %in% class(events$eventStart)) | 
		!('POSIXct' %in% class(events$eventEnd))){
		stop(paste0("Columns '",eventColNames$eventStartCol,"' and '", 
			eventColNames$eventEndCol,
			"' in the events data frame must be of class 'POSIXct'."),
			call.=FALSE)
	} 

	# If controlTable not supplied, create one - plotted in order locations 
	#  appear in the events data frame	
	if(is.null(controlTable)){
		controlTable <- data.frame(
			location = unique(events$location), 
			y_order = 1:length(unique(events$location)), 
			stringsAsFactors = FALSE)
	} else {
    
		# Check that the specified columns are in the control table data frame
		missingCols <- setdiff(c(eventColNames$locationCol, "y_order"), 
			names(controlTable))
		if (length(missingCols) > 0){
			stop(paste0("Control table data frame is missing the following ",
				"column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
				call.=FALSE)
		}
		
		#change name of location column in control table
		controlTable <- controlTable[,c(eventColNames[[1]],"y_order")] #order cols
		names(controlTable)[1] <- "location"
	}	

	#update Ylab value if NA
	if(is.na(Ylab)) Ylab <- eventColNames$locationCol
	
	# Merge events and controlTable dataframes
	# Keep only locations that appear in the controlTable data frame
	events <- merge(events, controlTable, by = "location", all.y = TRUE)
	
	#sort by timestamp
	events <- events[order(events$eventStart),] 
    
	# Variable which scales the height of the y-axis depending on the number of 
	# labels to appear. 
	# Assumes 24 labels is the perfect spacing for height = 1000 px.
	pngHeight <- c(nrow(controlTable)/24*1000)
	
	# Calculate a y-axis label offset to accomodate grouping variables with 
	# different string lengths (e.g., "DRM" vs "DRM-001").
	YlabOffset <- (max(nchar(events$location))-3)/3
    
	# Create a png file containing the events plot
	png(outFile, height = pngHeight, width = 1000, pointsize = 22)
		# Set inner and outer margins
		par(mar = c(1,1,2,2), oma = c(3,4 + YlabOffset,0,0))
		
		# Create and empty plot and set x and y limits - omit x and y labels
		with(events, 			
			plot(NA, 
				xlim = c(min(eventStart, na.rm = TRUE),	max(eventEnd, na.rm = TRUE)), 
				ylim = c(1,max(y_order, na.rm = TRUE)), 
				main = plotTitle, yaxt = "n", xaxt = "n", ylab = "", xlab = ""))
				
		# Add events as line segments
		with(events, 
			segments(eventStart, y_order, eventEnd, y_order, ...))
			
		# Add custom axes
		axis(2, at = controlTable$y_order, labels = controlTable$location, 
			las = 1)
		xmaj <- seq(from = min(events$eventStart, na.rm = TRUE), 
			to = max(events$eventEnd, na.rm = TRUE), length.out = 5)
		axis(1, at = xmaj, labels = format(xmaj, "%Y-%m-%d"), las = 1)
		
		# Add axes titles
		mtext("Date", side = 1, line = 2.2, cex = 1.2)
		mtext(Ylab, side = 2, line = 3.5, cex = 1.2)
	dev.off()
	
	#get output directory
	outDir <- ifelse(dirname(outFile)==".", getwd(), dirname(outFile)) 
	message(paste0("Output file is located in the following directory:\n", 
		outDir))
}