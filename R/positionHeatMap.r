#' Position Heat Maps
#'
#' Function for creating heat maps to display the spatial distribution of 
#'  acoustic telemetry positions. 
#'
#' @param positions A dataframe containing detection data with at least 4 
#'  columns named c('ID', 'timestamp', 'lat', 'lon'). The ID column contains 
#'  a an individual identifier. The 'timestamp' column contains the 
#'  datetime stamps for the positions (MUST be of class 'POSIXct'). The 
#'  columns 'lat' and 'lon' are the position latitudes and longitudes, 
#'  respectively.
#'  
#' @param  resolution A numeric value indicating the spatial resolution 
#'  (in meters) of the grid system used to make the heat maps. Default is 10 m.
#'
#' @param interval A numeric value indicating the duration of each time bin 
#'  (in seconds) for use in calculating NumIntervals (i.e., a surrogate for 
#'  amount of time spent in each cell of the grid). Default is 3600 seconds 
#'  (1 hour).
#'  
#' @param legendPos A character string indicating whether the legend should 
#'  be displayed on the right side or bottom of the png image. Valid arguments 
#'  are c("right", "bottom"). Default is "right".
#'
#' @param AbsOrRelFish A character string indicating whether number of 
#'  unique fish will display as absolute value (i.e, the actual number of fish 
#'  positioned in each grid) or as relative number (i.e., % of total fish 
#'  positioned). Valid arguments are c("absolute", "relative"). Default is 
#'  "absolute".
#'  
#' @param AbsOrRelPos A character string indicating whether number of 
#'  positions will display as absolute value (i.e, the actual number of 
#'  positions in each grid) or as relative number (i.e., mean number of 
#'  positions per fish detected). Valid arguments are c("absolute", 
#'  "relative"). Default is "absolute".
#'  
#' @param AbsOrRelInt A character string indicating whether NumIntervals 
#'  will display as absolute value (i.e, the actual number of detection 
#'  intervals in each grid) or as relative number (i.e., mean number of 
#'  intervals per fish detected ). Valid arguments are c("absolute", 
#'  "relative"). Default is "absolute".
#'  
#' @param folder A character string indicating the name of the output folder 
#'  (placed in the working directory). This will also be the name of the kml 
#'  file for viewing the heat maps in Google Earth. Default is 
#'  "PositionsHeatMaps".
#'  
#' @param inputTZ A character string specifying the timezone of the timestamps 
#'  in the positions dataframe (default = "GMT").
#'  
#' @details NumIntervals is the number of unique fish x interval 
#' combinations that occurred each grid cell. For example, in 4 hours there 
#' are a total of 4 1-h intervals. If fish 'A' was positioned in a single grid 
#' cell during 3 of the 4 intervals, than NumIntervals for that fish and grid 
#' combination is 3. Intervals are determined by applying the findInterval 
#' function (base R) to a sequence of timestamps (class: POSIXct) created 
#' using seq(from = min(positions[, timestamp]), to = 
#' min(positions[, timestamp]), by = interval), where interval is the 
#' user-assigned interval duration in seconds. NumIntervals is a more 
#' robust surrogate than NumPositions for relative time spent in each grid 
#' in cases where spatial or temporal variability in positioning probability 
#' are likely to significantly bias the distribution of positions in the array.
#' 
#' @details NumFish, NumPositions, and NumIntervals can all be displayed as 
#' absolute or relative, which are specified using the AbsOrRelXXX arguments. 
#' "absolute" is the actual value. "relative" is the absolute value divided 
#' by the total number of fish positioned. Units for plots: NumFish = number 
#' of unique fish (absolute) or % of total fish positioned (relative); 
#' NumPositions = number of positions (absolute) or mean number of positions 
#' per fish (relative); NumIntervals = number of unique fish x interval 
#' combinations (absolute) or mean number of unique fish x interval 
#' combinations per fish (relative). 
#' 
#' @return 
#' 3 png files (one for each of number of positions, number of fish, 
#'  and number of intervals).
#' 1 kml file for viewing the heat maps in Google Earth.
#' 3 CSV files containing summary data used to create the plots.
#' 
#' @author T. R. Binder
#' 
#' @examples
#' 
#' 
#' @export

positionHeatMap <- function (positions, resolution=10, interval=3600, 
  legendPos="right", AbsOrRelFish="absolute", AbsOrRelPos="absolute", 
  AbsOrRelInt="absolute", folder="PositionHeatMaps") {
	
	# Check that the require columns appear in the detections dataframe
    if (sum(c("location", "ID", "timestamp", "lat", "lon") %in% names(positions)) != 4){
        stop(paste0("Error: The columns 'ID', 'timestamp', 'lat', and 'lon' must appear in the positions dataframe."))
    }
    
    # Check that timestamp is of class 'POSIXct'
    if(!('POSIXct' %in% class(positions$timestamp))){
        stop(paste0("Error: Column 'timestamp' in the positions dataframe must be of class 'POSIXct'."))
    } 
    
	# Loads required packages.
	library(geosphere)
	library(raster)
	library(SDMTools)
	library(plotrix)

	
	# Creates a new directory to contain the results based on the folder name chosen by the user.
	filePath <- file.path(paste(getwd(), folder,sep="/"))
	dir.create(filePath, showWarnings = FALSE) 
		
	# Determine the total number of unique transmitters positioned.	
	totalFish <- length(unique(positions$ID))
	
	# Creates a sequence using the inputted interval and first and last position times in the positions dataframe.
    intervalSeq <- seq(from = as.POSIXct(as.Date(min(positions$timestamp))), to = as.POSIXct(as.Date(max(positions$timestamp) + 86400)), by = interval)
    
    # Determine during which time interval each position occurred.
    positions$Interval = findInterval(positions$timestamp, intervalSeq)

	
	# Determines bin size for latitude and longitude based on the resolution (in meters) chosen by the user.		
	binSizeLongitude <- (max(positions$lon) - min(positions$lon))/distMeeus(c(max(positions$lon), max(positions$lat)), c(min(positions$lon), max(positions$lat))) * resolution
	binSizeLatitude <- (max(positions$lat) - min(positions$lat))/distMeeus(c(max(positions$lon), max(positions$lat)), c(max(positions$lon), min(positions$lat))) * resolution
	
	# Defines the grid locations.
	seqLongitude <- seq(min(positions$lon), max(positions$lon), by = binSizeLongitude)
	seqLatitude <- seq(min(positions$lat), max(positions$lat), by = binSizeLatitude)
	
	# Determines in which grid number each position resides.
	positions$BinLon <- findInterval(positions$lon, seqLongitude)
	positions$BinLat <- findInterval(positions$lat, seqLatitude)
	
	# Calculate NumPositions
	# Creates a matrix of the number of positions in each grid. Used to plot NumPositions when AbsOrRelPos == "absolute".
	NumPositions <- as.matrix(unclass(table(factor(positions$BinLat, levels=max(positions$BinLat):1), factor(positions$BinLon, levels=1:max(positions$BinLon)))))	
	# Sets all grids with no positions to NA so they will appear transparent in the png file.
	NumPositions[NumPositions == 0] <- NA
	# Convert NumPositions to NumPositions/fish. Used to plot NumPositions when AbsOrRelPos == "relative".	
	if (AbsOrRelPos == "relative") {NumPositions = NumPositions/totalFish}
	
    # Calculate NumFish
    # Create a new dataframe containing only no-duplicated combinations of ID, BinLat, and BinLon - required for determining the number of unique fish positioned in each grid.
    positions2 <- positions[!(duplicated(positions[, c("ID", "BinLat", "BinLon")])),]
    # Create a matrix of the number of unique transmitters positioned in each grid. Used to plot NumFish when AbsOrRelFish == "absolute".
    NumFish <- as.matrix(unclass(table(factor(positions2$BinLat, levels=max(positions2$BinLat):1), factor(positions2$BinLon, levels=1:max(positions2$BinLon)))))
    # Sets all grids with no fish positions to NA so they will appear transparent in the png file.
    NumFish[NumFish == 0] <- NA    
    # Convert NumFish matrix to %NumFish, relative to the total number of unique transmitters positioned in the system.	Used to plot NumFish when AbsOrRelFish == "relative".
	if (AbsOrRelFish == "relative") {NumFish = NumFish/totalFish*100}
	
	# Calculate NumIntervals
	# Create a new dataframe containing only no-duplicated combinations of ID, BinLat, BinLon, and Interval - required for determining the number of unique fish positioned in each grid.
	positions3 <- positions[!(duplicated(positions[, c("ID", "BinLat", "BinLon", "Interval")])),]
	# Create a matrix containing the number of unique fish x interval combinations in each grid. Used to plot NumIntervals when AbsOrRelInt == "absolute".
	NumIntervals <- as.matrix(unclass(table(factor(positions3$BinLat, levels = max(positions3$BinLat):1), factor(positions3$BinLon, levels = 1:max(positions3$BinLon)))))
	NumIntervals[NumIntervals == 0] <- NA
	# Convert NumIntervals matrix to NumIntervals per fish (i.e., divides by the total number of fish detected). Used to plot NumIntervals when AbsOrRelInt == "relative".
	if (AbsOrRelInt == "relative") {NumIntervals = NumIntervals/totalFish}

	# Loop through the three matrices (NumPositions, NumFish, NumIntervals), converting each matrix to a raster and writing it to png file - files are written to the working directory and placed in a folder corresponding to the folder name chosen by the user.
	for (j in c(1:3)) {
		# Required for determining file names.
		SummaryMatrixName = c("NumPositions", "NumFish", "NumIntervals")[j]
		AbsOrRel <- get(c("AbsOrRelPos", "AbsOrRelFish", "AbsOrRelInt")[j])
        
        # Read in the summary matrix for plotting.
        nm <- get(SummaryMatrixName)
		png(file = file.path(paste0(filePath, "/", SummaryMatrixName, "_", AbsOrRel, ".png")), bg = 'transparent', height = 2400, width = 2400)
			par(mar = c(0,0,0,0))
			image(raster(nm), col = c(rev(rainbow(100, end = 0.7))), axes = FALSE)
			if (legendPos == "bottom"){
				color.legend(0.4, 0.01, 0.8, 0.02, round(seq(min(nm, na.rm = TRUE), max(nm, na.rm = TRUE), by = (max(nm, na.rm = TRUE) - min(nm, na.rm = TRUE))/4), 0),rev(rainbow(100, end = 0.7)), gradient = "x", font = 2, family = "sans", cex = 3)
			}else{
				color.legend(0.99, 0.4, 1.0, 0.8, round(seq(min(nm, na.rm = TRUE), max(nm, na.rm = TRUE), by = (max(nm, na.rm = TRUE) - min(nm, na.rm = TRUE))/4), 0), rev(rainbow(100, end = 0.7)), gradient = "y", font = 2, family = "sans", cex = 3)
			}	
		dev.off()
	}
	
		
	# Change row and column names for summary data to corresponding longitudes and latitudes for the corresponding grids. 
	rownames(NumFish) <- round(rev(seqLatitude), 6)
	colnames(NumFish) <- round(seqLongitude, 6)
	rownames(NumPositions) <- round(rev(seqLatitude), 6)
	colnames(NumPositions) <- round(seqLongitude, 6)
    rownames(NumIntervals) <- round(rev(seqLatitude), 6)
    colnames(NumIntervals) <- round(seqLongitude, 6)
    
    # Write csv files containing the summary data used to make the plots.
    write.csv(NumFish, paste0(filePath, "/NumFish_", AbsOrRelFish, ".csv"))
    write.csv(NumPositions, paste0(filePath, "/NumPositions_", AbsOrRelPos,".csv"))
    write.csv(NumIntervals, paste0(filePath, "/NumIntervals_", AbsOrRelInt,".csv"))
    
	# Write a text file containing the information required to output a kml file. This is where the N, S, E, and W bounds of the image are defined for rendering the image in Google Earth.
	
    kml <- paste0('<?xml version="1.0" encoding="UTF-8"?>','<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">',
    '<Folder>',
        '<name>',folder,'</name>',
            '<open>',1,'</open>',
            '<GroundOverlay>',
                paste0('<name>NumPositions_',AbsOrRelPos,'</name>'),
                '<Icon>',paste0('<href>',file.path(paste0(filePath, "/NumPositions_", AbsOrRelPos,".png")),'</href>'),'<viewBoundScale>0.75</viewBoundScale>','</Icon>',
                '<LatLonBox>',paste0('<north>',max(positions$lat),'</north>'), paste0('<south>',min(positions$lat),'</south>'),paste0('<east>',max(positions$lon),'</east>'),paste0('<west>',min(positions$lon),'</west>'),'</LatLonBox>',
            '</GroundOverlay>',
            '<GroundOverlay>',
                paste0('<name>NumFish_',AbsOrRelFish,'</name>'),
                '<Icon>',paste0('<href>',file.path(paste0(filePath, "/NumFish_", AbsOrRelFish, ".png")),'</href>'),'<viewBoundScale>0.75</viewBoundScale>','</Icon>',
                '<LatLonBox>',paste0('<north>',max(positions$lat),'</north>'), paste0('<south>',min(positions$lat),'</south>'),paste0('<east>',max(positions$lon),'</east>'),paste0('<west>',min(positions$lon),'</west>'),'</LatLonBox>',
            '</GroundOverlay>',
            '<GroundOverlay>',
                paste0('<name>NumIntervals_',AbsOrRelInt,'</name>'),
                '<Icon>',paste0('<href>',file.path(paste0(filePath, "/NumIntervals_", AbsOrRelInt,".png")),'</href>'),'<viewBoundScale>0.75</viewBoundScale>','</Icon>',
                '<LatLonBox>',paste0('<north>',max(positions$lat),'</north>'),	paste0('<south>',min(positions$lat),'</south>'),paste0('<east>',max(positions$lon),'</east>'),paste0('<west>',min(positions$lon),'</west>'),'</LatLonBox>',
            '</GroundOverlay>',
        '</Folder>',
    '</kml>')

	# Write the kml object to kml text file and places it in the folder containing the three png files.
	write.table(kml, file = file.path(paste0(filePath, "/", folder, ".kml")), col.names = FALSE, row.names = FALSE, quote = FALSE)
	
	return(paste0("Output file are located in a folder named '", folder, "' located in the following directory: ", getwd()))
}	

