#' Plot number of tagged animals or detections on a map
#'
#' Make bubble plots showing the number of fish detected and number of 
#'   detections across a telemetry receiver network.
#'
#' @param detections A data frame containing detection data with at least 
#'   5 columns containing 'location', 'animal', 'timestamp', 'latitude', 
#'   and 'longitude' data. Default column names match GLATOS standard detection 
#'   export file (e.g., '_detectionsWithLocs.csv'), but column names can also 
#'   be specified with \code{detColNames}.
#'   
#' @param detColNames A list with names of required columns in 
#'   \code{detections}: 
#'   
#' \itemize{
#'   \item \code{locationCol} is a character string with the name of the column 
#'   	 containing the locations that will be plotted (typically 'glatos_array'  
#'     or 'station' for GLATOS standard detection export data).
#'   \item \code{animalCol} is a character string with the name of the column 
#' 		 containing the individual animal identifier (typically 'transmitter_id' 
#'     or 'animal_id' for GLATOS standard detection export data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for detections (MUST be of class 
#'     'POSIXct'; typically 'detection_timestamp_utc' for GLATOS standard 
#'     detection export data).  
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver (typically 'deploy_lat' for 
#'     GLATOS standard detection export data). 
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver (typically 'deploy_lat' for 
#'     GLATOS standard detection export data).
#' }
#' 
#' #Should be removed
#' 
#' @param map An optional SpatialPolygonsDataFrame or other
#'   geo-referenced object to be plotted as the background for the plot. The
#'   default is a SpatialPolygonsDataFrame of the Great Lakes (e.g.,
#'   \code{data(greatLakesPoly)}.
#'   
#' @param receiverLocs An optional data frame containing at least 5 columns with 
#'   receiver 'location', 'lat', 'lon', 'deploy_timestamp', and 
#'   'recover_timestamp'. Default column names match GLATOS standard receiver 
#'   location file \cr(e.g., 'GLATOS_receiverLocations_yyyymmdd.csv'), but 
#'   column names can also be specified with \code{recColNames}.
#'   
#' @param recColNames A list with names of required columns in 
#'   \code{receiverLocs}: 
#' \itemize{
#'   \item \code{locationCol} is a character string with the name of the column 
#'   	 containing the locations that will be plotted (typically 'glatos_array' 
#'     or 'station' for GLATOS standard detection export data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver (typically 'deploy_lat' for 
#'     GLATOS standard detection export data). 
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver (typically 'deploy_long' for 
#'     GLATOS standard detection export data).
#'	 \item \code{deploy_timestampCol} is a character string with the name of 
#'     the column containing datetime stamps for receiver deployments (MUST be 
#'     of class 'POSIXct'; typically 'deploy_date_time'for GLATOS standard 
#'     detection export data). 
#'	 \item \code{recover_timestampCol} is a character string with the name of 
#'     the column containing datetime stamps for receier recover (MUST be of 
#'     class 'POSIXct'; typically 'recover_date_time'for GLATOS standard 
#'     detection export data). 
#' }
#' 
#' @param mapPars A list of optional mapping parameters (with exact names 
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
#' }
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
#'   ("BubblePlot_summaryNumDetections.png") are also written to the working 
#'   directory. Summary data for each plot are also written to CSV files 
#'   in the working directory.
#'
#' @author T. R. Binder
#' 
#' @examples
#' #example detection data
#' data(walleye_detections) 
#' head(walleye_detections)
#'
#' #call with defaults
#' detectionBubblePlot(walleye_detections)
#' 
#' #example receiver location data
#' data(recLoc_example) 
#' head(recLoc_example)
#' 
#' #view example map background
#' plot(ocean_poly)
#' 
#' detectionBubblePlot(walleye_detections, receiverLocs=recLoc_example,
#'   mapParms=list(symbolRadius = 1.4,colGrad = c("white", "blue"), showAll=T))
#'
#' @export

# My changes:
# Changed the column names to work with OTN, GLATOS, and sample data
# Changed map parameters to use OTN area
# Changed map (SpatialPolygonDataFrame) to use data from NaturalEarthData

# To use:
# For sample data, detectionBubble(detSampleDBP, recSampleDBP, "sample", "/Users/dinian/Desktop/glatos/data/ne_110m_ocean/ne_110m_ocean.shp")
# For glatos data, abacusPlot(glatos, glatosR "GLATOS", "/Users/dinian/Desktop/glatos/data/ne_110m_ocean/ne_110m_ocean.shp")
# For OTN data, abacusPlot(otn, otnR, "OTN", "/Users/dinian/Desktop/glatos/data/ne_110m_ocean/ne_110m_ocean.shp")

detectionBubblePlot <- function(detections, receiverLocs = NULL, type, ocean_shapefile="/Users/dinian/Desktop/glatos/data/ne_110m_ocean/ne_110m_ocean.shp"
  #mapPars ...,
  #detColNames,
  #recColNames,
){
  
# detectionBubblePlot <- function(detections, 
# 	receiverLocs = NULL, 
# 	map = NULL,
# 	mapPars = list(
# 		xLimits = c(-94.5, -75), 
# 		yLimits = c(41, 49.5), 
# 		symbolRadius = 1, 
# 		colGrad = c("white", "red"), 
# 		showAll = FALSE),
# 	detColNames = list(
# 		locationCol="glatos_array",
# 		animalCol="animal_id",
# 		timestampCol="detection_timestamp_utc",
# 		latCol="deploy_lat",
# 		longCol="deploy_long"),
# 	recColNames = list(
# 		locationCol="glatos_array",
# 		latCol="deploy_lat",
# 		longCol="deploy_long",
# 		deploy_timestampCol="deploy_date_time",
# 		recover_timestampCol="recover_date_time")){
# 
  
  #Libraries used for SpatialPolygonDataFrame
  library(maptools)
  library(rgdal)
  library(raster)
  library(maps)
  library(mapdata)
  library(marmap)
  library(lattice)
  
  #Map
  #The file that stores shp data from http://www.naturalearthdata.com/ with Ocean polygon (at 110 m) is at "/Users/dinian/Desktop/glatos/data/ne_110m_ocean/ne_110m_ocean.shp"
  layer <- ogrListLayers(ocean_shapefile)
  ogrInfo(ocean_shapefile, layer=layer)
  ocean_poly <- readOGR(ocean_shapefile, layer=layer) #SpatialPolygonDataFrame object
  
  if(type == "GLATOS") {
    detColNames = list(locationCol="glatos_array", animalCol="animal_id", timestampCol="detection_timestamp_utc", latCol="deploy_lat", longCol="deploy_long")
    recColNames = list(locationCol="glatos_array", latCol="deploy_lat", longCol="deploy_long", deploy_timestampCol="deploy_date_time", recover_timestampCol="recover_date_time")
    mapPars = list(
      xLimits = c(-94.5, -75), 
      yLimits = c(41, 49.5), 
      symbolRadius = 1, 
      colGrad = c("white", "red"), 
      showAll = FALSE
    )
    # Assign mapping parameter object default values (independent of mapPars)
    xLimits = c(-94.5, -75)
    yLimits = c(41, 49.5)
    symbolRadius = 1
    colGrad = c("white", "red")
    showAll = FALSE
  } else if (type == "OTN") {
    detColNames = list(locationCol="station", animalCol="catalognumber", timestampCol="datecollected", latCol="latitude", longCol="longitude")
    recColNames = list(locationCol="station", latCol="latitude", longCol="longitude", deploy_timestampCol="deploy_date_time", recover_timestampCol="recover_date_time")
    mapPars = list(
      xLimits = c(-69.4, -59.3), 
      yLimits = c(42.8, 47.6), 
      symbolRadius = 1, 
      colGrad = c("white", "red"), 
      showAll = FALSE
    )
    # Assign mapping parameter object default values (independent of mapPars)
    xLimits = c(-69.4, -59.3) 
    yLimits = c(42.8, 47.6)
    symbolRadius = 1
    colGrad = c("white", "red")
    showAll = FALSE
  } else if (type == "sample") {
    detColNames = list(locationCol="location", animalCol="animal", timestampCol="time", latCol="latitude", longCol="longitude")
    recColNames = list(locationCol="location", latCol="latitude", longCol="longitude", deploy_timestampCol="deploy_time", recover_timestampCol="recover_date_time")
    mapPars = list(
      xLimits = c(-69.4, -59.3), 
      yLimits = c(42.8, 47.6), 
      symbolRadius = 1, 
      colGrad = c("white", "red"), 
      showAll = FALSE
    )
    # Assign mapping parameter object default values (independent of mapPars)
    xLimits = c(-69.4, -59.3)
    yLimits = c(42.8, 47.6)
    symbolRadius = 1
    colGrad = c("white", "red")
    showAll = FALSE
  } else {
    stop(paste0("The type '",type,"' is not defined."), call.=FALSE)
  }
  # Update mapping parameter objects based on user-specified in mapPars	
  for(i in 1:length(mapPars)) 
    assign(names(mapPars)[i], mapPars[[i]]) 
 
	# Check that the specified columns appear in the detections data frame
	missingCols <- setdiff(unlist(detColNames), names(detections))
	if (length(missingCols) > 0){
		stop(paste0("Detections data frame is missing the following ",
			"column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
			call.=FALSE)
	}
    
	# Subset detections with only user-defined columns and change names
	# this makes the code more easy to understand.
	detections <- detections[,unlist(detColNames)] #subset
	names(detections) <- c("location","animal","timestamp","lat","long")
		
	# Check that timestamp is of class 'POSIXct'
	if(!('POSIXct' %in% class(detections$timestamp))){
		stop(paste0("Column '",detColNames$timestampCol,
			"' in 'detections' data frame must be of class 'POSIXct'."),
			call.=FALSE)
	}  	

	# Check that the receiverLocs data frame is supplied if showAll = TRUE
	if(showAll){
		if(is.null(receiverLocs)){
				stop("'receiverLocs' data frame must be specified if showAll = TRUE.")            
		}
		
		# Check that the specified columns appear in receiverLocs data frame
		missingCols <- setdiff(unlist(recColNames), names(receiverLocs))
		if (length(missingCols) > 0){
			stop(paste0("'receiverLocs' data frame is missing the following ",
				"column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
				call.=FALSE)
		}
		
		# Subset receiverLocs with only user-defined columns and change names
		#  this makes the code more easy to understand.
		receiverLocs <- receiverLocs[,unlist(recColNames)] #subset
		names(receiverLocs) <- c("location","lat","long","deploy_timestamp",
			"recover_timestamp")		
	
		# Check that timestamp is of class 'POSIXct'
		if(!('POSIXct' %in% class(receiverLocs$deploy_timestamp)) | 
			!('POSIXct' %in% class(receiverLocs$recover_timestamp))){
				stop(paste0("Columns '",recColNames$deploy_timestampCol,"' and '",
					recColNames$recover_timestampCol,"' in 'receiverLocs' data frame ",
					"must be of class 'POSIXct'."),call.=FALSE)
		}     
   
	
		# Format receiverLocs data frame and subset receiver deployments that 
		#  occurred within the time period between the first and last detection 
		#  in the detections data frame
     
		# Remove receiver locations that do not have both a deploy and recover time 
		# (i.e., ignore receivers for which the data were not in hand)
		receiverLocs <- receiverLocs[!is.na(receiverLocs$deploy_timestamp) & 
			!is.na(receiverLocs$recover_timestamp),]
		
		# Subset receiver locations that did not overlap with the period between 
		#  the first and last detection in the detections data frame
		receiverLocs <- receiverLocs[receiverLocs$deploy_timestamp < 
			max(detections$timestamp, na.rm = TRUE) & 
			receiverLocs$recover_timestamp > min(detections$timestamp, na.rm = TRUE),]
			
		# Determine mean location of receiver groups that were available to for 
		#  detecting fish but did not
		summaryReceivers <- plyr::ddply(receiverLocs, plyr::.(location), plyr::summarise, 
			Summary = 0, meanLat = mean(lat), meanLon = mean(long), .drop = FALSE)
					
		# Retain only those receiver groups that do not already appear in the 
		#  detections data frame
		summaryReceivers <- summaryReceivers[!(summaryReceivers$location %in% 
			unique(detections$location)),]
	}
    
	# Summarize number of unique fish detected and mean lat and lon for each 
	#  receiver group
	summaryNumFish <- plyr::ddply(detections, plyr::.(location), plyr::summarise, 
		Summary = length(unique(animal)), meanLat = mean(lat), meanLon = mean(long), 
		.drop = FALSE)
    
	# If showAll = TRUE, append receiver groups with no detections to the summary
	if(showAll){
			summaryNumFish <- rbind(summaryNumFish, summaryReceivers)
	}
    
	write.csv(summaryNumFish, "NumFish.csv", row.names = FALSE)

	
	# Summarize number of detections and mean lat and lon for each receiver group
	summaryNumDetections <- plyr::ddply(detections, plyr::.(location), plyr::summarise, 
		Summary = length(timestamp), meanLat = mean(lat), meanLon = mean(long), 
		.drop = FALSE)
    
	# If showAll = TRUE, append receiver groups with no detections to the summary
	if(showAll){
			summaryNumDetections <- rbind(summaryNumDetections, summaryReceivers)
	}
	
	write.csv(summaryNumDetections, "NumDetections.csv", row.names = FALSE)
	
	# Re-order the summaries so that sites with detections plot on top of sites 
	#  without. Makes it easier to see detected locations when they are close 
	#  enough together that the bubbles overlap    
	summaryNumFish <- summaryNumFish[order(summaryNumFish$Summary),]
	summaryNumDetections <- summaryNumDetections[order(
		summaryNumDetections$Summary),]
	
	# Create labs with degrees symbol for plots
	xlabs <- round(seq(from = xLimits[1], to = xLimits[2], 
		length.out = 5),2)
	ylabs <- round(seq(from = yLimits[1], to = yLimits[2], 
		length.out = 5),2)
	
	# Define the color palette used to color-code the bubbles
	color <- c(colorRampPalette(colGrad)(101))
	
	# Calculate the location to plot the color scale
	scaleLoc <- c(xLimits[1] + ((xLimits[2] - xLimits[1])*0.025), 
		yLimits[1] + ((yLimits[2] - yLimits[1])*0.25), xLimits[1] + ((xLimits[2] - 
		xLimits[1])*0.05), yLimits[2] - ((yLimits[2] - yLimits[1])*0.25))
	
	# Calculate the aspect ratio for the figure
	figRatio <- (yLimits[2] - yLimits[1])/(xLimits[2] - xLimits[1])*1.4
    
	# Make two plots, one for each number of unique fish and number of detections    
	for(i in c("summaryNumFish", "summaryNumDetections")){
		
		temp <- get(i)
		
		# Open png file
		png(filename = paste0("BubblePlot_", i, ".png"), height = 1000*figRatio, 
			width = 1000, pointsize = 28)
			
		# Set margins
		par(mar = c(1, 0, 0, 2), oma = c(3, 5, 1, 0))	    
		
		# Plot background image
		plot(ocean_poly, xlim = xLimits, ylim = yLimits, axes = T, asp = 1.4, xaxs = "i", 
			lwd = 1.5, xaxt = 'n', yaxt = 'n', col = "White", bg="WhiteSmoke")
		
		if(showAll){
			# Plot the bubbles for zeros
			symbols(temp$meanLon[temp$Summary == 0], temp$meanLat[temp$Summary == 0], 
				circles = rep((xLimits[2]-xLimits[1])*symbolRadius/100, 
				length(temp$meanLon[temp$Summary == 0])),add = T, inches = FALSE, 
				bg = "white", fg = "black", lwd = 3)
			# Add 'X' to bubbles with no detections
			text(temp$meanLon[temp$Summary == 0], temp$meanLat[temp$Summary == 0], 
				"X", cex=0.6*symbolRadius)
		}
		 # Plot the bubbles for non-zeros
		symbols(temp$meanLon[temp$Summary != 0], temp$meanLat[temp$Summary != 0], 
			circles = rep((xLimits[2]-xLimits[1])*symbolRadius/100, 
			length(temp$meanLon[temp$Summary != 0])),add = T, inches = FALSE, 
			bg = color[round(temp$Summary[temp$Summary != 0]/
			max(temp$Summary)*100, 0) + 1], fg = "black", lwd = 3)
		
		# Add color legend
		plotrix::color.legend(scaleLoc[1], scaleLoc[2], scaleLoc[3], scaleLoc[4], 
			paste0(" ", round(seq(from = 1, to = max(temp$Summary), length.out = 6), 
				0)), color, gradient="y", family = "sans", cex = 0.5, align = 'rb')
		
		# Add x-axis and title
		axis(1, at = xlabs, labels = paste0(format(xlabs,5), "°"), cex.axis = 1)
		mtext("Longitude", side = 1, line = 2.5, cex = 1)
		
		# Add y-axis and title
		axis(2, at = ylabs, labels = paste0(format(ylabs,4), "°"), cex.axis = 1, 
			las = 1)
		mtext("Latitude", side = 2, line = 4, cex = 1)

		box()
	
		# Close the png file       
		dev.off()
	}
   
	message(paste0("png files were written to the following directory:\n", 
		getwd(),"\n"))
	
	return(list(summaryNumFish=summaryNumFish, 
		summaryNumDetections=summaryNumDetections))
}		
