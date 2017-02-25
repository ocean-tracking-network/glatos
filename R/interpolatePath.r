#' Interpolate new positions within a spatiotemporal path data 
#'
#' Interpolate new positions within a spatiotemporal path data set 
#'   (e.g., detections of tagged fish) at regularly-spaced time intervals   
#' 	 using linear or non-linear interpolation (via \code{\link{movePath}}).
#' 
#' @param dtc A data frame containing spatiotemporal data with at least 
#'   4 columns containing 'individual', 'timestamp','longitude', and 'latitude' 
#'   data and an optional fifth column with the 'type' of record 
#'   (e.g., detection). Default column names match the GLATOS 
#'   detection export file but other names can be specified with 
#'   \code{detColNames}.
#'   
#' @param intTimeStamp The time step size (in seconds) of interpolated 
#'   positions. Default is 86400 (one day).
#'   
#' @param rast An optional transition matrix with the "cost" of moving across 
#'   each cell within the map extent. Must be of class 
#'   \code{TransitionLayer} (See \code{gdistance} package). Passed to  
#'   \code{trans} in \code{\link{movePath}}. 
#'   
#' @param lnlThresh A numeric threshold for determining if linear or non-linear 
#'   interpolation will be used based on the ratio of linear-to-non-linear
#'   shortest path distances. Passed to \code{ithresh} in 
#'   \code{\link{movePath}}.
#'   
#' @param detColNames A list with names of columns in \code{dtc}:
#' \itemize{
#'   \item \code{individualCol} is a character string that uniquely identies an
#'     an indvidual (e.g., tagged animal). Default is 'animal_id'.
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps. Default is 'detection_timestamp_utc'.
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude data. Default is 'deploy_lat'.
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver. Default is 'deploy_long'.
#'	 \item \code{typeCol} is a character string with the name of the optional 
#'     column that identifies the type of record. Default is 'record_type'.} 
#'
#' @details 
#' Interpolation is done by passing each consecutive pair of points to 
#' \code{\link{movePath}} for interpolation via linear or non-linear 
#' methods, depending on \code{rast}.
#' 
#' @details 
#' Non-linear interpolation uses the 'gdistance' package to find the shortest 
#' pathway between two locations (i.e., receivers) that avoid 'impossible' 
#' movements (e.g., over land for fish). The shortest non-linear path between 
#' two locations is calculated using a 'transition matrix layer' (\code{rast}) 
#' that represents the 'cost' of an animal moving between adjacent grid cells. 
#' For example, each cell in \code{rast} may be coded as water (1) or land (0)
#' to represent possible (1) and impossible(0) movement path. 
#' 
#' @details
#' Linear interpolation is used for all points when \code{rast} is not supplied.
#' When \code{rast} is supplied, then interpolation method is determined for 
#' pair of observed positions. For example, linear interpolaton will be used
#' if the two points are exactly the same and when the ratio of linear-to-non-
#  linear shortest path distances exceeds \code{lnlThresh}. \code{lnlThresh} 
#' can be used to control whether non-linear or linear interpolation is used
#' for all points. For example, non-linear interpolation will be used for all
#' points when \code{lnlThresh} = 1 and linear interpolation will be used for 
#' all points when \code{lnlThresh} = 0.        
#'
#' @return A dataframe with id, timestamp, lat, lon, and record type.
#'
#' @seealso \code{\link{movePath}}
#'
#' @author Todd Hayden
#' 
#' @examples
#'
#' --------------------------------------------------
#' EXAMPLE #1 - simple example
#'
#' #example transition matrix
#' data(greatLakesTrLayer)
#'  
#' #example map background
#' data(greatLakesPoly)
#' library(sp) #to plot SpatialPolygon without error
#' plot(greatLakesPoly)
#' 
#' #make up points points
#' pos <- data.frame(
#'   id=1,
#'   x=c(-87,-82.5, -78),
#'   y=c(44, 44.5, 43.5),
#'   time=as.POSIXct(c("2000-01-01 00:00",
#'     "2000-02-01 00:00", "2000-03-01 00:00")))
#'
#' #coerce to SpatialPoints object and plot
#' pts <- SpatialPoints(pos[,c("x","y")])
#' points(pts, pch=20, col='red', cex=3)
#'
#' #interpolate path using linear method
#' path1 <- interpolatePath(pos, 
#'   detColNames=list(individualCol="id", timestampCol="time", 
#' 		 longitudeCol="x", latitudeCol="y"))
#' 
#' #coerce to SpatialPoints object and plot
#' pts1 <- SpatialPoints(path[,c("x","y")])
#' points(pts1, pch=20, col='blue', lwd=2, cex=1.5) 
#'
#' #example transition matrix
#' data(greatLakesTrLayer)
#' 
#' #interpolate path using non-linear method (requires 'trans')
#' path2 <- interpolatePath(pos, 
#'   rast=greatLakesTrLayer,
#'   detColNames=list(individualCol="id", timestampCol="time",
#'   longitudeCol="x", latitudeCol="y"))
#'
#' #coerce to SpatialPoints object and plot
#' pts2 <- SpatialPoints(path2[,c("x","y")])
#' points(pts2, pch=20, col='green', lwd=2, cex=1.5) 
#' 
#' #can also force linear-interpolation with lnlThresh=0
#' path3 <- interpolatePath(pos, 
#'   rast=greatLakesTrLayer, lnlThresh=0,
#'   detColNames=list(individualCol="id", timestampCol="time",
#'   longitudeCol="x", latitudeCol="y"))
#'
#' #coerce to SpatialPoints object and plot
#' pts3 <- SpatialPoints(path3[,c("x","y")])
#' points(pts3, pch=20, col='magenta', lwd=2, cex=1.5) 
#'
#' --------------------------------------------------
#' #EXAMPLE #2 - GLATOS detection data
#' data(walleye_detections) 
#' head(walleye_detections)
#' 
#' #call with defaults; linear interpolation
#' pos1 <- interpolatePath(walleye_detections)
#' 
#' #plot on example map background
#' data(greatLakesPoly)
#' library(sp) #to plot SpatialPolygon without error
#' plot(greatLakesPoly)
#'
#' #coerce to SpatialPoints object and plot
#' pts1 <- SpatialPoints(pos1[pos1$animal_id==3,c("deploy_long","deploy_lat")])
#' points(pts1, pch=20, col='red', cex=0.5)
#' 
#' #example transition matrix
#' data(greatLakesTrLayer)
#' 
#' #call with "transition matrix" (non-linear interpolation), other options
#' # note that it is quite a bit slower due than linear interpolation
#' pos2 <- interpolatePath(walleye_detections, rast=greatLakesTrLayer)
#'
#' #coerce to SpatialPoints object and plot
#' pts2 <- SpatialPoints(pos2[,c("deploy_long","deploy_lat")])
#' points(pts2, pch=20, col='blue', cex=0.5)
#'
#' @export 

interpolatePath <- function(dtc, intTimeStamp=86400, rast=NULL, lnlThresh=0.9,  
	detColNames=list(individualCol="animal_id", 
		timestampCol="detection_timestamp_utc", latitudeCol="deploy_lat", 
		longitudeCol="deploy_long", typeCol="record_type")){

	#update defaultColNames with detColNames
	defaultColNames <- list(
		individualCol="animal_id", 
		timestampCol="detection_timestamp_utc", 
		latitudeCol="deploy_lat", 
		longitudeCol="deploy_long", 
		typeCol="record_type")
	
	for(i in 1:length(detColNames)) 
		defaultColNames[[names(detColNames)[i]]] <- detColNames[[i]]
		
	#Check that detections data frame contains required columns
	missingCols <- setdiff(unlist(defaultColNames[1:4]), names(dtc))
	if (length(missingCols) > 0){
		stop(paste0("'dtc' data frame is missing the following ",
			"column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
			call.=FALSE)
	}
	
	#check for optional type column and add if needed
	missingCols <- setdiff(defaultColNames$typeCol, names(dtc))
	if (length(missingCols) > 0){
		message(paste0("Column '",missingCols,"' was not found in 'dtc', ", 
			" so it was created."))
		#make record_type column
		dtc[,defaultColNames$typeCol] <- "detection"
	}	
	
	# Subset detections with only user-defined columns and change names
	# this makes code more easy to understand (esp. ddply)
	dtc <- dtc[,unlist(defaultColNames)] #subset
	#define column names used only inside this function
	colNamesInternal <- c("individual","timestamp","latitude","longitude","type")
	names(dtc) <- colNamesInternal
		
	# Check that timestamp is of class 'POSIXct'
	if(!('POSIXct' %in% class(dtc$timestamp))){
		stop(paste0("Column '",defaultColNames$timestampCol,
			"' in 'dtc' data frame must be of class 'POSIXct'."),
			call.=FALSE)
	} 	
	
	# Sort detections by tranmitter id and then by detection timestamp
	dtc <- dtc[order(dtc$individual,dtc$timestamp),]
	
	# create sequence of timestamps based on min/max timestamps in data
	rng <- as.POSIXct(trunc(range(dtc$timestamp), units = 'days'), tz = 'GMT')
	tSeq <- seq(rng[1], rng[2], intTimeStamp)

	# create vector of individual ids
	ids <- unique(dtc$individual)

	# bin data by time interval and add to dtc
	bin <- findInterval(dtc$timestamp, tSeq)
	dtc$bin <- tSeq[bin]

	# make all combinations of animals and detection bins
	fshBin <- expand.grid(individual = ids, bin = tSeq)

	# merge detections and all possible detections 
	#  adds NA to bins where detections did not occur
	dtc <- merge(dtc, fshBin, by.x = c('individual', 'bin'), 
		by.y = c('individual', 'bin'), 
		all = TRUE)
	dtc$bin <- as.numeric(dtc$bin)

	# create dataframe to hold interpolated data
	res <- dtc[0,]

	## loop through individuals, interpolate data
	for(i in 1:length(ids)){
		if(i==1) {
			message("Interpolating positions...")
			pb <- txtProgressBar(style=3)
		}
		
		#initialize progress bar
		dtc.i <- dtc[dtc$individual == ids[i],] #subset ith fish
		int <- which(!is.na(dtc.i$latitude)) #identify records to interpolate
		for(j in 1:(length(int)-1)){
			if(int[j+1] - int[j] > 1){
					#make objects to be passed to movePath
					startY <- dtc.i$latitude[int[j]]
					startX <- dtc.i$longitude[int[j]]
					endY <- dtc.i$latitude[int[(j+1)]]
					endX <- dtc.i$longitude[int[(j+1)]]
					startTS <- dtc.i$timestamp[int[j]]
					endTS <- dtc.i$timestamp[int[j+1]]
					iTime <- c(as.numeric(startTS), 
						dtc.i$bin[dtc.i$bin > as.numeric(startTS) & 
							dtc.i$bin < as.numeric(endTS) & 
							is.na(dtc.i$timestamp)], 
						as.numeric(endTS))
					iTime <- as.POSIXct(iTime, origin="1970-01-01", 
						tz=attr(dtc$timestamp, "tzone"))
					
					#interpolate via call to movePath
					iPoints <- movePath(startX, startY, endX, endY, iTime, 
						trans = rast, iThresh=lnlThresh)
					iPoints$type <- "interpolated" #add type
						
					k <- nrow(iPoints)
					temp <- res[0,]
					temp[1:k,] <- NA
					temp[1:k, c("longitude","latitude","timestamp","type")] <- iPoints
					temp$individual <- ids[i]
					res <- rbind(res, temp)
			}
		}
			
		#update progress bar
		setTxtProgressBar(pb, i/length(ids))
	}
	
	close(pb) #close progress bar

	#remove NA rows from dtc
	dtc <- dtc[!is.na(dtc$timestamp),]

	#combine interpolated and real data
	out <- rbind(dtc, res)

	#re-bin timestamps
	bin <- findInterval(out$timestamp, tSeq)
	out$bin <- tSeq[bin]

	#order by individual and timestamp
	out <- out[order(out$individual, out$timestamp),]
					
	#set column names back to original names
	changeNames <- names(out) %in% colNamesInternal
	newNames <- unname(unlist(defaultColNames)[match(names(out)[changeNames], 
		colNamesInternal)])
	names(out)[changeNames] <- newNames
					
	return(out)
}


