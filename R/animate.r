#' Interpolate geographic positions of animal movements within detection data
#'
#' \code{animate} interpolates geographic positions of animal movements within 
#' detection data using linear or non-linear interpolation (via 
#' \code{\link{movePath}}).
#' 
#' @param dtc A data frame containing detection data with at least 
#'   4 columns containing 'animal', 'latitude', 'longitude', and 'timestamp' 
#'   data. Default column names match the GLATOS standard detection 
#'   detection export file but other names can be specified with 
#'   \code{detColNames}.
#' @param detColNames A list with names of required columns in 
#'   \code{detections}: 
#' \itemize{
#'   \item \code{animalCol} is a character string with the name of the column 
#' 		 containing the individual animal identifier.
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct').
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latititude of the receiver.
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longititude of the receiver.}
#' 
#' @param rast An optional transition matrix with the "cost" of moving across 
#'   across each cell within the map extent. Must be of class 
#'   \code{TransitionLayer} (See \code{gdistance} package). Passed to  
#'   \code{trans} in \code{\link{movePath}}. 
#' @param relRecapCtl An optional data frame with seven columns ('id', 
#'  'timestamp', 'location', 'lat', 'lon', 'color', and 'marker') containing
#'  data and plotting parameters about release or recapture events. 
#' \itemize{
#'   \item \code{id} contains the unique identifer of individual animals and 
#'   	 corresponds to 'id' column in 'dtc'. 
#'   \item \code{timestamp} contains date-time stamps (must be of class POSIXct)
#'     when animal was released or recovered.  
#'   \item \code{location} column indicates if the control settings represents
#'     unique release or recovery of animal. All values must be 'start' 
#'     (for release) or 'end' (for recovery; i.e., if fish was caught by angler
#'     ). All animals in \code{dtc} do not need to be included. Either a 
#'     'start' or 'end' record or both can be included for each fish. 
#'   \item \code{lat} contains latitude (decimal degrees; WGS84) of release or 
#'     recovery.  
#'   \item \code{lon} contains longitude (decimal degrees; WGS84) of release or 
#'     recovery.
#'   \item \code{color} contains the marker color to be plotted.  
#'   \item \code{marker} contains the marker style to be plotted. 
#' }
#' @param positionCtl An optional data frame with four columns ('id', 'what', 
#'   'color', and 'marker') that specify the plot symbols and colors for 
#'   each animal and position type. 
#' \itemize{
#'   \item \code{id} contains the unique identifer of individual animals and 
#'   	 corresponds to 'id' column in 'dtc'. 
#'   \item \code{what} indicates if the options should be applied to observed
#'     positions (detections; 'real') or interpolated positions ('int'). Only
#'     values 'real' or 'int' are valid.
#'   \item \code{color} contains the marker color to be plotted for each 
#'     animal and position type.  
#'   \item \code{marker} contains the marker style to be plotted for each
#'     animal and position type.
#' } 
#' @param intTimeStamp The time step size (in seconds) of interpolated 
#'   positions.
#' @param lnlThresh A numeric threshold for determining if linear or non-linear 
#'   interpolation will be used based on the ratio of linear-to-non-linear
#'   shortest path distances. Passed to \code{trans} in \code{\link{movePath}}. 
#'
#' @details 
#' This function provides interpolated positions (and plot control parameters) 
#' at regularly-spaced interval from a set of irregularly-spaced geographic 
#' positions (e.g., telemetry detections). Each pair of positions is passed
#' to \code{\link{movePath}} for interpolation via linear or non-linear 
#' methods, depending on \code{rast}.
#' 
#' @details 
#' Non-linear interpolation uses the 'gdistance' package to find the shortest 
#' pathway between two locations (i.e., receivers) that avoid 'impossible' 
#' movements (e.g., over land for fish). The shortest non-linear path between 
#' two locations is calculated using a 'transition matrix layer' (\code{rast}) 
#' that represents the 'cost' of an animal moving between adjacent grid cells. 
#' For example, each cells in \code{rast} may be coded as water (1) or land (0)
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
#' all points \code{lnlThresh} = 0.        
#'
#' @note If \code{positionCtl} is given, positions will only be interpolated 
#'   for animals identified in that data frame.  
#'
#' @seealso \code{\link{movePath}}
#'
#' @author Todd Hayden
#' 
#' @examples
#' library(glatos)
#'
#' #example detection data
#' data(walleye_detections) 
#' head(walleye_detections)
#' 
#' #call with defaults; linear interpolation
#' pos1 <- animate(walleye_detections)
#' 
#' #plot on example map background
#' data(greatLakesPoly)
#' library(sp)
#' plot(greatLakesPoly)
#'
#' #coerce to SpatialPoints object and plot
#' pts <- SpatialPoints(pos1[pos1$id==3,c("lon","lat")])
#' points(pts, pch=20, col='red', cex=0.5)
#' 
#' #example relRecapCtl data
#' data(walleye_relRecapCtl) 
#' head(walleye_relRecapCtl)
#' 
#' #example positionCtl data
#' data(walleye_positionCtl) 
#' head(walleye_positionCtl)
#' 
#' #call with plot control tables and other options
#' pos2 <- animate(walleye_detections, relRecapCtl=walleye_relRecapCtl,
#' 	 positionCtl=walleye_positionCtl)
#'
#' #coerce to SpatialPoints object and plot
#' pts2 <- SpatialPoints(pos2[,c("lon","lat")])
#' points(pts2, pch=pos2$marker, col=pos2$color, cex=0.5)
#'
#' #example transition matrix
#' data(greatLakesTrLayer)
#' 
#' #call with plot control tables and other options
#' # note that it is quite a bit slower due to non-linear interpolation
#' pos3 <- animate(walleye_detections, rast=greatLakesTrLayer, 
#'   relRecapCtl=walleye_relRecapCtl,
#' 	 positionCtl=walleye_positionCtl)
#'
#' #coerce to SpatialPoints object and plot
#' pts3 <- SpatialPoints(pos3[,c("lon","lat")])
#' points(pts3, pch=pos3$marker, col=pos3$color, cex=0.5)
#'
#' @export 

animate <- function(dtc, rast=NULL, intTimeStamp=86400, relRecapCtl=NULL, 
	positionCtl=NULL, detColNames=list(animalCol="animal_id", 
	timestampCol="detection_timestamp_utc",
	latitudeCol="deploy_lat", longitudeCol="deploy_long"), lnlThresh = 0.9){

	# Check that detections data frame contains specified columns
	missingCols <- setdiff(unlist(detColNames), names(dtc))
	if (length(missingCols) > 0){
		stop(paste0("'dtc' data frame is missing the following ",
			"column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
			call.=FALSE)
	}
	
	# Subset detections with only user-defined columns and change names
	# this makes code more easy to understand (esp. ddply)
	dtc <- dtc[,unlist(detColNames)] #subset
	names(dtc) <- c("id","timestamp","lat","lon")
		
	# Check that timestamp is of class 'POSIXct'
	if(!('POSIXct' %in% class(dtc$timestamp))){
		stop(paste0("Column '",detColNames$timestampCol,
			"' in 'dtc' data frame must be of class 'POSIXct'."),
			call.=FALSE)
	} 	
	
	# add colors and symbols to detections data frame
	if(!is.null(positionCtl)){	
		#get colors and symbols from positionCtl, if given
		dtc <- merge(dtc, positionCtl[,c('id', 'color', 'marker')][
			positionCtl$what=='real',], by.x = 'id', by.y = 'id', all.x = TRUE)
	} else {
		#otherwise, assign default colors and symbols
    dtc$color = 'black'
    dtc$marker = 21
	}

	# combine 'real' dtcs and fish release/recap data (from control file)
	if(!is.null(relRecapCtl)){
			#subset records in relRecapCtl with non-missing locations and timestamps
			relRecov <- relRecapCtl[!(is.na(relRecapCtl$lat) | 
				is.na(relRecapCtl$timestamp)),]
			
			#add release and recovery records to detections
			dtc <- rbind.data.frame(relRecov[,-3], dtc)

			## subset all non-missing data:
			dtc <- dtc[!is.na(dtc$marker),]
	}

	# Sort detections by tranmitter id and then by detection timestamp
	dtc <- dtc[order(dtc$id,dtc$timestamp),]
	

	# create sequence of days based on min/max timestamps in data
	rng <- as.POSIXct(trunc(range(dtc$timestamp), units = 'days'), tz = 'GMT')
	tSeq <- seq(rng[1], rng[2], intTimeStamp)

	# create vector of fish
	fish <- unique(dtc$id)

	# bin data by time interval and add to dtc
	bin <- findInterval(dtc$timestamp, tSeq)
	dtc$bin <- tSeq[bin]

	# make all combinations of animals and detection bins
	fshBin <- expand.grid(id = fish, bin = tSeq)

	# merge detections and all possible detections 
	#  adds NA to bins where detections did not occur
	dtc <- merge(dtc, fshBin, by.x = c('id', 'bin'), by.y = c('id', 'bin'), 
		all = TRUE)
	dtc$bin <- as.numeric(dtc$bin)

	# create dataframe to hold interpolated data
	res <- dtc[0,]

	## loop through fish, interpolate data
	for(i in 1:length(fish)){
		if(i==1) {
			message("Interpolating positions...")
			pb <- txtProgressBar(style=3)
		}
		
		#initialize progress bar
		dtc.i <- dtc[dtc$id == fish[i],] #subset ith fish
		int <- which(!is.na(dtc.i$lat))
		for(j in 1:(length(int)-1)){
			if(int[j+1] - int[j] > 1){
					startY <- dtc.i$lat[int[j]]
					startX <- dtc.i$lon[int[j]]
					endY <- dtc.i$lat[int[(j+1)]]
					endX <- dtc.i$lon[int[(j+1)]]
					startTS <- dtc.i$timestamp[int[j]]
					endTS <- dtc.i$timestamp[int[j+1]]
					iTime <- c(as.numeric(startTS), 
						dtc.i$bin[dtc.i$bin > as.numeric(startTS) & 
							dtc.i$bin < as.numeric(endTS) & 
							is.na(dtc.i$timestamp)], 
						as.numeric(endTS))
					iTime <- as.POSIXct(iTime, origin="1970-01-01", 
						tz=attr(dtc$timestamp, "tzone"))
					iPoints <- movePath(startX, startY, endX, endY, iTime, 
						trans = rast, iThresh=lnlThresh)
					k <- nrow(iPoints)
					temp <- res[0,]
					temp[1:k,] <- NA
					temp[1:k, match(names(iPoints), names(temp)) ] <- iPoints
					temp$id <- fish[i]
					res <- rbind(res, temp)
			}
		}
			
		#update progress bar
		setTxtProgressBar(pb, i/length(fish))
	}
	
	close(pb) #close progress bar

	#assign colors and markers to default values
	if(is.null(positionCtl)){
			res$color = 'grey'
			res$marker = 21
	} else {

		# clean-up
		res <- res[,c(-6,-7)]

		# assign colors and markers to values specific in positionCtl
		res <- merge(res, positionCtl[,c('id', 'color', 'marker')][
			positionCtl$what=='int',], by.x = 'id', by.y = 'id', all.x = TRUE)
	}

	#remove NA rows from dtc
	dtc <- dtc[!is.na(dtc$timestamp),]

	#combine interpolated and real data
	out <- rbind(dtc, res)

	#re-bin timestamps
	bin <- findInterval(out$timestamp, tSeq)
	out$bin <- tSeq[bin]

	#order by animal and timestamp
	out <- out[order(out$id, out$timestamp),]
					
	return(out)

}


