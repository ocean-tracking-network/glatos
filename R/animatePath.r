#' Create an animated video of spatiotemporal path data
#' 
#' Create a set of frames (png image files) showing geographic location data
#' (e.g., detections of tagged fish or interpolated path data) at discrete 
#' points in time and stitch frames into a video animation (mp4 file).    
#' 
#' @param procObj A data frame created by \code{\link{interpolatePath}} 
#'   function.
#' @param recs A data frame containing at least four columns with 
#'   receiver 'lat', 'lon', 'deploy_timestamp', and 
#'   'recover_timestamp'. Default column names match GLATOS standard receiver 
#'   location file \cr(e.g., 'GLATOS_receiverLocations_yyyymmdd.csv'), but 
#'   column names can also be specified with \code{recColNames}.
#' @param outDir A character string with file path to directory where 
#'   individual frames for animations will be written.
#' @param background An optional object of class \code{SpatialPolygonsDataFrame} 
#'   to be used as background of each frame. Default is a simple polygon
#'   of the Great Lakes (\code{greatLakesPoly}) included in the 'glatos' 
#'   package.
#' @param backgroundYlim vector of two values specifying the min/max values 
#' 	 for y-scale of plot. Units are same as background argument.
#' @param backgroundXlim vector of two values specifying the min/max values 
#'   for x-scale of plot. Units are same as background argument.
#' @param ffmpeg A character string with path to install directory for ffmpeg. 
#'   This argument is only needed if ffmpeg has not been added to your 
#'   path variable on your computer.  For Windows machines, path must point 
#'   to ffmpeg.exe.  For example 'c:\\path\\to\\ffmpeg\\bin\\ffmpeg.exe'
#' @param plotControl An optional data frame with four columns ('id', 'what', 
#'   'color', and 'marker') that specify the plot symbols and colors for 
#'   each animal and position type. See examples below for an example.
#' \itemize{
#'   \item \code{id} contains the unique identifer of individual animals and 
#'   	 corresponds to 'id' column in 'dtc'. 
#'   \item \code{what} indicates if the options should be applied to observed
#'     positions (detections; 'detected') or interpolated positions 
#'     ('interpolated').
#'   \item \code{color} contains the marker color to be plotted for each 
#'     animal and position type.  
#'   \item \code{marker} contains the marker style to be plotted for each
#'     animal and position type.
#' } 
#' @param procObjColNames A list with names of required columns in 
#'   \code{procObj}: 
#' \itemize{
#'   \item \code{animalCol} is a character string with the name of the column 
#' 		 containing the individual animal identifier.
#'	 \item \code{binCol} contains timestamps that define each frame.
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct').
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latititude of the receiver.
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longititude of the receiver.
#'	 \item \code{typeCol} is a character string with the name of the optional 
#'     column that identifies the type of record. Default is 'record_type'.} 
#' }
#' @param recColNames A list with names of required columns in 
#'   \code{recs}: 
#' \itemize{
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver (typically 'deploy_lat' for 
#'     GLATOS standard detection export data). 
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver (typically 'deploy_long' for 
#'     GLATOS standard detection export data).
#'	 \item \code{deploy_timestampCol} is a character string with the name of 
#'     the column containing datetime stamps for receier deployments (MUST be 
#'     of class 'POSIXct'; typically 'deploy_date_time'for GLATOS standard 
#'     detection export data). 
#'	 \item \code{recover_timestampCol} is a character string with the name of 
#'     the column containing datetime stamps for receier recover (MUST be of 
#'     class 'POSIXct'; typically 'recover_date_time'for GLATOS standard 
#'     detection export data). 
#' }
#'
#' @return Sequentially-numbered png files (one for each frame) and 
#'   one mp4 file will be written to \code{outDir}.
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
#' #example receiver location data
#' data(recLoc_example) 
#' head(recLoc_example)
#' 
#' #call with defaults; linear interpolation
#' pos1 <- interpolatePath(walleye_detections)
#' 
#' #make sure ffmpeg is installed before calling animatePath
#' # and if you have not added path to 'ffmpeg.exe' to your Windows PATH 
#' # environment variable then you'll need to do that  
#' # or set path to 'ffmpeg.exe' using the 'ffmpeg' input argument
#' myDir <- paste0(getwd(),"/frames")
#' animatePath(pos1, recs=recLoc_example, outDir=myDir)
#'  
#' @export

animatePath <- function(procObj, recs, outDir, background=NULL, 
	backgroundYlim = c(41.48, 45.90), backgroundXlim = c(-84.0, -79.5), 
	ffmpeg = NA, plotControl=NULL,
	procObjColNames=list(animalCol="animal_id",binCol="bin",timestampCol="detection_timestamp_utc",
		 latitudeCol="deploy_lat", longitudeCol="deploy_long", typeCol="record_type"),
  recColNames=list(latitudeCol="deploy_lat", 
		longitudeCol="deploy_long", deploy_timestampCol="deploy_date_time", 
		recover_timestampCol="recover_date_time")){
	
	library(sp) #for plotting SpatialPolygonsDataFrame
	
	#try calling ffmpeg
	fftest <- ifelse(is.na(ffmpeg), "ffmpeg -version", paste(ffmpeg,"-version"))		
	ffVers <- try(system(fftest, intern= TRUE)) #if path was not given
	if(inherits(ffVers, "try-error")) stop(paste0('The command "ffmpeg" is not ',
			'available in your system. Please install FFmpeg or add path.\n\n',
			'FFmpeg is available from:\n https://ffmpeg.org/'), call.=FALSE)
		
	# Check that required columns appear in the procObj data frame
	missingCols <- setdiff(procObjColNames, names(procObj))
	if (length(missingCols) > 0){
		stop(paste0("'procObj' data frame is missing the following ",
			"column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
			call.=FALSE)
	}
    
	# Subset procObjwith only required columns and change names
	# this makes the code more easy to understand.
	procObj <- procObj[,unlist(procObjColNames)] #subset in order
	
	names(procObj) <- c("id","bin","timestamp","lat","lon","type") #rename
		
	# Check that timestamp is of class 'POSIXct'
	if(!('POSIXct' %in% class(procObj$timestamp))){
		stop(paste0("Column 'timestamp' in procObj",
			" data frame must be of class 'POSIXct'."), call.=FALSE)
	}  			
		
	# Check that the specified columns appear in recs data frame
	missingCols <- setdiff(unlist(recColNames), names(recs))
	if (length(missingCols) > 0){
		stop(paste0("'recs' data frame is missing the following ",
			"column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
			call.=FALSE)
	}
	
	# Subset recs with only user-defined columns and change names
	#  this makes the code more easy to understand.
	recs <- recs[,unlist(recColNames)] #subset
	names(recs) <- c("lat","lon","deploy_date_time","recover_date_time")		

	# Check that timestamp is of class 'POSIXct'
	if(!('POSIXct' %in% class(recs$deploy_date_time)) | 
		!('POSIXct' %in% class(recs$recover_date_time))){
			stop(paste0("Columns '",recColNames$deploy_timestampCol,"' and '",
				recColNames$recover_timestampCol,"' in 'recs' data frame ",
				"must be of class 'POSIXct'."),call.=FALSE)
	}    		
		

	# add colors and symbols to detections data frame
	if(!is.null(plotControl)){	
		#merge plotControl with procObj
		procObj <- merge(procObj, plotControl, by.x=c("id","type"), 
			by.y=c("id","what"))
	} else {
		#otherwise, assign default colors and symbols
    procObj$color = 'black'
    procObj$marker = 21
	}		

			
	#make output directory if it does not already exist
	if(!dir.exists(outDir))	dir.create(outDir)

  setwd(outDir)
    
	#get sequence of time breaks
  tSeq <- sort(unique(procObj$bin))

	# bin data by time interval and add to recs
	recs$depBin <- findInterval(recs$deploy_date_time, tSeq)
	recs$recBin <- findInterval(recs$recover_date_time, tSeq)

	for(i in 1:length(tSeq)){
		if(i==1) {
			message(paste0("Writing png files to ",outDir,"..."))
			pb <- txtProgressBar(style=3) #initialize progress bar
		}
		
		# subset for plotting
		procObj.i <- procObj[procObj$bin == tSeq[i],]
		
		# extract receivers active during time interval
		recs.i <- recs[,c('lat', 'lon', 'deploy_date_time', 
			'recover_date_time')][recs$recBin >= i & recs$depBin <= i,]

		# plot GL outline and movement points
		png(paste(i, '.png', sep = ''), width = 3200, height = 2400, units = 'px', res = 300)
		
		# plot background image
		par(oma=c(0,0,0,0), mar=c(0,0,0,0))  #no margins
		if(is.null(background)) {
			data(greatLakesPoly) #example in glatos package
			background <- greatLakesPoly 
		}
		plot(background, ylim = c(backgroundYlim) , xlim = c(backgroundXlim), 
			axes = FALSE, lwd = 2)

		# plot fish locations, receivers, clock
		points(x = recs.i$lon, y = recs.i$lat, pch = 21, cex = 2, col = 'tan2', 
			bg = 'tan2')
		points(x = procObj.i$lon, y = procObj.i$lat, pch = procObj.i$marker , 
			cex = 2.0, col = procObj.i$color)
		text(x = -84.0, y = 42.5, as.Date(tSeq[i]), cex = 2.5)
		dev.off()
		
		#update progress bar
		setTxtProgressBar(pb, i/length(tSeq))
	}       

	close(pb)

	message("Compiling video file (mp4)...")
			
	#specify a call to ffmpeg
	ffcall <- '-framerate 30 -i %d.png -c:v libx264 -vf "fps=30, 
		format=yuv420p" animation.mp4'
	ffcall <- ifelse(is.na(ffmpeg), paste("ffmpeg",ffcall), paste(ffmpeg,ffcall))
	system(ffcall)	
  
	message("\n\nVideo and frames have been created at:\n", outDir)
}