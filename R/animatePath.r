#' Create an animated video of spatiotemporal path data
#' 
#' Create a set of frames (png image files) showing geographic location data
#' (e.g., detections of tagged fish or interpolated path data) at discrete 
#' points in time and stitch frames into a video animation (mp4 file).    
#' 
#' @param procObj A data frame created by \code{\link{interpolatePath}} 
#'   function.
#'   
#' @param recs A data frame containing at least four columns with 
#'   receiver 'lat', 'lon', 'deploy_timestamp', and 
#'   'recover_timestamp'. Default column names match GLATOS standard receiver 
#'   location file \cr(e.g., 'GLATOS_receiverLocations_yyyymmdd.csv'), but 
#'   column names can also be specified with \code{recColNames}.
#'   
#' @param outDir A character string with file path to directory where 
#'   individual frames for animations will be written.
#'   
#' @param background An optional object of class \code{SpatialPolygonsDataFrame} 
#'   to be used as background of each frame. Default is a simple polygon
#'   of the Great Lakes (\code{greatLakesPoly}) included in the 'glatos' 
#'   package.
#'   
#' @param backgroundYlim vector of two values specifying the min/max values 
#' 	 for y-scale of plot. Units are same as background argument.
#' 	 
#' @param backgroundXlim vector of two values specifying the min/max values 
#'   for x-scale of plot. Units are same as background argument.
#'   
#' @param ffmpeg A character string with path to install directory for ffmpeg. 
#'   This argument is only needed if ffmpeg has not been added to your 
#'   path variable on your computer.  For Windows machines, path must point 
#'   to ffmpeg.exe.  For example, 'c:\\path\\to\\ffmpeg\\bin\\ffmpeg.exe'
#'   
#' @param plotControl An optional data frame with four columns ('id', 'what', 
#'   'color', and 'marker') that specify the plot symbols and colors for 
#'   each animal and position type. See examples below for an example.
#' \itemize{
#'   \item \code{id} contains the unique identifier of individual animals and 
#'   	 corresponds to 'id' column in 'dtc'. 
#'   \item \code{what} indicates if the options should be applied to observed
#'     positions (detections; 'detected') or interpolated positions 
#'     ('interpolated').
#'   \item \code{color} contains the marker color to be plotted for each 
#'     animal and position type.  
#'   \item \code{marker} contains the marker style to be plotted for each
#'     animal and position type. Passed to \code{par()$pch}.
#'   \item \code{marker_cex} contains the marker size to be plotted for each
#'     animal and position type. Passed to \code{par()$cex}.
#'   \item \code{line_color} contains the line color. Passed to 
#'     \code{par()$col}.
#'   \item \code{line_width} contains the line width. Passed to 
#'     \code{par()$lwd}.
#' } 
#' 
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
#'     containing latitude of the receiver.
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver.
#'	 \item \code{typeCol} is a character string with the name of the optional 
#'     column that identifies the type of record. Default is 'record_type'. 
#' }
#' 
#' @param recColNames A list with names of required columns in 
#'   \code{recs}: 
#' \itemize{
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver (typically, 'deploy_lat' for 
#'     GLATOS standard detection export data). 
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver (typically, 'deploy_long' for 
#'     GLATOS standard detection export data).
#'	 \item \code{deploy_timestampCol} is a character string with the name of 
#'     the column containing datetime stamps for receier deployments (MUST be 
#'     of class 'POSIXct'; typically, 'deploy_date_time' for GLATOS standard 
#'     detection export data). 
#'	 \item \code{recover_timestampCol} is a character string with the name of 
#'     the column containing datetime stamps for receier recover (MUST be of 
#'     class 'POSIXct'; typically, 'recover_date_time' for GLATOS standard 
#'     detection export data).
#' }
#' 
#' @param tail_dur contains the duration (in same units as \code{procObj$bin}; 
#'     see \code{\link{interpolatePath}}) of trailing points in each frame. 
#'     Default value is 0 (no trailing points). A value
#'     of \code{Inf} will show all points from start.
#'
#' @return Sequentially-numbered png files (one for each frame) and 
#'   one mp4 file will be written to \code{outDir}.
#' 
#' @author Todd Hayden
#'
#' @examples
#' library(glatos)
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
#' 
#' #add trailing points to include last 15 bins (in this case, days)
#' data(walleye_plotControl)
#' walleye_plotControl$line_color <- "grey60"
#' walleye_plotControl$line_width <- 5
#' animatePath(procObj = pos1, recs = recLoc_example, 
#'   plotControl = walleye_plotControl, outDir=myDir, tail_dur = 15)
#'  
#' @export


library(glatos)
# create procdata for development
# example detection data
## data(walleye_detections) 
## dtc <- walleye_detections
## dtc <- dtc[, c("animal_id", "detection_timestamp_utc", "deploy_lat", "deploy_long")]
## data(greatLakesTrLayer)
## trans <- greatLakesTrLayer
## procObj <- interpolatePath(dtc, trans=greatLakesTrLayer)
## saveRDS(procObj, "procObj.rds")

#development
procObj <- readRDS("procObj.rds")
# example receiver location data
data(recLoc_example) 
data(greatLakesPoly) 
background <- greatLakesPoly
backgroundYlim = c(41.48, 45.90)
backgroundXlim = c(-84.0, -79.5)
recs <- recLoc_example
int_time_stamp <- 86400
outDir <- "~/Desktop/test"

setDT(procObj)
setDT(recs)

#################################
# try calling ffmpeg
# add exe if ffmpeg is directory
# cmd <- ifelse(grepl("ffmpeg.exe$",ffmpeg) | is.na(ffmpeg), ffmpeg, 
# paste0(ffmpeg,"\\ffmpeg.exe"))
# cmd <- ifelse(is.na(ffmpeg), 'ffmpeg', cmd)	
# ffVers <- suppressWarnings(system2(cmd, "-version",stdout=F)) #call ffmpeg
# if(ffVers == 127) stop(paste0('"ffmpeg.exe" was not found.\n',
# 'Ensure it is installed add added to system PATH variable\n',
#   "or specify path using input argument 'ffmpeg'\n\n",
#   'FFmpeg is available from:\n https://ffmpeg.org/'), call.=FALSE)

# Need to add plot colors and symbols back into function.  Also need to add line functionality

  ## # add colors and symbols to detections data frame
  ##       if(!is.null(plotControl)){	
  ##       	#merge plotControl with procObj
  ##       	procObj <- merge(procObj, plotControl, by.x=c("id","type"), 
  ##       		by.y=c("id","what"))
  ##       } else {
  ##       	#otherwise, assign default colors and symbols
  ##   procObj$color = 'black'
  ##   procObj$marker = 21
  ##   procObj$marker_cex = 1
  ##   procObj$line_color = NA
  ##   procObj$line_width = NA
  ##       }		

			
	#make output directory if it does not already exist
#	if(!dir.exists(outDir))	dir.create(outDir)

  #setwd(outDir)
    
# create sequence of timestamps based on min/max timestamps in data
rng <- as.POSIXct(trunc(range(procObj$bin_stamp), units = "days"),
                  tz = "GMT")
t_seq <- seq(rng[1], rng[2], int_time_stamp)

# add bins to processed object for plotting
procObj[, plot_bin := findInterval(bin_stamp, t_seq)]

# create group counter
procObj[, grp := plot_bin]

# remove receivers not recovered (records with NA in recover_date_time)
setkey(recs, recover_date_time)
recs <- recs[!J(NA_real_), c("station", "deploy_lat", "deploy_long", "deploy_date_time", "recover_date_time")]

# bin data by time interval and add to recs
recs[, start := findInterval(deploy_date_time, t_seq)]
recs[, end := findInterval(recover_date_time, t_seq)]
####

#setkey(procObj, start, end)
#setkey(recs, start, end)
#plot_obj <- foverlaps(procObj, recs, type = "within")

# add clock for plot
procObj[, clk := t_seq[plot_bin]]


cust_plot <- function(x, outDir, background = NULL, backgroundYlim = c(41.48, 45.90),
                      backgroundXlim = c(-84.0, -79.5)){

  sub_recs <- recs[between(x$plot_bin[1], lower = recs$start, upper = recs$end)]

  # plot GL outline and movement points 
  png(paste(outDir,"/", x$plot_bin[1], '.png', sep = ''), width = 3200, height = 2400, units = 'px', res = 300)

  # plot background image
  par(oma=c(0,0,0,0), mar=c(0,0,0,0))  #no margins
  if(is.null(background)) {
    data(greatLakesPoly) #example in glatos package
    background <- greatLakesPoly
  }
  
  #note call to plot with sp
  sp::plot(background, ylim = c(backgroundYlim), xlim = c(backgroundXlim), axes = FALSE, lwd = 2)

  # plot fish locations, receivers, clock  
  points(x = sub_recs$deploy_long, y = sub_recs$deploy_lat, pch = 21, cex = 2, col = 'tan2', bg = 'tan2')
  text(x = -84.0, y = 42.5, as.Date(x$clk[1]), cex = 2.5)
  points(x = x$i_lon, y = x$i_lat, pch = 21, col = "red", bg = "red")
  dev.off()
}

# specify marker colors, sizes, etc in function?
procObj[,  cust_plot(x = .SD, outDir = outDir, background = NULL),  by = grp]

	## for(i in 1:length(tSeq)){
	## 	if(i==1) {
	## 		message(paste0("Writing png files to ",outDir,"..."))
	## 		pb <- txtProgressBar(style=3) #initialize progress bar
	## 	}
		
		# subset for plotting
#	  tail_start <- max(i - tail_dur, 1) #so always 1 or larger
        #procObj.i <- procObj[procObj$bin %in% tSeq[tail_start:i], ]
            
		#make lines if specified
		if(any(!is.na(procObj.i$line_color)) | any(!is.na(procObj.i$line_width))){
  		ids <- sort(unique(procObj.i$id))
  		for(j in 1:length(ids)){
  		  procObj.ij <- procObj.i[procObj.i$id == ids[j],]
  		  lines(x = procObj.ij$lon, y = procObj.ij$lat, lwd = procObj.ij$line_width, 
    		  col = procObj.ij$line_color)
  		}
		}
		points(x = procObj.i$lon, y = procObj.i$lat, pch = procObj.i$marker, 
			cex = procObj.i$marker_cex, col = procObj.i$color)
		text(x = -84.0, y = 42.5, as.Date(tSeq[i]), cex = 2.5)
		dev.off()
		
		#update progress bar
		setTxtProgressBar(pb, i/length(tSeq))
	}       

	close(pb)

	message("Compiling video file (mp4)...")

#        explore ffmpeg call with mapmate::ffmpeg

  
	#specify a call to ffmpeg
	ffcall <- sprintf('-framerate 30 -y -i "%s/%%d.png" -c:v libx264 -vf "fps=30, 
		format=yuv420p" "%s/animation.mp4"', outDir, outDir)
	system2(cmd, ffcall, stdout=F)	
  
	message("\n\nVideo and frames have been created at:\n", outDir)
}
