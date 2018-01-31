#' Create an animated video of spatiotemporal path data
#' 
#' Create a set of frames (png image files) showing geographic location data
#' (e.g., detections of tagged fish or interpolated path data) at discrete 
#' points in time and stitch frames into a video animation (mp4 file).    
#'
#'
#' @param proc_obj A data frame created by \code{\link{interpolatePath}} 
#'   function.
#'   
#' @param recs An optional data frame containing at least four columns with
#'   receiver 'deploy_lat', 'deploy_long', 'deploy_date_time', and
#'   'recover_date_time'. Other columns in object will be ignored.
#'   Default column names match GLATOS standard receiver location file
#'   \cr(e.g., 'GLATOS_receiverLocations_yyyymmdd.csv').
#'
#' @param out_dir A character string with file path to directory where 
#'   individual frames for animations will be written. Default is working directory.
#'   
#' @param background An optional object of class \code{SpatialPolygonsDataFrame} 
#'   to be used as background of each frame. Default is a simple polygon
#'   of the Great Lakes (\code{greatLakesPoly}) included in the 'glatos' 
#'   package.
#'   
#' @param background_ylim vector of two values specifying the min/max values 
#' 	 for y-scale of plot. Units are same as background argument.
#' 	 
#' @param background_xlim vector of two values specifying the min/max values 
#'   for x-scale of plot. Units are same as background argument.
#'   
#' @param ffmpeg A character string with path to install directory for ffmpeg. 
#'   This argument is only needed if ffmpeg has not been added to your 
#'   path variable on your computer.  For Windows machines, path must point 
#'   to ffmpeg.exe.  For example, 'c:\\path\\to\\ffmpeg\\bin\\ffmpeg.exe'
#'
#' @param ani_name Name of animation (character string)
#'
#' @param frame_delete Boolean.  Default (TRUE) delete individual
#'   image frames after animation is created
#'
#' @param animate Boolean. Default (TRUE) creates video animation
#' 
#' @param plot_control An optional data frame with four columns ('animal_id', 'type', 
#'   'color', and 'marker', 'marker_cex') that specify the plot symbols and colors for 
#'   each animal and position type. See examples below for an example.
#'   
#' @param overwrite Overwite the animation file if it already exists. Default is FALSE (file is not #'    overwritten)
#' 
#' @param threshold Threshold time in seconds before cease plotting interpolated points for a fish that leaves one lacation and is not detected elsewhere before returning to that location. Default is NULL (all interpolated points are plotted - appears that the fish was present constantly at the location)
#' 
#' \itemize{
#'   \item \code{animal_id} contains the unique identifier of individual animals and 
#'   	 corresponds to 'animal_id' column in 'dtc'. 
#'   \item \code{type} indicates if the options should be applied to observed
#'     positions (detections; 'real') or interpolated positions 
#'     ('inter').
#'   \item \code{color} contains the marker color to be plotted for each 
#'     animal and position type.  
#'   \item \code{marker} contains the marker style to be plotted for each
#'     animal and position type. Passed to \code{par()$pch}.
#'   \item \code{marker_cex} contains the marker size to be plotted for each
#'     animal and position type. Passed to \code{par()$cex}.
#' } 
#' 
#' @return Sequentially-numbered png files (one for each frame) and 
#'   one mp4 file will be written to \code{out_dir}.
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
#'  pos1 <- interpolatePath(walleye_detections)
#' 
#' #make sure ffmpeg is installed before calling animatePath
#' # and if you have not added path to 'ffmpeg.exe' to your Windows PATH 
#' # environment variable then you'll need to do that  
#' # or set path to 'ffmpeg.exe' using the 'ffmpeg' input argument
#' myDir <- paste0(getwd(),"/frames")
#' animatePath(pos1, recs=recLoc_example, outDir=myDir)
#' 
#'  
#' @export


animatePath <- function(proc_obj, recs = NULL, plot_control = NULL, out_dir = getwd(),
                        background = NULL,
                        background_ylim = c(41.3, 49.0),
                        background_xlim = c(-92.45, -75.87),
                        ffmpeg = NA,
                        ani_name = "animation.mp4",
                        frame_delete = FALSE,
                        animate = TRUE,
                        overwrite = FALSE,
                        threshold = NULL,
                        show_interpolated = TRUE){
  
  # Try calling ffmpeg if animate = TRUE.
  # If animate = FALSE, video file is not produced and there is no need to check for package.
  if(animate == TRUE){
    cmd <- ifelse(grepl("ffmpeg.exe$",ffmpeg) | is.na(ffmpeg), ffmpeg,
                  paste0(ffmpeg,"\\ffmpeg.exe"))
    cmd <- ifelse(is.na(ffmpeg), 'ffmpeg', cmd)	
    ffVers <- suppressWarnings(system2(cmd, "-version", stdout=F)) #call ffmpeg
    if(ffVers == 127)
      stop(paste0('"ffmpeg.exe" was not found.\n',
                  'Ensure it is installed add added to system PATH variable\n',
                  "or specify path using input argument 'ffmpeg'\n\n",
                  'FFmpeg is available from:\n https://ffmpeg.org/\n',
                  'You may create the individual frames and then combine them\n',
                  'into an animation manually using video editing software\n', 
                  '(e.g., Windows Movie Maker or iMovie) by setting the animate\n',
                  'argument to FALSE.'),
           call. = FALSE)
    }
    
    mapmate <- any(installed.packages()[,1] == "mapmate")
    if(mapmate == FALSE){
      stop(paste0("mapmate package is not installed.\n",
               "see: https://github.com/leonswicz/mapmate\n",
               'install: devtools::install_github("leonawicz/mapmate")'),
        call. = FALSE)
    }


  # Convert proc_obj and recs dataframes into data.table objects to inprove speed of script
  setDT(proc_obj)
  if(!is.null(recs)){
    setDT(recs)
    # Remove receivers not recovered (records with NA in recover_date_time)
    setkey(recs, recover_date_time)
    recs <- recs[!J(NA_real_), c("station", "deploy_lat", "deploy_long",
                                 "deploy_date_time", "recover_date_time")]
  }
  
  # Add plotting columns for dealing with fish that leave a site and aren't 
  # detected elsewhere before returning to that site (optional) - uses thershold
  # value that is user defined with argument 'threshold'
  if(!is.null(threshold)){
    setkey(proc_obj, animal_id, bin_timestamp)
    proc_obj <- proc_obj[, .(animal_id, bin_timestamp,
                            latitude, longitude, record_type,
                            diff_time = ifelse(length(bin_timestamp) == 1,
                                                as.numeric(NA),
                                                c(NA,as.numeric(diff(bin_timestamp)))),
                            diff_loc = ifelse(length(bin_timestamp) == 1,
                                              as.numeric(NA), 
                                              c(NA, ifelse(diff(latitude) == 0 &
                                              diff(longitude) == 0, 0, 1)))),
                            by = animal_id]
    
    proc_obj <-  proc_obj[,.(animal_id, bin_timestamp, latitude, longitude, record_type,
                          diff_time, diff_loc,
                          plot = ifelse(diff_time > threshold &
                                          diff_loc == 0, 0, 1))]
  }
  
  # Add colors and symbols to detections data frame
  if(!is.null(plot_control)){
    setDT(plot_control)
    proc_obj <- merge(proc_obj, plot_control, by.x=c("animal_id", "type"),
                     by.y=c("animal_id","type"))
    proc_obj <- proc_obj[!is.na(color)]
  } else {

    # Otherwise, assign default colors and symbols
    proc_obj$color = 'blue'
    proc_obj$marker = 16
    proc_obj$marker_cex = 2
  }

  # Make output directory if it does not already exist
  if(!dir.exists(out_dir)) dir.create(out_dir)

  # Create group identifier for plotting - time bins for plotting individual plots
  proc_obj[, grp := bin_timestamp]

  # extract time sequence for plotting
  t_seq <- unique(proc_obj$bin_timestamp)
  
  # determine leading zeros needed by ffmpeg and add as new column
  char <- paste0("%", 0, nchar((length(t_seq))), "d")
  setkey(proc_obj, bin_timestamp)
  proc_obj[, f_name := .GRP, by = grp]
  proc_obj[, f_name := paste0(sprintf(char, f_name), ".png")]

  if(is.null(background)) {
    data(greatLakesPoly) #example in glatos package
    background <- greatLakesPoly
  }
  
  cust_plot <- function(x, proc_obj, sub_recs, out_dir, background, background_xlim,
                        background_ylim, show_interpolated){

    if(!is.null(recs)){  
    # extract receivers in the water during plot interval
    sub_recs <- recs[between(x$bin_timestamp[1],
                             lower = recs$deploy_date_time,
                             upper = recs$recover_date_time)]
    }
    
    # Calculate linear distance in meters of x and y limits to determine aspect ratio of the output
    linear_x = geosphere::distMeeus(c(background_xlim[1],background_ylim[1]),
                                    c(background_xlim[2],background_ylim[1]))
    linear_y = geosphere::distMeeus(c(background_xlim[1],background_ylim[1]),
                                    c(background_xlim[1],background_ylim[2]))
    
    figRatio <- linear_y/linear_x
    
    # plot GL outline and movement points
    png(file.path(out_dir, x$f_name[1]),
        width = 2000,
        height = 2000*figRatio,
        units = 'px',
        pointsize = 22*figRatio)

    # Plot background image
    # Set bottom margin to plot timeline outside of plot window
    par(oma=c(0,0,0,0), mar=c(6,0,0,0), xpd=FALSE)  

    if(is.null(background)){
      # Note call to plot with sp
      sp::plot(background,
               ylim = c(background_ylim),
               xlim = c(background_xlim),
               axes = FALSE,
               lwd = 2*figRatio,
               col = "white",
               bg = "gray74")
    }else{
      # Note call to plot with sp
      sp::plot(background,
               ylim = c(background_ylim),
               xlim = c(background_xlim),
               axes = FALSE,
               lwd = 2*figRatio)
    }
    
    box(lwd = 3*figRatio)
    
    if(!is.null(recs)){
      # plot receivers
      points(x = sub_recs$deploy_long,
             y = sub_recs$deploy_lat,
             pch = 16,
             cex = 1.5)
    }
    
    ### Add timeline
    par(xpd = TRUE)
    # Define timeline x and y location
    xlim_diff <- diff(background_xlim) 
    ylim_diff <- diff(background_ylim) 
    timeline_y <- rep(background_ylim[1] - (0.06*ylim_diff), 2)
    timeline_x <- c(background_xlim[1] + (0.10*xlim_diff),
                    background_xlim[2] - (0.10*xlim_diff))
    
    # Initalize timeline
    lines(timeline_x, timeline_y, col = "grey70", lwd = 20*figRatio, lend = 0)
    
    # Calculate the duration of the animation based on data extents
    time_dur <- (as.numeric(max(proc_obj$grp)) - as.numeric(min(proc_obj$grp)))
    
    # Add labels to timeline
    labels <- seq(as.POSIXct(format(min(proc_obj$grp), "%Y-%m-%d")),
                  as.POSIXct(format(max(proc_obj$grp), "%Y-%m-%d")),
                  length.out = 5)
    labels_ticks <- as.POSIXct(format(labels, "%Y-%m-%d"), tz = "GMT")
    ptime <- (as.numeric(labels_ticks) - as.numeric(min(proc_obj$grp))) / time_dur
    labels_x <- timeline_x[1] + (diff(timeline_x) * ptime)
    text(x = labels_x,
         y = timeline_y[1]-0.01*(ylim_diff),
         labels = format(labels, "%Y-%m-%d"),
         cex = 2,
         pos = 1)
    
    # Update timeline
    ptime <- (as.numeric(x[1,"grp"]) - as.numeric(min(proc_obj$grp))) / time_dur 
    # Proportion of timeline elapsed
    timeline_x_i <- timeline_x[1] + diff(timeline_x) * ptime
    # Plot slider along timeline at appropriate location
    points(timeline_x_i,
           timeline_y[1],
           pch = 21,
           cex = 2,
           bg = "grey40", 
           col = "grey20",
           lwd = 1)
    
    # Plot detection data
    if(!show_interpolated){
      points(x = x$longitude,
             y = x$latitude,
             pch = x$marker,
             col = ifelse(x$record_type == "inter", "transparent", x$color),
             cex = x$marker_cex)
    }else{
      points(x = x$longitude,
             y = x$latitude,
             pch = x$marker,
             col = x$color,
             cex = x$marker_cex)
    }
    dev.off()
  }
  
  ### Progress bar
  grpn <- uniqueN(proc_obj$grp)
  pb <- txtProgressBar(min = 0, max = grpn, style = 3)

  setkey(proc_obj, grp)

  # create images
  proc_obj[proc_obj$plot %in% c(NA,1),
           {setTxtProgressBar(pb, .GRP); cust_plot(x = .SD,
                                                   proc_obj,
                                                   sub_recs,
                                                   out_dir,
                                                   background,
                                                   background_xlim,
                                                   background_ylim,
                                                   show_interpolated)},
           by = grp,
          .SDcols = c("bin_timestamp", "longitude", "latitude", "record_type", "marker", "color",
                      "marker_cex", "f_name", "grp")]
close(pb)
  
  if(animate & frame_delete){
    make_video(dir = out_dir,
                    pattern = paste0(char, ".png"),
                    output = ani_name,
                    output_dir = out_dir,
               overwrite = overwrite,
               ffmpeg = ffmpeg)
    unlink(file.path(out_dir, unique(proc_obj$f_name)))
  }else if(animate & !frame_delete){
    make_video(dir = out_dir,
                    pattern = paste0(char, ".png"),
                    output = ani_name,
                    output_dir = out_dir,
               overwrite = overwrite,
               ffmpeg = ffmpeg)
  }else{
    if(!animate){
       stop
    }
  }
}

