#' Create an animated video of spatiotemporal path data
#' 
#' Create a set of frames (png image files) showing geographic location data
#' (e.g., detections of tagged fish or interpolated path data) at discrete 
#' points in time on top of a Great Lakes shapefile and optionally stitches
#' frames into a video animation (mp4 file).    
#'
#'
#' @param proc_obj A data frame created by
#'   \code{\link{interpolate_path}} function or a data frame containing
#'   'animal_id', 'bin_timestamp', 'latitude', 'longitude', and
#'   'record_type'
#'   
#' @param recs An optional data frame containing at least four columns with
#'   receiver 'deploy_lat', 'deploy_long', 'deploy_date_time', and
#'   'recover_date_time'. Other columns in object will be ignored.
#'   Default column names match GLATOS standard receiver location file
#'   \cr(e.g., 'GLATOS_receiverLocations_yyyymmdd.csv').
#'
#' @param out_dir A character string with file path to directory where 
#'   individual frames for animations will be written. Default is working
#'   directory.
#'   
#' @param background_ylim Vector of two values specifying the min/max values 
#' 	 for y-scale of plot. Units are degrees.
#' 	 
#' @param background_xlim Vector of two values specifying the min/max values 
#'   for x-scale of plot. Units are degrees.
#'   
#' @param show_interpolated Boolean. Default (TRUE) include interpolated points.
#'   
#' @param animate Boolean. Default (TRUE) creates video animation
#' 
#' @param ani_name Name of animation (character string)
#' 
#' @param frame_delete Boolean.  Default (\code{frame_delete = TRUE}) delete individual
#'   image frames after animation is created
#'   
#' @param overwrite Overwite the animation file if it already
#'   exists. Default (\code{overwrite = FALSE}) prevents file from being overwritten.
#'
#' @param ffmpeg A file path (characer) to FFmpeg executable. This
##'   argument is only needed if ffmpeg is not added to your system
##'   path. For Windows machines, path must point to 'ffmpeg.exe',
##'   located in the bin subfolder within the ffmpeg folder.  For
##'   example on Windows machines,
##'   "C:\\Users\\Username\\Documents\\ffmpeg-3.4.1-win64-static\\bin\\ffmpeg.exe").
##'   On Mac, path must point to 'ffmpeg' within the 'bin'
##'   subfolder, i.e., "/home/directory/Documents/bin/ffmpeg".  
##'
#' @param tail_dur contains the duration (in same units as \code{proc_obj$bin_timestamp}; 
#'     see \code{\link{interpolate_path}}) of trailing points in each frame. 
#'     Default value is 0 (no trailing points). A value
#'     of \code{Inf} will show all points from start.
#'
#' @param preview write first frame only.  Useful for
#'   checking output before processing large number of frames.  Default \code{preview = FALSE}
#'
#' @param ... Optional graphing parameters that determine the size, style, and 
#' positions of (1) fish location points, (2) plot device, 
#' (3) receiver location points, (4) timeline and (5) slider (moves along 
#' the timeline). 
#' 
#' Most aspects of these objects can be modified using standard plot functions, 
#' but a prefix (e.g., 'rcv.cex' vs. 'timeline.cex' vs. 'timeslider.cex')
#' is used for the last three object types. 
#' 
#' \describe{
#' \item{\code{interpolated fish positions}}{
#' Any argument that can be passed to \code{plot::points}, such as \code{cex =
#'   2}, \code{pch = 21}. No prefix is used. Defaults are pch = 16, 
#'   col = "blue", cex = 2.}
#' \item{\code{receiver locations}}{Add prefix "rcv." to arguments of 
#' \link[graphics]{points}. 
#' Defaults are rcv.pch = 16, rcv.cex = 1.5.}
#' \item{\code{plot device}}{Add prefix "par." 
#' to plot argument. Defaults are par.oma=c(0,0,0,0), par.mar=c(6,0,0,0), 
#' par.xpd=FALSE.}
#' \item{\code{timeline}}{Add prefix "timeline." to arguments of 
#' \link[graphics]{axis}. Defaults are timeline.side=1, at = timeline.labels_x, 
#' pos = timeline_y[1], 
#' timeline.labels = format(labels, "%Y-%m-%d"), timeline.col = "grey70", 
#' timeline.lwd = 20*figRatio, timeline.lend = 0, timeline.lwd.ticks = NA, 
#' timeline.col.ticks = 1,timeline. cex.axis = 2, timeline.padj = 0.5)}
#' \item{\code{timeslider}}{Add prefix "timeslider." to arguments of 
#' \link[graphics]{points}. 
#' Defaults are timeslider.pch = 21, timeslider.cex = 2, 
#' timeslider.bg = "grey40", timeslider.col = "grey20", timeslider.lwd = 1.}
#' }
#' 
#' @return Sequentially-numbered png files (one for each frame) and 
#'   one mp4 file will be written to \code{out_dir}.
#' 
#' @author Todd Hayden, Tom Binder, Chris Holbrook
#'
#' @examples
#'
#' # load detection data
#' det_file <- system.file("extdata", "walleye_detections.csv",
#'                          package = "glatos")
#' dtc <- read_glatos_detections(det_file)
#' 
#' # take a look
#' head(dtc)
#'  
#' # load receiver location data
#' rec_file <- system.file("extdata", 
#'   "sample_receivers.csv", package = "glatos")
#' recs <- read_glatos_receivers(rec_file)
#' 
#' # call with defaults; linear interpolation
#'  pos1 <- interpolate_path(dtc)
#'
#' # make sequential frames and animation
#' # make sure ffmpeg is installed if argument \code{animate = TRUE}
#' # If you have not added path to 'ffmpeg.exe' to your Windows PATH 
#' # environment variable then you'll need to do that  
#' # or set path to 'ffmpeg.exe' using the 'ffmpeg' input argument
#' 
#' # make frames, preview the first frame
#' myDir <- paste0(getwd(),"/frames1")
#' make_frames(pos1, recs=recs, out_dir=myDir, preview = TRUE)
#'
#' # make frames but not animation 
#' myDir <- paste0(getwd(),"/frames2")
#' make_frames(pos1, recs=recs, out_dir=myDir, animate = FALSE)
#' 
#' # make sequential frames, and animate.  Make animation and frames.
#' #change default color of fish markers to red and change marker and size.
#' myDir <- paste0(getwd(), "/frames3")
#' make_frames(pos1, recs=recs, out_dir=myDir, animate = TRUE, col="red", pch = 16, cex = 3)
#'
#' # make sequential frames, animate, add 5-day tail
#' myDir <- paste0(getwd(), "/frames4")
#' make_frames(pos1, recs=recs, out_dir=myDir, animate = TRUE, tail_dur=5)
#' 
#' # make animation, remove frames.
#' myDir <- paste0(getwd(), "/frames5")
#' make_frames(pos1, recs=recs, out_dir=myDir, animate=TRUE, frame_delete = TRUE)
#'
#' \dontrun{
#' # if ffmpeg is not on system path
#'
#' # windows
#' myDir <- paste0(getwd(), "/frames6")
#' make_frames(pos1, recs=recs, out_dir=my_dir, animate=TRUE,
#' ffmpeg="C://path//to//ffmpeg//bin//ffmpeg.exe")
#'
#' # mac
#' myDir <- paste0(getwd(), "/frames7")
#' make_frames(pos1, recs=recs, outDir=myDir, animate=TRUE,
#' ffmpeg="/path/to/ffmpeg")
#' }
#'
#' @export

make_frames <- function(proc_obj, recs = NULL, out_dir = getwd(),
                        background_ylim = c(41.3, 49.0),
                        background_xlim = c(-92.45, -75.87),
                        show_interpolated = TRUE, tail_dur = 0, animate = TRUE,
                        ani_name = "animation.mp4", frame_delete = FALSE,
                        overwrite = FALSE, ffmpeg = NA, preview = FALSE, ...){
  
  # Try calling ffmpeg if animate = TRUE.
  # If animate = FALSE, video file is not produced- no need to check for package.
  if(animate == TRUE){
    cmd <- ifelse(is.na(ffmpeg), 'ffmpeg', ffmpeg)
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

  # Convert proc_obj and recs dataframes into data.table objects
  work_proc_obj <- data.table::as.data.table(proc_obj)

  #make column to identify original row to join with option plot arguments
  work_proc_obj[ , row_in := 1:.N]  
    
  # set recs to data.table  
  if(!is.null(recs)){
    recs <- data.table::as.data.table(recs)

    #make column to identify original row to join with option plot arguments
    recs[ , row_in := 1:.N]    
    
    # Remove receivers not recovered (records with NA in recover_date_time)
    data.table::setkey(recs, recover_date_time)
    recs <- recs[!J(NA_real_), c("station", "deploy_lat", "deploy_long", 
                                 "deploy_date_time", "recover_date_time",
                                 "row_in")]
  }
  
  # capture optional plot arguments passed via ellipses
  #  and add original row indices to join on both
  inargs <- list(...)
  #set defaults and apply if needed
  rcv_args <- list(pch = 16, cex = 1.5)
  dtc_args <- list(pch = 16, col = "blue", cex = 2)
  #if(length(inargs) > 0){
    #identify and subset par arguments
    par_inargs <- inargs[grepl("^par\\.", names(inargs))] #temporary
    #identify and subset receiver point arguments
    rcv_inargs <- inargs[grepl("^rec\\.", names(inargs))] #temporary
    #identify and subset timeline arguments
    timeline_inargs <- inargs[grepl("^timeline\\.", names(inargs))]
    #identify and subset timeslider arguments
    timeslider_inargs <- inargs[grepl("^timeslider\\.", names(inargs))]
    #identify and subset detection point arguments
    dtc_inargs <- inargs[setdiff(names(inargs), c(names(par_inargs),
                                                  names(rcv_inargs),
                                                  names(timeline_inargs),
                                                  names(timeslider_inargs)))]
    #strip argument names
    names(par_inargs) <- gsub("^par\\.", "", names(par_inargs))
    names(timeline_inargs) <- gsub("^timeline\\.", "", names(timeline_inargs))
    names(timeslider_inargs) <- gsub("^timeslider\\.", "", names(timeslider_inargs))
    names(rcv_inargs) <- gsub("^rec\\.", "", names(rcv_inargs))
    names(dtc_inargs) <- gsub("^dtc\\.", "", names(dtc_inargs))
    
    #update from ...
    if(length(rcv_inargs) > 0) rcv_args[names(rcv_inargs)] <- rcv_inargs
    #update from ...
    if(length(dtc_inargs) > 0) dtc_args[names(dtc_inargs)] <- dtc_inargs
        
    #expand single rcv_args elements to equal number of rows in recs
    for(i in 1:length(rcv_args)) {
      if(length(rcv_args[[i]] == 1)) {
        rcv_args[[i]] <- rep(rcv_args[[i]], nrow(recs))
      } else if (length(rcv_args[[i]]) != nrow(recs)) {
           stop(paste0("Length of optional plot parameters pass via '...' ",
             "must be 1 or equal to\n number of rows in input data."))
        }
    }

    #expand single dtc_args elements to equal number of rows in work_proc_obj
    for(i in 1:length(dtc_args)) {
      if(length(dtc_args[[i]] == 1)) {
        dtc_args[[i]] <- rep(dtc_args[[i]], nrow(work_proc_obj))
      } else if (length(dtc_args[[i]]) != nrow(work_proc_obj)) {
        stop(paste0("Length of optional plot parameters pass via '...' ",
          "must be 1 or equal to\n number of rows in input data."))
      }
    }    
    
    #coerce to data.table and add original row index (for join to recs)
    rcv_args <- as.data.table(rcv_args)
    rcv_args[ , row_in := 1:.N]
    dtc_args <- as.data.table(dtc_args)
    dtc_args[ , row_in := 1:.N]
  #} 

  # Make output directory if it does not already exist
  if(!dir.exists(out_dir)) dir.create(out_dir)

  # extract time sequence for plotting
  t_seq <- unique(work_proc_obj$bin_timestamp)

 
  # make tails if needed
  if(tail_dur == 0){

    #  Create group identifier for plotting
    work_proc_obj[, grp := bin_timestamp]
  } else {

    # make tail groups if needed
    dur <- work_proc_obj[, .(t_seq = sort(unique(bin_timestamp)))]
    dur[, c("t_end", "t_grp") :=
            list(data.table::shift(t_seq, type = "lag",
                                   fill = min(t_seq), n = tail_dur),
                 1:nrow(dur))]

    # group obs for tails
    work_proc_obj[, t_end := bin_timestamp]
    data.table::setkey(dur, t_end, t_seq)

    # merge by overlap
    work_proc_obj <- data.table::foverlaps(work_proc_obj, dur, type = "within",
                          nomatch = 0L, by.x = c("bin_timestamp", "t_end"))
    work_proc_obj <- work_proc_obj[, c("animal_id", "t_seq", "latitude",
                                       "longitude", "record_type", "row_in")]
    data.table::setnames(work_proc_obj, c("animal_id", "bin_timestamp", "latitude",
                              "longitude", "record_type", "row_in"))
    work_proc_obj[, grp := bin_timestamp]
  }

  # set rows in time order
  data.table::setorder(work_proc_obj, bin_timestamp)
  
  # create num group for later
  work_proc_obj[, grp_num := .GRP, by = bin_timestamp]
  
  # determine leading zeros needed by ffmpeg and add as new column
  char <- paste0("%", 0, nchar((length(t_seq))), "d")
  data.table::setkey(work_proc_obj, bin_timestamp)
  work_proc_obj[, f_name := .GRP, by = grp]
  work_proc_obj[, f_name := paste0(sprintf(char, f_name), ".png")]

  # order data for plotting
  data.table::setkey(work_proc_obj, bin_timestamp, animal_id,  record_type)

  # Load Great lakes background
  background <- greatLakesPoly #example in glatos package

  # turn off interpolated points if show_interpolated = FALSE
  if(!show_interpolated){
    work_proc_obj[record_type == "interpolated", latitude := NA]
    work_proc_obj[record_type == "interpolated", longitude := NA]
  }
  
  # Calculate the duration of the animation for timeline
  time_period <- range(work_proc_obj$bin_timestamp) 
  
  # define custom plot function
  cust_plot <- function(x, time_period, recs, out_dir, background,
                        background_xlim, background_ylim){

    # Calculate great circle distance in meters of x and y limits.
    # needed to determine aspect ratio of the output
    linear_x = geosphere::distMeeus(c(background_xlim[1],background_ylim[1]),
                                    c(background_xlim[2],background_ylim[1]))
    linear_y = geosphere::distMeeus(c(background_xlim[1],background_ylim[1]),
                                    c(background_xlim[1],background_ylim[2]))

    # aspect ratio of image
    figRatio <- linear_y/linear_x

    # calculate image height based on aspect ratio
    height <- trunc(2000*figRatio)

    # plot GL outline and movement points
    png(file.path(out_dir, x$f_name[1]), width = 2000,
        height = ifelse(height%%2==0, height, height + 1), units = 'px',
        pointsize = 22*figRatio)

    # Plot background image
    # Set bottom margin to plot timeline outside of plot window
    
    #set defaults and apply if needed
    par_args <- list(oma=c(0,0,0,0), mar=c(6,0,0,0), xpd=FALSE)
    #update from defaults...
    if(length(par_inargs) > 0) par_args[names(par_inargs)] <- par_inargs
    
    do.call(par, par_args)

    # Note call to plot with sp
    sp::plot(background, ylim = c(background_ylim), xlim = c(background_xlim),
             axes = FALSE, lwd = 2*figRatio, col = "white", bg = "gray74")

    box(lwd = 3*figRatio)
    
    # Add receiver locations
    if(!is.null(recs)){
      # extract receivers in the water during plot interval
      sub_recs <- recs[data.table::between(x$bin_timestamp[1], 
                       lower = recs$deploy_date_time,
                       upper = recs$recover_date_time)]
      
      # get optional plot arguments that correspond with sub_recs
      sub_rcv_args <- rcv_args[match(sub_recs$row_in,rcv_args$row_in), ] 
      
      # plot receivers; not do.call to include optional input arguments
      do.call(points, c(list(x = sub_recs$deploy_long, y = sub_recs$deploy_lat), 
                        sub_rcv_args[ , !"row_in", with = FALSE]))
    }

    # Add timeline
    par(xpd = TRUE)

    # Define timeline x and y location
    xlim_diff <- diff(background_xlim)
    ylim_diff <- diff(background_ylim)
    timeline_y <- rep(background_ylim[1] - (0.06*ylim_diff), 2)
    timeline_x <- c(background_xlim[1] + (0.10*xlim_diff),
                    background_xlim[2] - (0.10*xlim_diff))

    time_dur <- diff(as.numeric(time_period))
    
    # Add labels to timeline
    labels <- seq(as.POSIXct(format(min(time_period), "%Y-%m-%d")),
                  as.POSIXct(format(max(time_period), "%Y-%m-%d")),
                  length.out = 5)
    labels_ticks <- as.POSIXct(format(labels, "%Y-%m-%d"), tz = "GMT")
    ptime <- (as.numeric(labels_ticks) - as.numeric(min(time_period))) / time_dur
    labels_x <- timeline_x[1] + (diff(timeline_x) * ptime)
    
    #set defaults and apply if needed
    timeline_args = list(side=1, at = labels_x, pos = timeline_y[1], 
                         labels = format(labels, "%Y-%m-%d"), col = "grey70", 
                         lwd = 20*figRatio, lend = 0, lwd.ticks = NA, 
                         col.ticks = 1, cex.axis = 2, padj = 0.5)
 
    #update from ...
    if(length(timeline_inargs) > 0) timeline_args[names(timeline_inargs)] <- 
                                      timeline_inargs    
       
    do.call(axis, timeline_args)
    
    
    # Update timeline
    ptime <- (as.numeric(x[1,"grp"]) - as.numeric(min(time_period))) / time_dur

    # Proportion of timeline elapsed
    timeline_x_i <- timeline_x[1] + diff(timeline_x) * ptime

    #set defaults and apply if needed
    timeslider_args = list(pch = 21, 
                          cex = 2, bg = "grey40", col = "grey20", lwd = 1)
    
    #update from ...
    if(length(timeslider_inargs) > 0) timeslider_args[names(timeslider_inargs)] <- 
                                        timeslider_inargs    
    
    # Plot slider along timeline at appropriate location
    do.call(points, c(list(x=timeline_x_i, y=timeline_args$pos), 
                      timeslider_args))
   
    
    # Add fish positions
    # get optional plot arguments that correspond with x
    sub_dtc_args <- dtc_args[match(x$row_in, dtc_args$row_in), ] 
    
    do.call(points, c(list(x = x$longitude, y = x$latitude), 
                      dtc_args[ , !"row_in", with = FALSE]))

    dev.off()
  }

  # order for plotting
  data.table::setkey(work_proc_obj, grp_num)


  if(preview){ grpn <- 1 } else {
    # start progress bar
    grpn <- data.table::uniqueN(work_proc_obj$grp)
    pb <- txtProgressBar(min = 0, max = grpn, style = 3)
  }
  
  # call cust_plot witin data.table
  work_proc_obj[grp_num <= grpn, 
               {if(!preview) setTxtProgressBar(pb, .GRP)
                cust_plot(x = .SD, 
                          time_period = time_period, 
                          recs = recs, 
                          out_dir = out_dir,
                          background = background, 
                          background_xlim = background_xlim, 
                          background_ylim = background_ylim 
                )}
                , by = grp,
                .SDcols = c("bin_timestamp", "longitude", "latitude",
                  "record_type", "f_name", "grp", "row_in")]
  
  if(preview) { return(paste("preview frames are in \n", out_dir)) } else {
     close(pb) }
  
  if(animate == FALSE & frame_delete == TRUE) message("are you sure?")
  if(animate == FALSE) message(paste("frames are in\n", out_dir))

  if(animate & frame_delete){
    make_video(dir = out_dir, pattern = paste0(char, ".png"), output = ani_name,
               output_dir = out_dir, overwrite = overwrite, ffmpeg = ffmpeg)
    unlink(file.path(out_dir, unique(work_proc_obj$f_name)))
    message(paste("video is in\n", out_dir))}

  if(animate & !frame_delete){
    make_video(dir = out_dir, pattern = paste0(char, ".png"), output = ani_name,
               output_dir = out_dir, overwrite = overwrite, ffmpeg = ffmpeg)
    message(paste("video and frames in \n", out_dir))}
}

