#' Create an animated video of spatiotemporal path data
#' 
#' Create a set of frames (png image files) showing geographic location data
#' (e.g., detections of tagged fish or interpolated path data) at discrete 
#' points in time on top of a Great Lakes shapefil and optionally stitches
#' frames into a video animation (mp4 file).    
#'
#'
#' @param proc_obj A data frame created by
#'   \code{\link{interpolatePath}} function or a data frame containing
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
#' 	 for y-scale of plot. Units are same as background argument.
#' 	 
#' @param background_xlim Vector of two values specifying the min/max values 
#'   for x-scale of plot. Units are same as background argument.
#'   
#' @param animate Boolean. Default (TRUE) creates video animation
#' 
#' @param ani_name Name of animation (character string)
#' 
#' @param frame_delete Boolean.  Default (TRUE) delete individual
#'   image frames after animation is created
#'   
#' @param overwrite Overwite the animation file if it already exists. Default is
#'   FALSE (file is not overwritten)
#' 
#' @param ffmpeg A character string with path to ffmpeg executable file
#'   (Windows: "ffmpeg.exe", MacOS: "ffmpeg"). The ffmpeg executable is found in
#'   the ffmpeg folder you can download from https://ffmpeg.org/. This argument
#'   is only needed if ffmpeg has not been added to your path variable on your
#'   computer.
#' 
#' @return Sequentially-numbered png files (one for each frame) and 
#'   one mp4 file will be written to \code{out_dir}.
#' 
#' @author Todd Hayden
#'
#' @examples
#'
#' # load detection data
#' det_file <- system.file("extdata", "walleye_detections.zip", package = "glatos")
#' det_file <- unzip(det_file, "walleye_detections.csv")
#' dtc <- read_glatos_detections(det_file)
#' 
#' # shrink size to reduce computation time
#' dtc <- dtc[1:5000,]
#' 
#' # take a look
#' head(dtc)
#'
#' # load receiver location data
#' rec_file <- system.file("extdata", 
#'   "receiver_locations_2011.csv", package = "glatos")
#' recs <- read_glatos_receiver_locations(rec_file)
#' 
#' # call with defaults; linear interpolation
#'  pos1 <- interpolatePath(dtc)
#'
#' # make sequential frames and animation
#' # make sure ffmpeg is installed if argument \code{animate = TRUE}
#' # If you have not added path to 'ffmpeg.exe' to your Windows PATH 
#' # environment variable then you'll need to do that  
#' # or set path to 'ffmpeg.exe' using the 'ffmpeg' input argument
#' 
#' # make sequential frames but don't make animation  
#' myDir <- paste0(getwd(),"/frames1")
#' make_frames(pos1, recs=recs, out_dir=myDir, animate = FALSE)
#'
#' # make sequential frames, no animation, add threshold for non-detection
#' myDir <- paste0(getwd(),"/frames2")
#' make_frames(pos1, recs=recs, out_dir=myDir, animate = FALSE, threshold = (86400))
#'
#' # make sequential frames, and animate.  Keep both animation and frames
#' myDir <- paste0(getwd(), "/frames3")
#' make_frames(pos1, recs=recs, out_dir=myDir, animate = TRUE, threshold = (86400*10))
#'
#' # make animation, remove frames.
#' myDir <- paste0(getwd(), "/frames4")
#' make_frames(pos1, recs=recs, out_dir=myDir, animate=TRUE)
#'
#' \dontrun{
#' # if ffmpeg is not on system path
#'
#' # windows
#' myDir <- paste0(getwd(), "/frames5")
#' make_frames(pos1, recs=recs, out_dir=my_dir, animate=TRUE,
#' ffmpeg="C://path//to//ffmpeg//bin//ffmpeg.exe")
#'
#' # mac
#' myDir <- paste0(getwd(), "/frames6")
#' make_frames(pos1, recs=recs, outDir=myDir, animate=TRUE, ffmpeg="/path/to/ffmpeg")}
#'
#'
#'
#' @export
#' 
#######################3
# development
## data(walleye_detections)
## dtc <- walleye_detections
## # example receiver location data
## data(recLoc_example) 
 
## # call with defaults; linear interpolation
## pos1 <- out

## # shrink size to reduce computation time
## dtc <- dtc[1:5000, ]

## proc_obj = pos1
## recs = recLoc_example
## tail_dur = 5
## animate = TRUE
## ani_name = "animation.mp4"
## frame_delete = FALSE
## overwrite = FALSE
## ffmpeg = NA
## out_dir = "."

make_frames <- function(proc_obj,
                         recs = NULL,
                         plot_control = NULL,
                        out_dir = getwd(),
                        background_ylim = c(41.3, 49.0),
                        background_xlim = c(-92.45, -75.87),
                        show_interpolated = TRUE,
                        tail_dur = 0,
                        animate = TRUE,
                        ani_name = "animation.mp4",
                        frame_delete = FALSE,
                        overwrite = FALSE,
                        ffmpeg = NA){
  
  ## # Try calling ffmpeg if animate = TRUE.
  ## # If animate = FALSE, video file is not produced- no need to check for package.

## if(animate == TRUE){
  ##   cmd <- ifelse(is.na(ffmpeg), 'ffmpeg', ffmpeg)
  ##   ffVers <- suppressWarnings(system2(cmd, "-version", stdout=F)) #call ffmpeg
  ##   if(ffVers == 127)
  ##     stop(paste0('"ffmpeg.exe" was not found.\n',
  ##                 'Ensure it is installed add added to system PATH variable\n',
  ##                 "or specify path using input argument 'ffmpeg'\n\n",
  ##                 'FFmpeg is available from:\n https://ffmpeg.org/\n',
  ##                 'You may create the individual frames and then combine them\n',
  ##                 'into an animation manually using video editing software\n', 
  ##                 '(e.g., Windows Movie Maker or iMovie) by setting the animate\n',
  ##                 'argument to FALSE.'),
  ##          call. = FALSE)
  ##   }

##   # Convert proc_obj and recs dataframes into data.table objects
##   setDT(proc_obj)
##   if(!is.null(recs)){
##     setDT(recs)
##     # Remove receivers not recovered (records with NA in recover_date_time)
##     setkey(recs, recover_date_time)
##     recs <- recs[!J(NA_real_), c("station", "deploy_lat", "deploy_long",
##                                  "deploy_date_time", "recover_date_time")]
##   }

##   # Make output directory if it does not already exist
##   if(!dir.exists(out_dir)) dir.create(out_dir)

##   # Create group identifier for plotting
##   # groups are defined by time bins
##   proc_obj[, grp := bin_timestamp]

##   # extract time sequence for plotting
##   t_seq <- unique(proc_obj$bin_timestamp)
  
##   # determine leading zeros needed by ffmpeg and add as new column
##   char <- paste0("%", 0, nchar((length(t_seq))), "d")
##   setkey(proc_obj, bin_timestamp)
##   proc_obj[, f_name := .GRP, by = grp]
##   proc_obj[, f_name := paste0(sprintf(char, f_name), ".png")]

 
##   # Load Great lakes background
##   data(greatLakesPoly) #example in glatos package
##   background <- greatLakesPoly

##   # make tail groups
##   tail_dur <- proc_obj[, .(t_seq = sort(unique(bin_timestamp)))]
##   tail_dur[, c("t_end", "t_grp") := list(data.table::shift(t_seq, type = "lag", fill = min(t_seq), n=5), 1:nrow(tail_dur))]


## ## # ref = tail_dur
## ## ref <- data.table(space=rep('nI',3),t1=c(100,300,500),t2=c(150,400,600),id=letters[1:3])
## ## # map = proc_obj
## ## map <- data.table(space=rep('nI',241),t1=seq(0,1200,by=5),t2=seq(5,1205,by=5),res=rnorm(241))
## ## setkey(ref, space, t1, t2)
## ## foverlaps(map, ref, type="within", nomatch = 0L)

## #####

## proc_obj[, t_end := bin_timestamp]
## setkey(tail_dur, t_end, t_seq)
## check <- foverlaps(proc_obj, tail_dur, type = "within", nomatch = 0L, by.x = c("bin_timestamp", "t_end"))

## setkey(check, t_grp, animal_id, bin_timestamp)
## check[t_grp == 1]















##   cust_plot <- function(x,
##                         proc_obj,
##                         sub_recs,
##                         out_dir,
##                         background,
##                         background_xlim,
##                         background_ylim,
##                         show_interpolated){

##     if(!is.null(recs)){
##     # extract receivers in the water during plot interval
##     sub_recs <- recs[between(x$bin_timestamp[1],
##                              lower = recs$deploy_date_time,
##                              upper = recs$recover_date_time)]
##     }
    
##     # Calculate great circle distance in meters of x and y limits.
##     # needed to determine aspect ratio of the output
##     linear_x = geosphere::distMeeus(c(background_xlim[1],background_ylim[1]),
##                                     c(background_xlim[2],background_ylim[1]))
##     linear_y = geosphere::distMeeus(c(background_xlim[1],background_ylim[1]),
##                                     c(background_xlim[1],background_ylim[2]))
    
##     figRatio <- linear_y/linear_x

##     # calculate image height
##     height <- trunc(2000*figRatio)
    
##     # plot GL outline and movement points
##     png(file.path(out_dir, x$f_name[1]),
##         width = 2000,
##         height = ifelse(height%%2==0,
##                         height, height+1),
##         units = 'px',
##         pointsize = 22*figRatio)

##     # Plot background image
##     # Set bottom margin to plot timeline outside of plot window
##     par(oma=c(0,0,0,0), mar=c(6,0,0,0), xpd=FALSE)  

##     # Note call to plot with sp
##     sp::plot(background,
##              ylim = c(background_ylim),
##              xlim = c(background_xlim),
##              axes = FALSE,
##              lwd = 2*figRatio,
##              col = "white",
##              bg = "gray74")
    
##     box(lwd = 3*figRatio)
    
##     if(!is.null(recs)){
##       # plot receivers
##       points(x = sub_recs$deploy_long,
##              y = sub_recs$deploy_lat,
##              pch = 16,
##              cex = 1.5)
##     }
    
##     ### Add timeline
##     par(xpd = TRUE)
##     # Define timeline x and y location
##     xlim_diff <- diff(background_xlim) 
##     ylim_diff <- diff(background_ylim) 
##     timeline_y <- rep(background_ylim[1] - (0.06*ylim_diff), 2)
##     timeline_x <- c(background_xlim[1] + (0.10*xlim_diff),
##                     background_xlim[2] - (0.10*xlim_diff))
    
##     # Initalize timeline
##     lines(timeline_x, timeline_y, col = "grey70", lwd = 20*figRatio, lend = 0)
    
##     # Calculate the duration of the animation based on data extents
##     time_dur <- (as.numeric(max(proc_obj$grp)) - as.numeric(min(proc_obj$grp)))
    
##     # Add labels to timeline
##     labels <- seq(as.POSIXct(format(min(proc_obj$grp), "%Y-%m-%d")),
##                   as.POSIXct(format(max(proc_obj$grp), "%Y-%m-%d")),
##                   length.out = 5)
##     labels_ticks <- as.POSIXct(format(labels, "%Y-%m-%d"), tz = "GMT")
##     ptime <- (as.numeric(labels_ticks) - as.numeric(min(proc_obj$grp))) / time_dur
##     labels_x <- timeline_x[1] + (diff(timeline_x) * ptime)
##     text(x = labels_x,
##          y = timeline_y[1]-0.01*(ylim_diff),
##          labels = format(labels, "%Y-%m-%d"),
##          cex = 2,
##          pos = 1)
    
##     # Update timeline
##     ptime <- (as.numeric(x[1,"grp"]) - as.numeric(min(proc_obj$grp))) / time_dur 

##     # Proportion of timeline elapsed
##     timeline_x_i <- timeline_x[1] + diff(timeline_x) * ptime

##     # Plot slider along timeline at appropriate location
##     points(timeline_x_i,
##            timeline_y[1],
##            pch = 21,
##            cex = 2,
##            bg = "grey40",
##            col = "grey20",
##            lwd = 1)
    
##     # Plot detection data
##     if(!show_interpolated){
##       points(x = x$longitude, y = x$latitude, pch = 16,
##              col = ifelse(x$record_type == "inter", "transparent", "blue"),
##              cex = 2) }else{
##                points(x = x$longitude, y = x$latitude, pch = 16, col = "blue",
##                       cex = 2)
##              }
##     dev.off()
##   }
  
##   ### Progress bar
##   grpn <- uniqueN(proc_obj$grp)
##   pb <- txtProgressBar(min = 0, max = grpn, style = 3)

##   setkey(proc_obj, grp)

##   # create images
##   proc_obj[proc_obj$p_thresh %in% c(NA,1),
##            {setTxtProgressBar(pb, .GRP); cust_plot(x = .SD,
##                                                    proc_obj,
##                                                    sub_recs,
##                                                    out_dir,
##                                                    background,
##                                                    background_xlim,
##                                                    background_ylim,
##                                                    show_interpolated)},
##            by = grp,
##            .SDcols = c("bin_timestamp", "longitude", "latitude", "record_type",
##                        "marker", "color", "marker_cex", "f_name", "grp")]

##   close(pb)

##   if(animate == FALSE & frame_delete == TRUE){
##     message("are you sure?")}

##   if(animate == FALSE){message(paste("frames are in\n", out_dir))}
  
##   if(animate & frame_delete){
##     make_video(dir = out_dir,
##                pattern = paste0(char, ".png"),
##                output = ani_name,
##                output_dir = out_dir,
##                overwrite = overwrite,
##                ffmpeg = ffmpeg)
##       unlink(file.path(out_dir, unique(proc_obj$f_name)))
##     message(paste("video is in\n", out_dir))}

##   if(animate & !frame_delete){
##         make_video(dir = out_dir,
##                    pattern = paste0(char, ".png"),
##                    output = ani_name,
##                    output_dir = out_dir,
##                    overwrite = overwrite,
##                    ffmpeg = ffmpeg)
##         message(paste("video and frames in \n", out_dir))}
   }
