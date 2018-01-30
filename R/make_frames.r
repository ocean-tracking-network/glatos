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
#' @param recs A data frame containing at least four columns with
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


animatePath <- function(proc_obj, recs, plot_control = NULL, out_dir = getwd(), background = NULL,
                        background_ylim = c(41.48, 45.9),
                        background_xlim = c(-84, -79.5),
                        ffmpeg = NA,
                        ani_name = "animation.mp4",
                        frame_delete = TRUE,
                        animate = TRUE,
                        overwrite = FALSE,
                        threshold = NULL){
  setDT(proc_obj)
  setDT(recs)

  # try calling ffmpeg if animate = TRUE.
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
                  'FFmpeg is available from:\n https://ffmpeg.org/'),
           call. = FALSE)

    mapmate <- any(installed.packages()[,1] == "mapmate")
    if(mapmate == FALSE) stop(
        paste0("mapmate package is not installed.\n",
               "see: https://github.com/leonswicz/mapmate\n",
               'install: devtools::install_github("leonawicz/mapmate")'),
        call. = FALSE)
  }

  # add plotting columns for dealing with fish that leave a site and aren't detected elsewhere (optional) - uses thershold value that is user defined with argument 'threshold'
  if(!is.null(threshold)){
    setkey(proc_obj, animal_id, bin_stamp)
    proc_obj <- proc_obj[, .(animal_id, bin_stamp, i_lat, i_lon, record_type,
                        diff_time = ifelse(length(bin_stamp == 1), NA,
                        c(NA,as.numeric(diff(bin_stamp)))),
                        diff_loc = ifelse(length(bin_stamp == 1), NA, c(NA,
                        ifelse(diff(i_lat) == 0 & diff(i_lon) == 0, 0, 1)))),
                        by = animal_id]
    
    proc_obj <-  proc_obj[,.(animal_id, bin_stamp, i_lat, i_lon, record_type,
                          diff_time, diff_loc,
                          plot = ifelse(diff_time > threshold &
                                          diff_loc == 0, 0, 1))]
  }
  
  # add colors and symbols to detections data frame
  if(!is.null(plot_control)){
    setDT(plot_control)
    proc_obj <- merge(proc_obj, plot_control, by.x=c("animal_id", "type"),
                     by.y=c("animal_id","type"))
    proc_obj <- proc_obj[!is.na(color)]
  } else {

    # otherwise, assign default colors and symbols
    proc_obj$color = 'black'
    proc_obj$marker = 21
    proc_obj$marker_cex = 1
  }

  #make output directory if it does not already exist
  if(!dir.exists(out_dir)) dir.create(out_dir)

  # create group identifier
  proc_obj[, grp := bin_stamp]

  # remove receivers not recovered (records with NA in recover_date_time)
  setkey(recs, recover_date_time)
  recs <- recs[!J(NA_real_), c("station", "deploy_lat", "deploy_long",
                               "deploy_date_time", "recover_date_time")]

  
  # extract time sequence for plotting
  t_seq <- unique(proc_obj$bin_stamp)
  
  # determine leading zeros needed by ffmpeg and add as new column
  char <- paste0("%", 0, nchar((length(t_seq))), "d")
  setkey(proc_obj, bin_stamp)
  proc_obj[, f_name := .GRP, by = grp]
  proc_obj[, f_name := paste0(sprintf(char, f_name), ".png")]

  if(is.null(background)) {
    data(greatLakesPoly) #example in glatos package
    background <- greatLakesPoly
  }
  
  cust_plot <- function(x){

    # extract receivers in the water during plot interval
    sub_recs <- recs[between(x$bin_stamp[1], lower = recs$deploy_date_time, upper = recs$recover_date_time)]
    
    # plot GL outline and movement points
    png(file.path(out_dir, x$f_name[1]), width = 3200, height = 2400,
        units = 'px', res = 300)

    # plot background image
    par(oma=c(0,0,0,0), mar=c(0,0,0,0))  #no margins

    # note call to plot with sp
    sp::plot(background, ylim = c(background_ylim), xlim = c(background_xlim),
             axes = FALSE, lwd = 2)

    # plot fish locations, receivers, clock
    points(x = sub_recs$deploy_long, y = sub_recs$deploy_lat, pch = 21, cex = 2,
           col = "tan2", bg = "tan2")
    text(x = -84.0, y = 42.5, as.Date(x$bin_stamp[1]), cex = 2.5)
    points(x = x$i_lon, y = x$i_lat, pch = x$marker, col = x$color,
           cex = x$marker_cex)
    dev.off()
  }

  grpn <- uniqueN(proc_obj$grp)
  pb <- txtProgressBar(min = 0, max = grpn, style = 3)

  setkey(proc_obj, grp)

  # create images
  proc_obj[proc_obj$plot %in% c(NA,1), {setTxtProgressBar(pb, .GRP); cust_plot(x = .SD)},  by = grp,
          .SDcols = c("bin_stamp", "i_lon", "i_lat", "marker", "color",
                      "marker_cex", "f_name")]
close(pb)
  
  if(animate & frame_delete){
mapmate::ffmpeg(dir = out_dir, pattern = paste0(char, ".png"),
                    output = ani_name, output_dir = out_dir, rate = "ntsc", overwrite = overwrite)
    unlink(file.path(out_dir, unique(proc_obj$f_name)))
  } else {
    if(animate & !frame_delete){
      mapmate::ffmpeg(dir = out_dir, pattern = paste0(char, ".png"),
                      output = ani_name, output_dir = out_dir, rate = "ntsc", overwrite = overwrite)
    } else {
      if(!animate){
        stop
      }
    }
  }
}
