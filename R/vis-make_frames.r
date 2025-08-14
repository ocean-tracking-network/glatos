#' Create an animated video of spatiotemporal path data
#'
#' Create a set of frames (png image files) showing geographic location data
#' (e.g., detections of tagged fish or interpolated path data) at discrete points
#' in time on top of a Great Lakes shapefile and optionally stitches frames into
#' a video animation (mp4 file).
#'
#'
#' @param proc_obj A data frame created by [interpolate_path()] function or a
#'  data frame containing 'animal_id', 'bin_timestamp', 'latitude', 'longitude',
#'  and 'record_type'
#'
#' @param recs An optional data frame containing at least four columns with
#'  receiver 'deploy_lat', 'deploy_long', 'deploy_date_time', and
#'  'recover_date_time'. Other columns in object will be ignored. Default column
#'  names match GLATOS standard receiver location file (e.g.,
#'  'GLATOS_receiverLocations_yyyymmdd.csv').
#'
#' @param out_dir A character string with file path to directory where individual
#'  frames for animations will be written. Default is working directory.
#'
#' @param background_ylim Vector of two values specifying the min/max values for
#'  y-scale of plot. Units are degrees.
#'
#' @param background_xlim Vector of two values specifying the min/max values for
#'  x-scale of plot. Units are degrees.
#'
#' @param show_interpolated Boolean. Default (TRUE) include interpolated points.
#'
#' @param animate Boolean. Default (TRUE) creates video animation by calling
#'  [make_video()] with `output = ani_name`. Default values are used for all
#'  other arguments. See Details below.
#'
#' @param ani_name Character string with name and extension of animation output
#'  video file. Full path is optional. If file name only (no path), then the
#'  output video is written to 'out_dir' (same as images). To write to working
#'  directory, use "./" prefix (e.g., `ani_name = "./animation.mp4"`. If
#'  `animate = TRUE`, the path and filename are passed to [make_video()].
#'
#' @param frame_delete Boolean.  Default (`frame_delete = TRUE`) delete
#'  individual image frames after animation is created
#'
#' @param overwrite Overwite the animation (output video) file if it already
#'  exists. Default (`overwrite = FALSE`) prevents file from being overwritten
#'  and will result in error if the file exists. Passed to [make_video()] if
#'  `animate = TRUE`.
#'
#' @param tail_dur contains the duration (in same units as
#'  `proc_obj$bin_timestamp`; see [interpolate_path()]) of trailing points in
#'  each frame. Default value is 0 (no trailing points). A value of `Inf` will
#'  show all points from start.
#'
#' @param preview write first frame only.  Useful for checking output before
#'  processing large number of frames.  Default `preview = FALSE`
#'
#' @param bg_map A sf points, lines, or polygons object.  Spatial `sp` or `terra` objects
#'  will be converted to `sf`.  Coordinate system of map must be latitude/longitude (WGS 84).
#'
#' @param show_progress Logical. Progress bar and status messages will be shown
#'  if TRUE (default) and not shown if FALSE.
#'
#' @param ... Optional graphing parameters for customizing elements of fish
#'  location points, receiver location points, timeline, and slider (moves along
#'  the timeline). See also **Details** and **Note** sections.
#'
#' @details
#' ***To customize fish location points (from `proc_obj`):*** Add any argument
#' that can be passed to [points][graphics::points]. The following values will
#' create the default plot:
#' \itemize{
#'    \item `cex`: symbol size; default = 2
#'    \item `col`: symbol color; default = "blue"
#'    \item `pch`: symbol type; default = 16
#' }
#'
#' @details
#'
#' ***To customize receiver location points (from `recs`):*** Add prefix
#' `recs.` to any argument that can be passed to [points][graphics::points]. The
#' following values will create the default plot:
#' \itemize{
#'    \item `recs.cex`: symbol size; default = 1.5
#'    \item `recs.pch`: symbol type; default = 16
#' }
#'
#' ***To customize timeline:*** Add add prefix `timeline.` to any
#' argument of [axis][graphics::axis].  Note all elements of the timeline except
#' the sliding symbol (see 'slider' below) are created by a call to `axis`. The
#' following values will create the default plot:
#' \itemize{
#'    \item `timeline.at`: a sequence with locations of labels (with first
#' and last being start and end) along x-axis; in units of longitude; by default
#' this will center the timeline with five equally-spaced labels in the middle
#' 80% of background_xlim.
#'    \item `timeline.pos`: location along the y-axis; in units of latitude;
#' by default this will place the timeline up from the bottom 6% of the range
#' of `background_ylim`
#'    \item `timeline.labels`: text used for labels; default =
#' `format(labels, "\%Y-\%m-\%d")`, where labels are values of proc_obj$bin_timestamp
#'    \item `timeline.col`: color of line; default = "grey70"
#'    \item `timeline.lwd`: width of line; default = 20 times the aspect
#' ratio of the plot device
#'    \item `timeline.cex.axis`: size of labels; default = 2
#' }
#'
#' ***To customize time slider (symbol that slides):*** Add prefix
#' `timeline.` to any argument that can be passed to [points][graphics::points].
#' The following values will create the default plot:
#' \itemize{
#'    \item `timeslider.bg`: a single value with symbol bg color; default =
#' "grey40"
#'    \item `timeslider.cex`: a single value with symbol size; default = 2
#'    \item `timeslider.col`: a single value with symbol type; default =
#' "grey20"
#'    \item `timeslider.pch`: a single value with symbol type; default = 21
#' }
#'
#' ***To customize parameters controlled by `par`:*** Add prefix
#' `par.` to any argument that can be passed to [par][graphics::par]. Note that
#' `par.mar` controls whitespace behind default timeslider. The following values
#' will create the default plot:
#' \itemize{
#'    \item `par.oma`: plot outer margins; default = c(0,0,0,0)
#'    \item `par.mar`: plot inner margins; default = c(6,0,0,0)
#' }
#'
#'  If `animate = TRUE` then the animation output file name (`ani_name`
#'  argument) will be passed to the `output` argument in [make_video()]. Default
#'  values for all other [make_video()] arguments will be used. Note that the
#'  default frame rate is 24 frames per second (`framerate` argument in
#'  [make_video()]), which will determine the total length (duration) of the
#'  output video. For example, a video containing 240 images (frames) will run
#'  for 10 seconds at these default parameters. Note that output video duration,
#'  dimensions (size), and other ouput video characteristics can be modified by
#'  calling [make_video()] directly. To do this, set `animate = FALSE` and then
#'  use [make_video()] to create a video from the resulting set of images.
#'
#'
#' @return Sequentially-numbered png files (one for each frame) and one mp4 file
#'  will be written to `out_dir`.
#'
#' @author Todd Hayden, Tom Binder, Chris Holbrook
#'
#' @note
#' *Customizing plot elements with input argument ``...``*
#' The option to allow customization of plot elements with input argument `...`
#' provides a great deal of flexibility, but users will need to be familiar with
#' each associated graphics functions (e.g., [axis][graphics::axis] for timeline
#' arguments). We expect that this will require some trial and error and that
#' input argument `preview = TRUE` will be useful while exploring optional plot
#' arguments.
#'
#'
#' @examples
#' \dontrun{
#'
#' # load detection data
#' det_file <- system.file("extdata", "walleye_detections.csv",
#'   package = "glatos"
#' )
#' dtc <- read_glatos_detections(det_file)
#'
#' # take a look
#' head(dtc)
#'
#' # load receiver location data
#' rec_file <- system.file("extdata",
#'   "sample_receivers.csv",
#'   package = "glatos"
#' )
#' recs <- read_glatos_receivers(rec_file)
#'
#' # call with defaults; linear interpolation
#' pos1 <- interpolate_path(dtc)
#'
#' # make frames, preview the first frame
#' myDir <- paste0(getwd(), "/frames1")
#' make_frames(pos1, recs = recs, out_dir = myDir, preview = TRUE)
#'
#' # make frames but not animation
#' myDir <- paste0(getwd(), "/frames2")
#' make_frames(pos1, recs = recs, out_dir = myDir, animate = FALSE)
#'
#' # make sequential frames, and animate.  Make animation and frames.
#' # change default color of fish markers to red and change marker and size.
#'
#' myDir <- paste0(getwd(), "/frames3")
#' make_frames(pos1,
#'   recs = recs, out_dir = myDir, animate = TRUE,
#'   ani_name = "animation3.mp4", col = "red", pch = 16, cex = 3
#' )
#'
#' # make sequential frames, animate, add 5-day tail
#' myDir <- paste0(getwd(), "/frames4")
#' make_frames(pos1,
#'   recs = recs, out_dir = myDir, animate = TRUE,
#'   ani_name = "animation4.mp4", tail_dur = 5
#' )
#'
#' # make animation, remove frames.
#' myDir <- paste0(getwd(), "/frames5")
#' make_frames(pos1,
#'   recs = recs, out_dir = myDir, animate = TRUE,
#'   ani_name = "animation5.mp4", frame_delete = TRUE
#' )
#' }
#'
#' @export

make_frames <- function(
    proc_obj,
    recs = NULL,
    out_dir = getwd(),
    background_ylim = c(41.3, 49.0),
    background_xlim = c(-92.45, -75.87),
    show_interpolated = TRUE,
    tail_dur = 0,
    animate = TRUE,
    ani_name = "animation.mp4",
    frame_delete = FALSE,
    overwrite = FALSE,
    preview = FALSE,
    bg_map = NULL,
    show_progress = TRUE,
    ...) {
  # NOTE: As of glatos v 0.4.1, the package no longer uses the external program ffmpeg.  Input argument 'ffmpeg' has been removed"

  #  Declare global variables for NSE & R CMD check
  row_in <- recover_date_time <- grp <- bin_timestamp <- t_end <- grp_num <-
    f_name <- animal_id <- record_type <- latitude <- longitude <-
    deploy_date_time <- great_lakes_polygon <- NULL

  # expand path to animation output file
  # - place in same file as images (out_dir) if none specified
  # - preserve "./" prefix if specified
  if (animate) {
    # is working directory specifically included by "."?
    starts_with_dot <- grepl(pattern = "^\\.", ani_name)
    if (!starts_with_dot) {
      # includes path to a dir? (separate from out_dir?)
      contains_dir <- dirname(ani_name) != "."
      # if no dir, add out_dir (video in same dir as images)
      if (!contains_dir) ani_name <- file.path(out_dir, ani_name)
    }
  }

  # if overwrite = FALSE, check if animation output file exists
  if (!overwrite & file.exists(ani_name)) {
    stop(
      "Operation aborted ",
      "because output video file ",
      "exists and 'overwrite = ",
      "FALSE'.",
      call. = FALSE
    )
  }

  # Convert proc_obj and recs dataframes into data.table objects
  work_proc_obj <- data.table::as.data.table(proc_obj)

  # make column to identify original row to join with option plot arguments
  work_proc_obj[, row_in := 1:.N]

  # capture optional plot arguments passed via ellipses
  #  and add original row indices to join on both
  inargs <- list(...)
  # set defaults and apply if needed
  rcv_args <- list(pch = 16, cex = 1.5)
  dtc_args <- list(pch = 16, col = "blue", cex = 2)

  # identify and subset par arguments
  par_inargs <- inargs[grepl("^par\\.", names(inargs))] # temporary
  # identify and subset receiver point arguments
  rcv_inargs <- inargs[grepl("^recs\\.", names(inargs))] # temporary
  # identify and subset timeline arguments
  timeline_inargs <- inargs[grepl("^timeline\\.", names(inargs))]
  # identify and subset timeslider arguments
  timeslider_inargs <- inargs[grepl("^timeslider\\.", names(inargs))]

  # identify dtc input arguments
  dtc_inarg_names <- setdiff(
    names(inargs),
    c(
      names(par_inargs),
      names(rcv_inargs),
      names(timeline_inargs),
      names(timeslider_inargs)
    )
  )

  # identify and subset detection point arguments
  dtc_inargs <- inargs[dtc_inarg_names]
  # strip argument names
  names(par_inargs) <- gsub("^par\\.", "", names(par_inargs))
  names(timeline_inargs) <- gsub("^timeline\\.", "", names(timeline_inargs))
  names(timeslider_inargs) <- gsub(
    "^timeslider\\.",
    "",
    names(timeslider_inargs)
  )
  names(rcv_inargs) <- gsub("^recs\\.", "", names(rcv_inargs))

  # update from ...
  if (length(rcv_inargs) > 0) {
    rcv_args[names(rcv_inargs)] <- rcv_inargs
  }
  # update from ...
  if (length(dtc_inargs) > 0) {
    dtc_args[names(dtc_inargs)] <- dtc_inargs
  }

  # expand single rcv_args elements to equal number of rows in recs
  if (!is.null(recs)) {
    for (i in 1:length(rcv_args)) {
      if (length(rcv_args[[i]] == 1)) {
        rcv_args[[i]] <- rep(rcv_args[[i]], nrow(recs))
      } else if (length(rcv_args[[i]]) != nrow(recs)) {
        stop(paste0(
          "Length of optional plot parameters pass via '...' ",
          "must be 1 or equal to\n number of rows in input data."
        ))
      }
    }
  }

  # expand single dtc_args elements to equal number of rows in work_proc_obj
  for (i in 1:length(dtc_args)) {
    if (length(dtc_args[[i]]) == 1) {
      dtc_args[[i]] <- rep(dtc_args[[i]], nrow(work_proc_obj))
    } else if (length(dtc_args[[i]]) != nrow(work_proc_obj)) {
      stop(paste0(
        "Length of optional plot parameters pass via '...' ",
        "must be 1 or equal to\n number of rows in input data."
      ))
    }
  }

  # coerce to data.table and add original row index (for join to recs)
  rcv_args <- data.table::as.data.table(rcv_args)
  rcv_args[, row_in := 1:.N]
  dtc_args <- data.table::as.data.table(dtc_args)
  dtc_args[, row_in := 1:.N]

  # set recs to data.table and remove receivers not recovered
  if (!is.null(recs)) {
    recs <- data.table::as.data.table(recs)

    # make column to identify original row to join with option plot arguments
    recs[, row_in := 1:.N]

    # Remove receivers not recovered (records with NA in recover_date_time)
    data.table::setkey(recs, recover_date_time)
    recs <- recs[
      !list(NA_real_),
      c(
        "station",
        "deploy_lat",
        "deploy_long",
        "deploy_date_time",
        "recover_date_time",
        "row_in"
      )
    ]
  }

  # Make output directory if it does not already exist
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # extract time sequence for plotting
  t_seq <- unique(work_proc_obj$bin_timestamp)

  # make tails if needed
  if (tail_dur == 0) {
    #  Create group identifier for plotting
    work_proc_obj[, grp := bin_timestamp]
  } else {
    # make tail groups if needed
    dur <- work_proc_obj[, .(t_seq = sort(unique(bin_timestamp)))]
    dur[
      ,
      c("t_end", "t_grp") := list(
        data.table::shift(t_seq, type = "lag", fill = min(t_seq), n = tail_dur),
        1:nrow(dur)
      )
    ]

    # group obs for tails
    work_proc_obj[, t_end := bin_timestamp]
    data.table::setkey(dur, t_end, t_seq)

    # merge by overlap
    work_proc_obj <- data.table::foverlaps(
      work_proc_obj,
      dur,
      type = "within",
      nomatch = 0L,
      by.x = c("bin_timestamp", "t_end")
    )
    work_proc_obj <- work_proc_obj[, c(
      "animal_id",
      "t_seq",
      "latitude",
      "longitude",
      "record_type",
      "row_in"
    )]
    data.table::setnames(
      work_proc_obj,
      c(
        "animal_id",
        "bin_timestamp",
        "latitude",
        "longitude",
        "record_type",
        "row_in"
      )
    )
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
  data.table::setkey(work_proc_obj, bin_timestamp, animal_id, record_type)

  # Load background (use example Great Lakes if null)
  if (is.null(bg_map)) {
    # example in glatos package

    utils::data(
      "great_lakes_polygon",
      envir = environment(),
      package = "glatos"
    )
    background <- great_lakes_polygon
    rm(great_lakes_polygon)
  } else {
    background <- bg_map

    # convert to sf if sp::Spatial object
    if (inherits(background, "Spatial")) {
      background <- sf::st_as_sf(background)
      message("Converted sp object to sf")
    }

    # convert to sf if map is terra::SpatVector object
    if (inherits(background, "SpatVector")) {
      background <- sf::st_as_sf(background)
      message("Converted terra object to sf")
    }

    # convert to WGS 84 (EPSG 4326)
    if (sf::st_crs(background)$epsg != 4326) {
      background <- sf::st_transform(background, 4326)
      message("Converted background to long/lat (epsg: 4326) CRS")
    }

    # if x and y limits are equal to default, then set limits to extent of bg_map
    # if x and y limits are not equal to default, then leave as specified in input arguments.
    if (missing(background_ylim) | all(background_ylim == c(41.3, 49.0))) {
      background_ylim <- as.numeric(sf::st_bbox(bg_map)[c("ymin", "ymax")])
    }

    if (missing(background_xlim) | all(background_xlim == c(-92.45, -75.87))) {
      background_xlim <- as.numeric(sf::st_bbox(bg_map)[c("xmin", "xmax")])
    }
  }

  # turn off interpolated points if show_interpolated = FALSE
  if (!show_interpolated) {
    work_proc_obj[record_type == "interpolated", latitude := NA]
    work_proc_obj[record_type == "interpolated", longitude := NA]
  }

  # Calculate the duration of the animation for timeline
  time_period <- range(work_proc_obj$bin_timestamp)

  # define custom plot function

  cust_plot <- function(
      x,
      .time_period,
      .recs,
      .out_dir,
      .background,
      .background_xlim,
      .background_ylim) {

    # Calculate great circle distance in meters of x and y limits.
    # needed to determine aspect ratio of the output

    # old version, new below, needs tested
    # linear_x = geosphere::distMeeus(c(.background_xlim[1], .background_ylim[1]),
    #                                c(.background_xlim[2], .background_ylim[1]))
    # linear_y = geosphere::distMeeus(c(.background_xlim[1], .background_ylim[1]),
    #                                c(.background_xlim[1], .background_ylim[2]))

    linear_x <- geodist::geodist_vec(
      x1 = .background_xlim[1],
      y1 = .background_ylim[1],
      x2 = .background_xlim[2],
      y2 = .background_ylim[1],
      measure = "haversine"
    )

    linear_y <- geodist::geodist_vec(
      x1 = .background_xlim[1],
      y1 = .background_ylim[1],
      x2 = .background_xlim[1],
      y2 = .background_ylim[2],
      measure = "haversine"
    )

    # aspect ratio of image
    figRatio <- linear_y / linear_x

    # calculate image height based on aspect ratio
    height <- trunc(2000 * figRatio)

    # plot GL outline and movement points
    png(
      file.path(.out_dir, x$f_name[1]),
      width = 2000,
      height = ifelse(height %% 2 == 0, height, height + 1),
      units = "px",
      pointsize = 22 * figRatio
    )

    # Plot background image
    # Set bottom margin to plot timeline outside of plot window

    # set defaults and apply if needed
    par_args <- list(oma = c(0, 0, 0, 0), mar = c(6, 0, 0, 0), xpd = FALSE)
    # update from defaults...
    if (length(par_inargs) > 0) {
      par_args[names(par_inargs)] <- par_inargs
    }

    do.call(par, par_args)

    # Note this call was changed to sf?
    plot(
      sf::st_geometry(.background),
      ylim = c(.background_ylim),
      xlim = c(.background_xlim),
      axes = FALSE,
      lwd = 2 * figRatio,
      col = "white",
      bg = "gray74"
    )

    box(lwd = 3 * figRatio)

    # Add receiver locations
    if (!is.null(.recs)) {
      # extract receivers in the water during plot interval
      sub_recs <- .recs[
        deploy_date_time <= x$bin_timestamp[1] &
          (recover_date_time >= x$bin_timestamp[1] & !is.na(recover_date_time))
      ]

      # get optional plot arguments that correspond with sub_recs
      sub_rcv_args <- rcv_args[match(sub_recs$row_in, rcv_args$row_in), ]

      # plot receivers; not do.call to include optional input arguments
      do.call(
        points,
        c(
          list(x = sub_recs$deploy_long, y = sub_recs$deploy_lat),
          sub_rcv_args[, !"row_in", with = FALSE]
        )
      )
    }

    # Add timeline
    par(xpd = TRUE)

    # Define timeline x and y location
    xlim_diff <- diff(.background_xlim)
    ylim_diff <- diff(.background_ylim)
    timeline_y <- rep(.background_ylim[1] - (0.06 * ylim_diff), 2)
    timeline_x <- c(
      .background_xlim[1] + (0.10 * xlim_diff),
      .background_xlim[2] - (0.10 * xlim_diff)
    )

    time_dur <- diff(as.numeric(.time_period))

    # Add labels to timeline
    labels <- seq(
      as.POSIXct(format(min(.time_period), "%Y-%m-%d")),
      as.POSIXct(format(max(.time_period), "%Y-%m-%d")),
      length.out = 5
    )
    labels_ticks <- as.POSIXct(format(labels, "%Y-%m-%d"), tz = "GMT")
    ptime <- (as.numeric(labels_ticks) - as.numeric(min(.time_period))) /
      time_dur
    labels_x <- timeline_x[1] + (diff(timeline_x) * ptime)

    # set defaults and apply if needed
    timeline_args <- list(
      side = 1,
      at = labels_x,
      pos = timeline_y[1],
      labels = format(labels, "%Y-%m-%d"),
      col = "grey70",
      lwd = 20 * figRatio,
      lend = 0,
      lwd.ticks = NA,
      col.ticks = 1,
      cex.axis = 2,
      padj = 0.5
    )

    # update from ...
    if (length(timeline_inargs) > 0) {
      timeline_args[names(timeline_inargs)] <-
        timeline_inargs
    }

    do.call(axis, timeline_args)

    # Update timeline
    ptime <- (as.numeric(x[1, "grp"]) - as.numeric(min(.time_period))) /
      time_dur

    # Proportion of timeline elapsed
    timeline_x_i <- timeline_x[1] + diff(timeline_x) * ptime

    # set defaults and apply if needed
    timeslider_args <- list(
      pch = 21,
      cex = 2,
      bg = "grey40",
      col = "grey20",
      lwd = 1
    )

    # update from ...
    if (length(timeslider_inargs) > 0) {
      timeslider_args[names(timeslider_inargs)] <-
        timeslider_inargs
    }

    # Plot slider along timeline at appropriate location
    do.call(
      points,
      c(
        list(x = timeline_x_i, y = timeline_args$pos),
        timeslider_args
      )
    )

    # Add fish positions
    # get optional plot arguments that correspond with x
    sub_dtc_args <- dtc_args[match(x$row_in, dtc_args$row_in), ]

    do.call(
      points,
      c(
        list(x = x$longitude, y = x$latitude),
        sub_dtc_args[, !"row_in", with = FALSE]
      )
    )

    dev.off()
  }

  # order for plotting
  data.table::setkey(work_proc_obj, grp_num)

  if (preview) {
    grpn <- 1
  } else {
    # start progress bar
    grpn <- data.table::uniqueN(work_proc_obj$grp)
    if (show_progress) pb <- txtProgressBar(min = 0, max = grpn, style = 3)
  }

  # call cust_plot witin data.table
  work_proc_obj[
    grp_num <= grpn,
    {
      if (!preview & show_progress) {
        setTxtProgressBar(pb, .GRP)
      }
      cust_plot(
        x = .SD,
        .time_period = time_period,
        .recs = recs,
        .out_dir = out_dir,
        .background = background,
        .background_xlim = background_xlim,
        .background_ylim = background_ylim
      )
    },
    by = grp,
    .SDcols = c(
      "bin_timestamp",
      "longitude",
      "latitude",
      "record_type",
      "f_name",
      "grp",
      "row_in"
    )
  ]

  if (preview) {
    message("Preview frames written to\n ", out_dir)
  } else {
    if (show_progress) {
      close(pb)
    }

    if (animate) {
      make_video(input_dir = out_dir, output = ani_name, overwrite = overwrite)
    }

    if (frame_delete) {
      unlink(file.path(out_dir, unique(work_proc_obj$f_name)))
    } else {
      message("Frames written to\n ", out_dir)
    }
  }
}
