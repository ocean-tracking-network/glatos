#' Interpolate new positions within a spatiotemporal path data 
#'
#' Interpolate new positions within a spatiotemporal path data set 
#'   (e.g., detections of tagged fish) at regularly-spaced time intervals   
#' 	 using linear or non-linear interpolation.
#' 
#' @param dtc An object of class \code{glatos_detections} or data frame
#'   containing spatiotemporal data with at least 4 columns containing
#'   'animal_id', 'detection_timestamp_utc','deploy_long', and
#'   'deploy_lat' columns.
#'
#' @param int_time_stamp The time step size (in seconds) of interpolated 
#'   positions. Default is 86400 (one day).
#'   
#' @param trans An optional transition matrix with the "cost" of
#'   moving across each cell within the map extent. Must be of class
#'   \code{TransitionLayer} (See \code{gdistance} package).
#'   
#' @param lnl_thresh A numeric threshold for determining if linear or
#'   non-linear interpolation shortest path will be used.
#'
#' @details Non-linear interpolation uses the 'gdistance' package to
#'   find the shortest pathway between two locations (i.e., receivers)
#'   that avoid 'impossible' movements (e.g., over land for fish). The
#'   shortest non-linear path between two locations is calculated
#'   using a 'transition matrix layer' (\code{trans}) that represents
#'   the 'cost' of an animal moving between adjacent grid cells.  For
#'   example, each cell in \code{rast} may be coded as water (1) or
#'   land (0) to represent possible (1) and impossible (0) movement
#'   paths.
#' 
#' @details Linear interpolation is used for all points when
#'   \code{trans} is not supplied.  When \code{trans} is supplied,
#'   then interpolation method is determined for each pair of observed
#'   positions. For example, linear interpolation will be used if the
#'   two points are exactly the same and when the ratio (linear
#'   distance:non-linear distance) between two positions is less than
#'   \code{lnl_thresh}.  Non-linear interpolation will be used when
#'   ratio is greater than \code{lnl_thresh}.  \code{lnl_thresh} can
#'   be used to control whether non-linear or linear interpolation is
#'   used for all points. For example, non-linear interpolation will
#'   be used for all points when \code{lnl_thresh} = 1 and linear
#'   interpolation will be used for all points when \code{lnl_thresh}
#'   = 0.
#'
#' @return A dataframe with animal_id, bin_timestamp,
#'   latitude, longitude, and record type.
#'
#'
#' @author Todd Hayden
#' 
#' @examples
#' 
#' -------------------------------------------------------
#' # EXAMPLE #1 - simple example
#' # example transition matrix
#' data(greatLakesTrLayer)
#'   
#' # example map background
#' data(greatLakesPoly)
#' library(sp) #to plot SpatialPolygon without error
#' plot(greatLakesPoly)
#' 
#' # make up points
#' pos <- data.frame(
#'    id=1,
#'    x=c(-87,-82.5, -78),
#'    y=c(44, 44.5, 43.5),
#'    time=as.POSIXct(c("2000-01-01 00:00",
#'      "2000-02-01 00:00", "2000-03-01 00:00")))
#' 
#' # coerce to SpatialPoints object and plot
#'  pts <- SpatialPoints(pos[,c("x","y")])
#'  points(pts, pch=20, col='red', cex=3)
#' 
#' # interpolate path using linear method
#' path1 <- interpolatePath(pos, 
#'    detColNames=list(individualCol="id", timestampCol="time",
#'    longitudeCol="x", latitudeCol="y"))
#'  
#' # coerce to SpatialPoints object and plot
#' pts1 <- SpatialPoints(path1[,c("x","y")])
#' points(pts1, pch=20, col='blue', lwd=2, cex=1.5) 
#' 
#' # example transition matrix
#' data(greatLakesTrLayer)
#'  
#' # interpolate path using non-linear method (requires 'trans')
#' path2 <- interpolatePath(pos, trans=greatLakesTrLayer,
#' detColNames=list(individualCol="id", timestampCol="time",
#'                  longitudeCol="x", latitudeCol="y"))
#' 
#' # coerce to SpatialPoints object and plot
#' pts2 <- SpatialPoints(path2[,c("x","y")])
#' points(pts2, pch=20, col='green', lwd=2, cex=1.5) 
#'  
#' # can also force linear-interpolation with lnlThresh=0
#' path3 <- interpolatePath(pos, trans=greatLakesTrLayer, lnl_thresh=0,
#'   detColNames=list(individualCol="id", timestampCol="time",
#'                    longitudeCol="x", latitudeCol="y"))
#' 
#' # coerce to SpatialPoints object and plot
#' pts3 <- SpatialPoints(path3[,c("x","y")])
#' points(pts3, pch=20, col='magenta', lwd=2, cex=1.5) 
#' --------------------------------------------------
#' # EXAMPLE #2 - GLATOS detection data
#'
#' # load detection data
#' det_file <- system.file("extdata", "walleye_detections.zip", package = "glatos")
#' det_file <- unzip(det_file, "walleye_detections.csv")
#' dtc <- read_glatos_detections(det_file)
#'
#' # take a look
#' head(walleye_detections)
#'  
#' # call with defaults; linear interpolation
#' pos1 <- interpolatePath(walleye_detections)
#'  
#' # plot on example map background
#' data(greatLakesPoly)
#' library(sp) # to plot SpatialPolygon without error
#' plot(greatLakesPoly)
#' 
#' # remove any missing positions from beginning or end of interpolated timeseries
#' # missing positions occur for timestamps before a fish is first detected
#' # or after a fish is last detected.
#' pos1 <- pos1[pos1$animal_id == 3 & !is.na(pos1$deploy_lat),
#'                   c("deploy_long", "deploy_lat")]
#' 
#' # coerce to SpatialPoints object and plot
#' pts1 <- SpatialPoints(pos1)
#' points(pts1, pch=20, col='red', cex=0.5)
#'  
#' # example transition matrix
#' data(greatLakesTrLayer)
#'  
#' # call with "transition matrix" (non-linear interpolation), other options
#' # note that it is quite a bit slower due than linear interpolation
#' pos2 <- interpolatePath(walleye_detections, trans=greatLakesTrLayer)
#' 
#' # coerce to SpatialPoints object and plot
#' pts2 <- SpatialPoints(pos2[, c("longitude","latitude")])
#' points(pts2, pch=20, col='blue', cex=0.5)
#' 
#' @export 

interpolatePath <- function(dtc, trans = NULL, int_time_stamp = 86400,
                            lnl_thresh = 0.9){
  
  # this function uses data.table extensively
  setDT(dtc)

  # subset only columns for function:
  dtc <- dtc[, c("animal_id", "detection_timestamp_utc", "deploy_lat", "deploy_long")]

  dtc[, record_type := "detection"]
  
  # count number of rows- single observations are not interpolated
  dtc[, num_rows := nrow(.SD), by = animal_id]
  
  # Sort detections by transmitter id and then by detection timestamp
  setkey(dtc, animal_id, detection_timestamp_utc)

  # save original dataset to combine with interpolated data in the end
  det <- dtc
  names(det) <- c("animal_id", "bin_stamp", "i_lat", "i_lon", "record_type", "num_rows")

  # remove any fish with only one detection
  dtc <- dtc[num_rows != 1]

  # error if only fish with one observation.
  if (nrow(dtc) == 0) {stop("must have two observations to interpolate")
  }
  
  t_seq <- seq(min(dtc$detection_timestamp_utc), max(dtc$detection_timestamp_utc), int_time_stamp)

  # bin data by time interval and add bin to dtc
  dtc[, bin := t_seq[findInterval(detection_timestamp_utc, t_seq)] ]

  # make all combinations of animals and detection bins
  dtc <- merge(CJ(bin = t_seq, animal_id = unique(dtc$animal_id)), dtc,
               by = c("bin", "animal_id"), all.x = TRUE)
  setkey(dtc, animal_id, bin, detection_timestamp_utc)

  # if only need to do linear interpolation:
  if (is.null(trans) | lnl_thresh == 0){
    dtc[, bin_stamp := detection_timestamp_utc][is.na(detection_timestamp_utc), bin_stamp := bin]
    dtc[, i_lat := approx(detection_timestamp_utc, deploy_lat, xout = bin_stamp)$y,
        by = animal_id]
    dtc[, i_lon := approx(detection_timestamp_utc, deploy_long, xout = bin_stamp)$y,
        by = animal_id]
    dtc[is.na(deploy_long), record_type := "interpolated"]
    dtc <- dtc[, c("animal_id", "bin_stamp", "i_lat", "i_lon", "record_type")]
    det <- det[num_rows == 1, c("animal_id", "bin_stamp", "i_lat", "i_lon",
                                "record_type")]
    out <- rbind(dtc, det)
    setkey(out, animal_id, bin_stamp)
    out[, bin_stamp := t_seq[findInterval(bin_stamp, t_seq)] ]
    names(out) <- c("animal_id", "bin_timestamp", "latitude", "longitude", "record_type")
    return(as.data.frame(out))
    stop
  }

  # routine for combined nln and ln interpolation
  # identify start and end rows for observations before and after NA
  ends <- dtc[!is.na(deploy_lat), .(start = .I[-nrow(.SD)], end = .I[-1]),
              by = animal_id][end - start > 1]

  # identify observations that are both start and ends
  dups <-  c(ends$start, ends$end)[ ends[, duplicated(c(start, end))]]

  # create and append duplicate rows for observations
  # that are both start and end.
  # This is so each observation can be in only one group

  # identifies rows and duplicate rows that need duplicated
  dtc[, c("rep", "num") := list(1L, 1:.N)][dups, rep := 2L]
  dtc <- dtc[rep(num, rep)]
  dtc[, rep := NULL]
  dtc[, num := NULL]

  # recalculate first and last rows- no duplicate rows this time...
  new_ends <- dtc[!is.na(deploy_lat), .(start = .I[-nrow(.SD)], end = .I[-1]),
                  by = animal_id][end - start > 1]

  # create row index needed for overlap join
  dtc[, c("start", "end") := list(1:.N, 1:.N)]
  setkey(new_ends, start, end)
  setkey(dtc, start, end)

  # extract rows that need interpolation
  dtc <- foverlaps(new_ends[, -1], dtc, by.x = c("start", "end"),
                   by.y = c("start", "end"))

  setkey(dtc, animal_id, bin, detection_timestamp_utc)

  # calculate great circle distance between coords
  dtc[, gcd := geosphere::distHaversine(as.matrix(
    .SD[1, c("deploy_long", "deploy_lat")]),
    as.matrix(.SD[.N, c("deploy_long", "deploy_lat")])), by = i.start]

  # calculate least cost (non-linear) distance between points
  dtc[, lcd := gdistance::costDistance(trans, fromCoords = as.matrix(
    .SD[1, c("deploy_long", "deploy_lat")]),
    toCoords = as.matrix(.SD[.N, c("deploy_long", "deploy_lat")])),
    by = i.start]

  # calculate ratio of gcd:lcd
  dtc[, crit := gcd / lcd]

  # create keys for lookup
  dtc[!is.na(detection_timestamp_utc),
      t_lat := data.table::shift(deploy_lat, type = "lead"), by = i.start]
  dtc[!is.na(detection_timestamp_utc),
      t_lon := data.table::shift(deploy_long, type = "lead"), by = i.start]
  dtc[!is.na(detection_timestamp_utc),
      t_timestamp := data.table::shift(detection_timestamp_utc, type = "lead"),
      by = i.start]

  # extract rows that need non-linear interpolation
  # based on ratio between gcd:lcd
  nln <- dtc[crit < lnl_thresh ]

  land_chk <- dtc[is.infinite(lcd)][!is.na(deploy_lat),
                                    c("deploy_lat", "deploy_long")]

  # stop execution and display offending receivers if any receivers are on land.

  capture <- function(x){paste(capture.output(print(x)), collapse = "\n")
  }

  if (nrow(land_chk) > 0) {stop("coordinates outside extent of transition layer.
    Interpolation impossible! Check receiver locations or extents of transition
    layer:\n", capture(as.data.table(land_chk)), call. = FALSE)
  }

  # extract data for linear interpolation
  # check to make sure that all points to be interpolated
  # are within the tranition layer is needed before any interpolation.

  ln <- dtc[crit >= lnl_thresh | is.nan(crit) ]
  if (nrow(ln) == 0){
    ln <- data.table(animal_id = character(), i_lat = numeric(),
                     i_lon = numeric(),
                     bin_stamp = as.POSIXct(character()),
                     record_type = character())
    } else {

    message("starting linear interpolation")
    # linear interpolation
      ln[, bin_stamp := detection_timestamp_utc][is.na(detection_timestamp_utc),
                                                 bin_stamp := bin]
      ln[, i_lat := {tmp = .SD[c(1, .N),
                               c("detection_timestamp_utc", "deploy_lat")];
                               approx(c(tmp$detection_timestamp_utc),
                                      c(tmp$deploy_lat),
                                      xout = c(bin_stamp))$y}, by = i.start]
      ln[, i_lon := {tmp = .SD[c(1, .N),
                               c("detection_timestamp_utc", "deploy_long")];
                               approx(c(tmp$detection_timestamp_utc),
                                      c(tmp$deploy_long), xout = c(bin_stamp))$y},
         by = i.start]
      ln[is.na(deploy_long), record_type := "interpolated"]
    }
  message("finished linear interpolation")
  
  # extract records to lookup
  nln_small <- nln[ !is.na(detection_timestamp_utc)][!is.na(t_lat)]
  
  if(nrow(nln_small) == 0){
    nln <- data.table(animal_id = character(), i_lat = numeric(),
                      i_lon = numeric(),
                      bin_stamp = as.POSIXct(character()),
                      record_type = character())
  } else {
    # nln interpolation
    # create lookup table
    setkey(nln_small, deploy_lat, deploy_long, t_lat, t_lon)
    lookup <- unique(nln_small[, .(deploy_lat, deploy_long, t_lat, t_lon),
                               allow.cartesian = TRUE])

    message("starting non-linear interpolation")
    # calculate non-linear interpolation for all unique movements in lookup table
    lookup[, coord := {sp::coordinates(
      gdistance::shortestPath(trans, as.matrix(
      .SD[1, c("deploy_long", "deploy_lat")]), as.matrix(
        .SD[1, c("t_lon", "t_lat")]), output = "SpatialLines"))},
      by = 1:nrow(lookup)]

    message("finished non-linear interpolation")
    
    lookup[, grp := 1:.N]

    # extract interpolated points from coordinate lists...
    res <- lookup[, .(nln_longitude = lookup$coord[[.I]][, 1],
                      nln_latitude = lookup$coord[[.I]][, 2]), by = grp]

    # set keys, join interpolation and original data
    setkey(lookup, grp)
    setkey(res, grp)
    lookup <- lookup[res]
    lookup[, coord := NULL]

    # added first/last rows, number sequence for groups
    lookup[lookup[, .I[1], by = grp]$V1, nln_longitude := deploy_long]
    lookup[lookup[, .I[.N], by = grp]$V1, nln_longitude := t_lon]
    lookup[lookup[, .I[1], by = grp]$V1, nln_latitude := deploy_lat]
    lookup[lookup[, .I[.N], by = grp]$V1, nln_latitude := t_lat]
    lookup[,seq_count := 1:.N, by = grp]

    # lookup interpolated values for original dataset
    setkey(lookup, deploy_lat, deploy_long, t_lat, t_lon)
    nln_small <- lookup[nln_small, allow.cartesian = TRUE]
    setkey(nln_small, i.start, seq_count)

    # add timeseries for interpolating nln movements
    nln_small[nln_small[, .I[1], by = i.start]$V1,
              i_time := detection_timestamp_utc]
    nln_small[nln_small[, .I[.N], by = i.start]$V1, i_time := t_timestamp]

    # calculate cumdist
    nln_small[, cumdist := cumsum(c(0, sqrt(diff(nln_longitude) ^ 2 +
                                              diff(nln_latitude) ^ 2))),
              by = i.start]

    # interpolate missing timestamps for interpolated coordinates
    nln_small[, i_time := as.POSIXct(approx(cumdist, i_time, xout = cumdist)$y,
                                     origin = "1970-01-01 00:00:00",
                                     tz = attr(nln_small$i_time, "tzone")),
              by = i.start]

    # create timestamp vector to interpolate on.
    nln[, bin_stamp := detection_timestamp_utc]
    nln[is.na(detection_timestamp_utc), bin_stamp := bin]
    nln[, grp := i.start]

    # interpolate timestamps
    setkey(nln_small, i.start)
    setkey(nln, i.start)
    nln[, i_lat := {tmp = nln_small[.(.SD[1, "i.start"]),
                                    c("i_time", "nln_latitude")];
                                    approx(tmp$i_time, tmp$nln_latitude,
                                           xout = bin_stamp)$y}, by = grp]

    nln[, i_lon := {tmp = nln_small[.(.SD[1, "i.start"]),
                                    c("i_time", "nln_longitude")];
                                    approx(tmp$i_time, tmp$nln_longitude,
                                           xout = bin_stamp)$y}, by = grp]

    nln[is.na(deploy_long), record_type := "interpolated"]
  }

  # combine into a single data.table
  out <- rbind(ln[record_type == "interpolated",
                  c("animal_id", "bin_stamp", "i_lat", "i_lon", "record_type")],
               nln[record_type == "interpolated",
                   c("animal_id", "bin_stamp", "i_lat", "i_lon", "record_type")],
               det[, c("animal_id", "bin_stamp", "i_lat", "i_lon", "record_type")])

  out[, !c("animal_id")]
  setkey(out, animal_id, bin_stamp)
  out[, bin_stamp := t_seq[findInterval(bin_stamp, t_seq)] ]
  names(out) <- c("animal_id", "bin_timestamp", "latitude", "longitude", "record_type")

  return(as.data.frame(out))
}
