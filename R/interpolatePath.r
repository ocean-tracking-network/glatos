#' Interpolate new positions within a spatiotemporal path data 
#'
#' Interpolate new positions within a spatiotemporal path data set 
#'   (e.g., detections of tagged fish) at regularly-spaced time intervals   
#' 	 using linear or non-linear interpolation (via \code{\link{movePath}}).
#' 
#' @param dtc A data frame containing spatiotemporal data with at
#'   least 4 columns containing 'animal_id' (numeric or character),
#'   'detection_timestamp_utc' (POSIXct),'deploy_lat' (numeric),
#'   'deploy_long' (numeric).  'deploy_lat' and 'deploy_long' must be
#'   in WGS 84 coordinate system and 'detection_timestamp_utc'. Default column names match the
#'   GLATOS detections export. Additional columns in dtc will be ignored.
#'
#' @param int_time_stamp The time step size (in seconds) of interpolated 
#'   positions. Default is 86400 (one day).
#'   
#' @param trans An optional transition matrix with the "cost" of
#'   moving across each cell within the map extent. Must be of class
#'   \code{TransitionLayer} (See \code{gdistance} package).
#'   
#' @param lnl_thresh A numeric threshold for determining if linear or
#'   non-linear interpolation will be used based on the ratio of
#'   linear-to-non-linear shortest path distances.
#'
#' @param detColNamesA list with names of columns in \code{dtc}:
#' \itemize{
#'  \item \code{individualCol} is a character string that uniquely identifies 
#'     an individual (e.g., tagged animal). Default is 'animal_id'.
#'  \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps. Default is 'detection_timestamp_utc'.
#'  \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude data. Default is 'deploy_lat'.
#'  \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver. Default is 'deploy_long'.
#'  \item \code{typeCol} is a character string with the name of the optional 
#'     column that identifies the type of record. Default is 'record_type'.} 
#'
#' @details Non-linear interpolation uses the 'gdistance' package to
#'   find the shortest pathway between two locations (i.e., receivers)
#'   that avoid 'impossible' movements (e.g., over land for fish). The
#'   shortest non-linear path between two locations is calculated
#'   using a 'transition matrix layer' (\code{rast}) that represents
#'   the 'cost' of an animal moving between adjacent grid cells.  For
#'   example, each cell in \code{rast} may be coded as water (1) or
#'   land (0) to represent possible (1) and impossible (0) movement
#'   path.
#' 
#' @details Linear interpolation is used for all points when
#'   \code{trans} is not supplied.  When \code{trans} is supplied,
#'   then interpolation method is determined for each pair of observed
#'   positions. For example, linear interpolation will be used if the
#'   two points are exactly the same and when the ratio of linear-to-
#'   # non-linear shortest path distances exceeds
#'   \code{lnlThresh}. \code{lnlThresh} can be used to control whether
#'   non-linear or linear interpolation is used for all points. For
#'   example, non-linear interpolation will be used for all points
#'   when \code{lnlThresh} = 1 and linear interpolation will be used
#'   for all points when \code{lnl_thresh} = 0.
#'
#' @return A data frame with animal_id, detection_timestamp_utc,
#'   deploy_lat, deploy_long, and record type.
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
#' data(walleye_detections) 
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
#' pts2 <- SpatialPoints(pos2[, c("deploy_long","deploy_lat")])
#' points(pts2, pch=20, col='blue', cex=0.5)
#' 
#' @export 

interpolatePath <- function(dtc, trans = NULL, int_time_stamp = 86400,
                            lnl_thresh = 0.9,
                            detColNames = list(individualCol="animal_id",
                                               timestampCol = "detection_timestamp_utc",
                                               latitudeCol = "deploy_lat",
                                               longitudeCol = "deploy_long",
                                               typeCol = "record_type")){
  # this function uses data.table extensively
  setDT(dtc)
  
  # update defaultColNames with detColNames
  defaultColNames <- list(individualCol="animal_id",
                          timestampCol="detection_timestamp_utc",
                          latitudeCol="deploy_lat",
                          longitudeCol="deploy_long",
                          typeCol="record_type")
  for(i in 1:length(detColNames))
    defaultColNames[[names(detColNames)[i]]] <- detColNames[[i]]

  # Check that detections data frame contains required columns
  missingCols <- setdiff(unlist(defaultColNames[1:4]), names(dtc))
  if (length(missingCols) > 0){
    stop(paste0("'dtc' data frame is missing the following ", "column(s):\n",
                paste0("       '",missingCols,"'", collapse="\n")), call.=FALSE)
  }

  missingCols <- setdiff(defaultColNames$typeCol, names(dtc))
  if (length(missingCols) > 0) dtc[, defaultColNames$typeCol:= "detection"]
  dtc <- dtc[, unlist(defaultColNames), with = FALSE]

  # define column names used only inside this function
  colNamesInternal <- c("individual","timestamp","latitude","longitude","type")
  names(dtc) <- colNamesInternal

  # Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(dtc$timestamp))){
    stop(paste0("Column '",defaultColNames$timestampCol,
                "' in 'dtc' data frame must be of class 'POSIXct'."),
         call.=FALSE)
  } 	

  # count number of rows- single observations are not interpolated
  dtc[, num_rows := nrow(.SD), by = individual]
  
  # Sort detections by transmitter id and then by detection timestamp
  setkey(dtc, individual, timestamp)

  # save original dataset to combine with interpolated data in the end
  det <- dtc
  names(det) <- c("individual", "bin_stamp", "i_lat", "i_lon", "type", "num_rows")

  # remove any fish with only one detection
  dtc <- dtc[num_rows != 1]

  # error if only fish with one observation.
  if (nrow(dtc) == 0) {stop("must have two observations to interpolate")
  }
  
  t_seq <- seq(min(dtc$timestamp), max(dtc$timestamp), int_time_stamp)

  # bin data by time interval and add bin to dtc
  dtc[, bin := t_seq[findInterval(timestamp, t_seq)] ]

  # make all combinations of animals and detection bins
  dtc <- merge(CJ(bin = t_seq, individual = unique(dtc$individual)), dtc,
               by = c("bin", "individual"), all.x = TRUE)
  setkey(dtc, individual, bin, timestamp)

  # if only need to do linear interpolation:
  if (is.null(trans) | lnl_thresh == 0){
    dtc[, bin_stamp := timestamp][is.na(timestamp), bin_stamp := bin]
    dtc[, i_lat := approx(timestamp, latitude, xout = bin_stamp)$y,
        by = individual]
    dtc[, i_lon := approx(timestamp, longitude, xout = bin_stamp)$y,
        by = individual]
    dtc[is.na(longitude), type := "inter"]
    dtc <- dtc[, c("individual", "bin_stamp", "i_lat", "i_lon", "type")]
    det <- det[num_rows == 1, c("individual", "bin_stamp", "i_lat", "i_lon",
                                "type")]
    out <- rbind(dtc, det)
    setkey(out, individual, bin_stamp)
    out[, bin_stamp := t_seq[findInterval(bin_stamp, t_seq)] ]
    names(out) <- c("individual", "timestamp", "latitude", "longitude", "type")

    # set column names back to original names
    changeNames <- names(out) %in% colNamesInternal
    newNames <- unname(unlist(defaultColNames)[match(names(out)[changeNames],
                                                   colNamesInternal)])
    names(out)[changeNames] <- newNames
    return(as.data.frame(out))
    stop
  }

  # routine for combined nln and ln interpolation
  # identify start and end rows for observations before and after NA
  ends <- dtc[!is.na(latitude), .(start = .I[-nrow(.SD)], end = .I[-1]),
              by = individual][end - start > 1]

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
  new_ends <- dtc[!is.na(latitude), .(start = .I[-nrow(.SD)], end = .I[-1]),
                  by = individual][end - start > 1]

  # create row index needed for overlap join
  dtc[, c("start", "end") := list(1:.N, 1:.N)]
  setkey(new_ends, start, end)
  setkey(dtc, start, end)

  # extract rows that need interpolation
  dtc <- foverlaps(new_ends[, -1], dtc, by.x = c("start", "end"),
                   by.y = c("start", "end"))

  setkey(dtc, individual, bin, timestamp)

  # calculate great circle distance between coords
  dtc[, gcd := geosphere::distHaversine(as.matrix(
    .SD[1, c("longitude", "latitude")]),
    as.matrix(.SD[.N, c("longitude", "latitude")])), by = i.start]

  # calculate least cost (non-linear) distance between points
  dtc[, lcd := gdistance::costDistance(trans, fromCoords = as.matrix(
    .SD[1, c("longitude", "latitude")]),
    toCoords = as.matrix(.SD[.N, c("longitude", "latitude")])),
    by = i.start]

  # calculate ratio of gcd:lcd
  dtc[, crit := gcd / lcd]

  # create keys for lookup
  dtc[!is.na(timestamp), t_lat := data.table::shift(latitude, type = "lead"), by = i.start]
  dtc[!is.na(timestamp), t_lon := data.table::shift(longitude, type = "lead"), by = i.start]
  dtc[!is.na(timestamp), t_timestamp := data.table::shift(timestamp, type = "lead"), by = i.start]

  # extract rows that need non-linear interpolation
  # based on ratio between gcd:lcd
  nln <- dtc[crit < lnl_thresh ]

  land_chk <- dtc[is.infinite(lcd)][!is.na(latitude), c("latitude", "longitude")]

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
    ln <- data.table(individual = character(), i_lat = numeric(), i_lon = numeric(),
                     bin_stamp = as.POSIXct(character()), type = character())
    } else {
    
    # linear interpolation
    ln[, bin_stamp := timestamp][is.na(timestamp), bin_stamp := bin]
    ln[, i_lat := {tmp = .SD[c(1, .N), c("timestamp", "latitude")];
    approx(c(tmp$timestamp), c(tmp$latitude), xout = c(bin_stamp))$y},
    by = i.start]
    ln[, i_lon := {tmp = .SD[c(1, .N), c("timestamp", "longitude")];
    approx(c(tmp$timestamp), c(tmp$longitude), xout = c(bin_stamp))$y},
    by = i.start]
    ln[is.na(longitude), type := "inter"]
  }

  # extract records to lookup
  nln_small <- nln[ !is.na(timestamp)][!is.na(t_lat)]
  
  if(nrow(nln_small) == 0){
    nln <- data.table(individual = character(), i_lat = numeric(), i_lon = numeric(),
                      bin_stamp = as.POSIXct(character()), type = character())
  } else {

    # nln interpolation
    # create lookup table
    setkey(nln_small, latitude, longitude, t_lat, t_lon)
    lookup <- unique(nln_small[, .(latitude, longitude, t_lat, t_lon),
                               allow.cartesian = TRUE])

    # calculate non-linear interpolation for all unique movements in lookup table
    lookup[, coord := sp::coordinates(gdistance::shortestPath(trans, as.matrix(
      .SD[1, c("longitude", "latitude")]), as.matrix(
        .SD[1, c("t_lon", "t_lat")]), output = "SpatialLines")),
      by = 1:nrow(lookup)]
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
    lookup[lookup[, .I[1], by = grp]$V1, nln_longitude := longitude]
    lookup[lookup[, .I[.N], by = grp]$V1, nln_longitude := t_lon]
    lookup[lookup[, .I[1], by = grp]$V1, nln_latitude := latitude]
    lookup[lookup[, .I[.N], by = grp]$V1, nln_latitude := t_lat]
    lookup[,seq_count := 1:.N, by = grp]

    # lookup interpolated values for original dataset
    setkey(lookup, latitude, longitude, t_lat, t_lon)
    nln_small <- lookup[nln_small, allow.cartesian = TRUE]
    setkey(nln_small, i.start, seq_count)

    # add timeseries for interpolating nln movements
    nln_small[nln_small[, .I[1], by = i.start]$V1, i_time := timestamp]
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
    nln[, bin_stamp := timestamp]
    nln[is.na(timestamp), bin_stamp := bin]
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

    nln[is.na(longitude), type := "inter"]
  }

  # combine into a single data.table
  out <- rbind(ln[type == "inter", c("individual", "bin_stamp", "i_lat",
                                     "i_lon", "type")],
               nln[type == "inter", c("individual", "bin_stamp", "i_lat",
                                      "i_lon", "type")],
               det[, c("individual", "bin_stamp", "i_lat", "i_lon", "type")])

  out[, !c("individual")]
  setkey(out, individual, bin_stamp)
  out[, bin_stamp := t_seq[findInterval(bin_stamp, t_seq)] ]
  names(out) <- c("individual", "timestamp", "latitude", "longitude", "type")

  # set column names back to original names
  changeNames <- names(out) %in% colNamesInternal
  newNames <- unname(unlist(defaultColNames)[match(names(out)[changeNames],
                                                   colNamesInternal)])
  names(out)[changeNames] <- newNames
  return(as.data.frame(out))
}
