library(glatos)
library(sp) 
library(raster)

## # get example walleye detection data
##  det_file <- system.file("extdata", "walleye_detections.csv",
##                          package = "glatos")
##  det <- read_glatos_detections(det_file)

## # extract one fish and subset date
## det <- det[det$animal_id == 22 & 
##             det$detection_timestamp_utc > as.POSIXct("2012-04-08") &
##             det$detection_timestamp_utc < as.POSIXct("2013-04-15") , ]
 
##  # get polygon of the Great Lakes 
##  data(greatLakesPoly) #glatos example data; a SpatialPolygonsDataFrame

## # crop polygon to western Lake Erie
##  maumee <-  crop(greatLakesPoly, extent(-83.7, -82.5, 41.3, 42.4))
## plot(maumee, col = "grey")
##  points(deploy_lat ~ deploy_long, data = det, pch = 20, col = "red", 
##    xlim = c(-83.7, -80))
 
##  # not high enough resolution- bump up resolution
##  tran1 <- make_transition(maumee, res = c(0.001, 0.001))
 
## ## pos2 <- interpolate_path(det, trans = tran1$transition, out_class = "data.table")

det = dtc
trans <- tran$transition
  start_time = as.POSIXct("2018-08-01 00:00:00", tz = "UTC")
  int_time_stamp = 86400
  lnl_thresh= 0.9
  out_class= NULL
show_progress = TRUE
linear = FALSE

## bar <-interpolate_path(det, trans = tran1$transition, start_time = NULL, int_time_stamp = 86400, out_class = NULL, linear = FALSE, show_progress = TRUE)


## interpolate_path_2 <- function(det, trans = NULL, start_time = NULL,
##                              int_time_stamp = 86400,
##                              out_class = NULL, linear = TRUE, show_progress = TRUE){
  
     if (!is.null(out_class)) {
        if (!(out_class %in% c("data.table", "tibble"))) {
            stop("out_class is not a \"data.table\" or \"tibble\"")
        }
    }
    if (!is.null(trans) & inherits(trans, c("TransitionLayer", 
        "TransitionStack")) == FALSE) {
        stop(paste0("Supplied object for 'trans' argument is not class ", 
            "TransitionLayer or TransitionStack."), call. = FALSE)
    }
    if (is.null(start_time)) {
        start_time <- min(det$detection_timestamp_utc)
    }
    if (is.na(start_time) & length(start_time) > 0) {
        stop("start_time cannot be coerced to 'POSIXct' or 'POSIXt' class")
    }
    if (is.character(start_time)) {
        start_time <- as.POSIXct(start_time, tz = "UTC")
    }
    if (start_time > max(det$detection_timestamp_utc)) {
        stop("start_time is larger than last detection.  No data to interpolate!", 
            call. = FALSE)
    }

  # make copy of detections for function
  dtc <- data.table::as.data.table(det)
    
  # subset only columns for function and rows >= start_time:
  dtc <- dtc[detection_timestamp_utc >= start_time, c("animal_id",
                                                      "detection_timestamp_utc",
                                                      "deploy_lat", 
                                                      "deploy_long")]
 
  dtc[, record_type := "detection"]
  
  # count number of rows- single observations are not interpolated
  dtc[, num_rows := nrow(.SD), by = animal_id]
  
  # Sort detections by transmitter id and then by detection timestamp
  data.table::setkey(dtc, animal_id, detection_timestamp_utc)

  # save original dataset to combine with interpolated data in the end
  det <- data.table::copy(dtc)
  data.table::setnames(det, c("animal_id", "bin_stamp", "i_lat", "i_lon", 
                              "record_type", "num_rows"))
  
  # remove any fish with only one detection
  dtc <- dtc[num_rows != 1]

  # error if only fish with one observation.
    if (nrow(dtc) == 0) stop("must have two observations to interpolate")

  # extract and determine start time
  t_seq <- seq(start_time, max(dtc$detection_timestamp_utc),
               int_time_stamp)

  # bin data by time interval and add bin to dtc
  dtc[, bin := t_seq[findInterval(detection_timestamp_utc, t_seq)] ]

  # make all combinations of animals and detection bins
  dtc <- dtc[data.table::CJ(bin = t_seq, animal_id = unique(animal_id)),
             on = c("bin", "animal_id")]

  # if only need to do linear interpolation:
  if(linear){
    dtc[, bin_stamp := detection_timestamp_utc][is.na(detection_timestamp_utc), 
                                                bin_stamp := bin]

    dtc[, i_lat := approx(detection_timestamp_utc, deploy_lat,
                          xout = bin_stamp, ties = "ordered")$y, by = animal_id]
    dtc[, i_lon := approx(detection_timestamp_utc, deploy_long,
                          xout = bin_stamp, ties = "ordered")$y, by = animal_id]

    dtc[is.na(deploy_long), record_type := "interpolated"]
    dtc <- dtc[, c("animal_id", "bin_stamp", "i_lat", "i_lon", "record_type")]
    det <- det[num_rows == 1, c("animal_id", "bin_stamp", "i_lat", "i_lon",
                                "record_type")]
    out <- data.table::rbindlist(list(dtc, det), use.names = TRUE)
    data.table::setkey(out, animal_id, bin_stamp)
    out[, bin_stamp := t_seq[findInterval(bin_stamp, t_seq)] ]
    out <- na.omit(out, cols = "i_lat")
    data.table::setnames(out, c("animal_id", "bin_timestamp", "latitude", 
                                "longitude", "record_type"))
    out <- unique(out)
    out <- data.table::setorder(out, animal_id, bin_timestamp, -record_type)
 }

  # routine for nonlinear interpolation

#  if(!linear){
    message("starting non-linear interpolation- check for points on land- (step 1)")
    
    land_chk <- function(dtc, trans){
      trans <- raster(trans)
      dtc <- unique(dtc[, c("deploy_lat", "deploy_long")])
      dtc <- dtc[!is.na(deploy_lat)]
      sp::coordinates(dtc) <- c("deploy_long", "deploy_lat")
      raster::projection(dtc) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      tran_values <- as.data.table(raster::extract(trans, dtc, sp = TRUE))
      tran_na <- tran_values[is.na(layer)]

      if(nrow(tran_na) != 0){
        coordinates(tran_na) <- c("deploy_long", "deploy_lat")
        one <- union(extent(trans), extent(tran_na))
        out <- setExtent(trans, one) 
        plot(out)
        plot(bar, add = TRUE, pch = 16, col = "red")
        stop("Some coordinates are on land or beyond extent.\n    Interpolation impossible! Check receiver locations or extents of transition\n    layer:\n", 
             utils::capture.output(coordinates(tran_na)), call. = FALSE)
      }
    } 
    
    land_chk(dtc, trans)

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

    # create row index
    dtc[, start_dtc := 1:.N]

    # extract rows that need interpolated
    dtc <- dtc[new_ends, .(animal_id = x.animal_id,
                           detection_timestamp_utc = x.detection_timestamp_utc,
                           deploy_lat = x.deploy_lat, deploy_long = x.deploy_long,
                           record_type = x.record_type, num_rows = x.num_rows,
                           bin = x.bin, i.start = start),
               on = .(start_dtc >= start, start_dtc <= end)]

    # create keys for lookup
    dtc[!is.na(detection_timestamp_utc),
        t_lat := data.table::shift(deploy_lat, type = "lead"), by = i.start]
    dtc[!is.na(detection_timestamp_utc),
        t_lon := data.table::shift(deploy_long, type = "lead"), by = i.start]
    dtc[!is.na(detection_timestamp_utc),
        t_timestamp := data.table::shift(detection_timestamp_utc, type = "lead"),
        by = i.start]

    # extract records to lookup
    nln_small <- dtc[ !is.na(detection_timestamp_utc)][!is.na(t_lat)][!(deploy_lat == t_lat & deploy_long == t_lon),]
  
    # nln interpolation
    # create lookup table
    lookup <- unique(nln_small[, .(deploy_lat, deploy_long, t_lat, t_lon)]) 
    
    # calculate non-linear interpolation for all unique movements in lookup
    message("\nStarting non-linear interpolation... (step 2)")
    grpn <- nrow(lookup)
    if (show_progress)
      pb <- txtProgressBar(min = 0, max = grpn, style = 3)
    lookup[, coord := {
      if(show_progress)
        setTxtProgressBar(pb, value = .GRP)
         sp::coordinates(gdistance::shortestPath(trans, as.matrix(
              .SD[1, c("deploy_long", "deploy_lat")]), as.matrix(
                .SD[1, c("t_lon", "t_lat")]), output = "SpatialLines"))},
      by = 1:nrow(lookup)]
message("\nFinalizing results.")

lookup[, grp := 1:.N]

    # extract interpolated points from coordinate lists...
    res <- lookup[, .(nln_longitude = lookup$coord[[.I]][, 1],
                      nln_latitude = lookup$coord[[.I]][, 2]), by = grp]
    res[, seq_num := 1:.N, by = .(grp)]
    lookup[, coord := NULL]
    lookup <- lookup[res, on = .(grp)] 

    # added first/last rows, number sequence for groups
    lookup[lookup[, .I[1], by = grp]$V1, nln_longitude := deploy_long]
    lookup[lookup[, .I[.N], by = grp]$V1, nln_longitude := t_lon]
    lookup[lookup[, .I[1], by = grp]$V1, nln_latitude := deploy_lat]
    lookup[lookup[, .I[.N], by = grp]$V1, nln_latitude := t_lat]
    
    # lookup interpolated values for original dataset
    data.table::setkey(lookup, deploy_lat, deploy_long, t_lat, t_lon)
    data.table::setkey(nln_small, deploy_lat, deploy_long, t_lat, t_lon)

nln_small <- lookup[nln_small, allow.cartesian = TRUE ]
    data.table::setkey(nln_small, i.start, seq_count)







    # add timeseries for interpolating nln movements
    nln_small[nln_small[, .I[1], by = i.start]$V1,
              i_time := detection_timestamp_utc]
    nln_small[nln_small[, .I[.N], by = i.start]$V1, i_time := t_timestamp]
    nln_small[, latitude_lead := data.table::shift(nln_latitude, type = "lag", fill = NA), by = i.start]
    nln_small[, longitude_lead := data.table::shift(nln_longitude, type = "lag", fill = NA), by = i.start]

    nln_small[, cumdist :=  geosphere::distGeo(.SD[, c("nln_longitude", "nln_latitude")],
                                               .SD[,c("longitude_lead", "latitude_lead")]), by = i.start]

    nln_small[is.na(cumdist), cumdist := 0]
    nln_small[, cumdist := cumsum(cumdist), by = i.start]
    nln_small[, latitude_lead := NULL][, longitude_lead := NULL]

    # interpolate timestamps for interpolated points
    nln_small[, i_time := as.POSIXct(approx(cumdist, i_time, xout = cumdist)$y,
                                     origin = "1970-01-01 00:00:00",
                                     tz = attr(nln_small$i_time, "tzone")),
              by = i.start]

    # create timestamp vector to interpolate on.
    dtc[, bin_stamp := detection_timestamp_utc]
    dtc[is.na(detection_timestamp_utc), bin_stamp := bin]
    dtc[, grp := i.start]

    # interpolate timestamps
   # data.table::setkey(dtc, animal_id, bin, detection_timestamp_utc)
    data.table::setkey(nln_small, 
    dtc[, i_lat := {tmp = nln_small[, .(.SD[, c("i_time", "nln_latitude")])];
                                    approx(tmp$i_time, tmp$nln_latitude, xout = bin_stamp, ties = "ordered")$y}, by = grp]
    
    # interpolate timestamps
    ## data.table::setkey(nln_small, i.start)
    ## data.table::setkey(dtc, i.start)
    ## dtc[, i_lat := {tmp = nln_small[.(.SD[1, "i.start"]),
    ##                                 c("i_time", "nln_latitude")];
    ##                                 approx(tmp$i_time, tmp$nln_latitude,
    ##                                        xout = bin_stamp)$y}, by = grp]

    dtc[, i_lon := {tmp = nln_small[, .(.SD[, c("i_time", "nln_longitude")])];
                                    approx(tmp$i_time, tmp$nln_longitude, xout = bin_stamp, ties = "ordered")$y}, by = grp]

    dtc[is.na(deploy_long), record_type := "interpolated"]

    # combine into a single data.table
    out <- data.table::rbindlist(list(
      dtc[record_type == "interpolated",
          c("animal_id", "bin_stamp", "i_lat", "i_lon", 
            "record_type")],
      det[, c("animal_id", "bin_stamp", "i_lat", "i_lon", 
              "record_type")]), use.names = TRUE)
    data.table::setkey(out, animal_id, bin_stamp)
    out[, bin_stamp := t_seq[findInterval(bin_stamp, t_seq)] ]
    data.table::setnames(out, c("animal_id", "bin_timestamp", "latitude", 
                                "longitude", "record_type"))
    out <- na.omit(out, cols = "latitude")
    out <- unique(out)
    data.table::setorder(out, animal_id, bin_timestamp, -record_type)
    }

    # If out_class == NULL, then return data as data.table
    if(is.null(out_class)){ out <- as.data.frame(out)
      return(out)
    }

    # if out_class == "tibble", then return tibble object
    if(out_class == "tibble"){ out <- tibble::as_tibble(out)
      return(out)}

    # if out_class == NULL, then return data.frame object
    return(out)
  }




make_frames(bar, recs = NULL, out_dir = "~/Desktop/output", background_ylim = c(41.3, 49), background_xlim = c(-92.45, -75.87), show_interpolated = TRUE, tail_dur = 0, animate = FALSE, ani_name = "animation.mp4", frame_delete = FALSE, overwrite = TRUE, bg_map = greatLakesPoly)

###################
# test function- new make_video function that eliminates ffmpeg requirement.
# 

make_video2 <- function(dir = getwd(),
                       pattern = "*.png",
                       output = "animation.mp4",
                       fps_in = 30,
                       start_frame = 1,
                       end_frame = NULL,
                       codec = NULL,
                       vfilter = NULL,
                       verbose = TRUE,
                       audio = NULL){

  files <- list.files(dir, full.names = TRUE, pattern = pattern)

  #check if dir exists
  if(!dir.exists(dir)) stop(paste0("Input dir '", dir , "' not found."), 
                            .call = FALSE)
  
  # subset out frames if needed
  if(is.null(end_frame)){end_frame <- length(files)}
   fls <- files[start_frame:end_frame]

  if(is.null(vfilter)){vfilt <- "null"} else {vfilt = vfilter}
  
  av::av_encode_video(input = fls, output = output, framerate = fps_in, vfilter = vfilt, codec = codec, verbose = verbose)

    message("Video file written to ", output, ".")
}

################
# make video
# adjust file paths...
make_video2(dir = "~/Desktop/output", output = "~/Desktop/output/test.mp4", start_frame = 1, end_frame = NULL)





  
#####

## # plot to check resolution- much better
## plot(tran1$rast, xlim = c(-83.7, -82.0), ylim = c(41.3, 42.7))
## plot(maumee, add = TRUE)

## # add fish detections to make sure they are "on the map"
## # plot unique values only for simplicity
## foo <- unique(out[record_type == "interpolated", c("longitude", "latitude")]) 
## points(foo$longitude, foo$latitude, pch = 20, col = "red")
## points(latitude ~ longitude, data = out, pch=20, col='red', cex=0.5)
##     arch <- trans
##     arch_dtc <- dtc

## trans <- arch
## dtc <- arch_dtc
    
    #test

#    dtc[[1,3]] <- 50
#    dtc[[2,3]] <- 41.7
