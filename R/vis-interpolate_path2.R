#' Interpolate new positions within a spatiotemporal path data 
#'
#' Interpolate new positions within a spatiotemporal path data set 
#'   (e.g., detections of tagged fish) at regularly-spaced time intervals   
#' 	 using linear or non-linear interpolation.
#' 
#' @param det An object of class \code{glatos_detections} or data frame
#'   containing spatiotemporal data with at least 4 columns containing
#'   'animal_id', 'detection_timestamp_utc', 'deploy_long',
#'   'deploy_lat', 'glatos_array', and 'station_no' columns.
#'
#' @param start_time specify the first time bin for interpolated data.
#'     If not supplied, default is first timestamp in the input data
#'     set.  Must be a character string that can be coerced to
#'     'POSIXct' or an object of class 'POSIXct'.  If character string
#'     is supplied, timezone is automatically set to UTC.
#'
#' @param out_class Return results as a data.table or tibble.  Default
#'   returns results as data.frame.  Accepts `data.table` or `tibble`.
#' 
#' @param int_time_stamp The time step size (in seconds) of interpolated 
#'   positions. Default is 86400 (one day).
#'   
#' @param trans An optional transition matrix with the "cost" of
#'   moving across each cell within the map extent. Must be of class
#'   \code{TransitionLayer}. A transition layer may be created from a
#'   polygon shapefile using \link{make_transition}. If not supplied,
#'   default is to use linear interpolation to calculate fish
#'   positions.
#'
#' @details Non-linear interpolation uses the \code{gdistance} package
#'   to find the shortest pathway between two locations (i.e.,
#'   receivers) that avoid 'impossible' movements (e.g., over land for
#'   fish). The shortest non-linear path between two locations is
#'   calculated using a transition matrix layer that represents the
#'   'cost' of an animal moving between adjacent grid cells.  The
#'   transition matrix layer (see \link{gdistance}) is created from
#'   a polygon shapefile using \link{make_transition} or from a
#'   \code{RasterLayer} object using \link[gdistance]{transition}. In
#'   \code{make_transition}, each cell in the output transition layer
#'   is coded as water (1) or land (0) to represent possible (1) and
#'   impossible (0) movement paths.
#' 
#' @details Linear interpolation is used for all points when
#'   \code{trans} is not supplied.  When \code{trans} is supplied,
#'   non-linear interpolation is used to connect each pair of
#'   sequential movements.  Output of \code{interpolate_path2} is
#'   similar to \code{interpolate_path} but does not allow user to
#'   specify \code{lnl_thresh} to automatically switch between linear
#'   and non-linear interpolation routines.  As a result,
#'   \code{interpolate_path2} calculations are substantially
#'   faster. We recommend using \code{interpolate_path2} as this
#'   function will eventually replace \code{interpolate_path}.
#' 
#' @return A dataframe with animal_id, bin_timestamp,
#'   latitude, longitude, and record_type.
#'
#' @author Todd Hayden, Tom Binder, Chris Holbrook
#' 
#' @examples
#' 
#' #--------------------------------------------------
#' # EXAMPLE #1 - simple interpolate among lakes
#'   
#' library(sp) #for loading greatLakesPoly because spatial object   
#'   
#' # get polygon of the Great Lakes 
#' data(greatLakesPoly) #glatos example data; a SpatialPolygonsDataFrame
#' plot(greatLakesPoly, xlim = c(-92, -76))
#'   
#' # make sample detections data frame
#' pos <- data.frame(
#'    animal_id=1,
#'    deploy_long=c(-87,-82.5, -78),
#'    deploy_lat=c(44, 44.5, 43.5),
#'    detection_timestamp_utc=as.POSIXct(c("2000-01-01 00:00",
#'      "2000-02-01 00:00", "2000-03-01 00:00"), tz = "UTC"),
#'    glatos_array = c("LM", "LH", "LO"),
#'    station_no = c(1,1,1))
#' 
#' #add to plot
#' points(deploy_lat ~ deploy_long, data = pos, pch = 20, cex = 2, col = 'red')
#' 
#' # interpolate path using linear method
#' path1 <- interpolate_path2(pos)
#' nrow(path1) #now 61 points
#' sum(path1$record_type == "interpolated") #58 interpolated points
#'  
#' #add linear path to plot
#' points(latitude ~ longitude, data = path1, pch = 20, cex = 0.8, col = 'blue')
#' 
#' # load a transition matrix of Great Lakes
#' # NOTE: This is a LOW RESOLUTION TransitionLayer suitable only for 
#' #       coarse/large scale interpolation only. Most realistic uses
#' #       will need to create a TransitionLayer; see ?make_transition.
#' data(greatLakesTrLayer) #glatos example data; a TransitionLayer
#'  
#' # interpolate path using non-linear method (requires 'trans')
#' path2 <- interpolate_path2(pos, trans = greatLakesTrLayer)
#' 
#' # add non-linear path to plot
#' points(latitude ~ longitude, data = path2, pch = 20, cex = 1, 
#'        col = 'green')
#'             
#' #--------------------------------------------------
#' # EXAMPLE #2 - walleye in western Lake Erie
#' \dontrun{
#'
#' library(sp) #for loading greatLakesPoly
#' library(raster) #for raster manipulation (e.g., crop)
#'
#' # get example walleye detection data
#' det_file <- system.file("extdata", "walleye_detections.csv",
#'                         package = "glatos")
#' det <- read_glatos_detections(det_file)
#' 
#' # take a look
#' head(det)
#' 
#' # extract one fish and subset date
#' det <- det[det$animal_id == 22 & 
#'            det$detection_timestamp_utc > as.POSIXct("2012-04-08") &
#'            det$detection_timestamp_utc < as.POSIXct("2013-04-15") , ]
#' 
#' # get polygon of the Great Lakes 
#' data(greatLakesPoly) #glatos example data; a SpatialPolygonsDataFrame
#' 
#' # crop polygon to western Lake Erie
#' maumee <-  crop(greatLakesPoly, extent(-83.7, -82.5, 41.3, 42.4))
#' plot(maumee, col = "grey")
#' points(deploy_lat ~ deploy_long, data = det, pch = 20, col = "red", 
#'   xlim = c(-83.7, -80))
#' 
#' #make transition layer object
#' # Note: using make_transition3 here for speed and simplicity, but 
#' #       make_transition is generally preferred for real application  
#' #       if your system can run it see ?make_transition
#' tran <- make_transition3(maumee, res = c(0.1, 0.1))
#' 
#' plot(tran$rast, xlim = c(-83.7, -82.0), ylim = c(41.3, 42.7))
#' plot(maumee, add = TRUE)
#' 
#' # not high enough resolution- bump up resolution
#' tran1 <- make_transition3(maumee, res = c(0.001, 0.001))
#' 
#' # plot to check resolution- much better
#' plot(tran1$rast, xlim = c(-83.7, -82.0), ylim = c(41.3, 42.7))
#' plot(maumee, add = TRUE)
#' 
#' 
#' # add fish detections to make sure they are "on the map"
#' # plot unique values only for simplicity
#' foo <- unique(det[, c("deploy_lat", "deploy_long")]) 
#' points(foo$deploy_long, foo$deploy_lat, pch = 20, col = "red")
#' 
#' # call with "transition matrix" (non-linear interpolation)
#'
#' start_interpolate_path2 <- Sys.time()
#' pos2 <- interpolate_path2(det, trans = tran1$transition, out_class = "data.table")
#' new <- Sys.time() - start_interpolate_path2
#'
#' # plot
#' plot(maumee, col = "grey")
#' points(latitude ~ longitude, data = pos2, pch=20, col='red', cex=0.5)
#'
#' # compare execution time with interpolate_path
#'
#' start_interpolate_path <- Sys.time()
#' pos3 <- interpolate_path(det, trans = tran1$transition, out_class = "data.table")
#' old <- Sys.time() - start_interpolate_path
#'
#'#' # plot
#' dev.new()
#' plot(maumee, col = "grey")
#' points(latitude ~ longitude, data = pos2, pch = 20, col = "red", cex = 0.5) 
#'
#' # timing:
#' (as.numeric(new)/as.numeric(old))*100 # interpolate_path2 is ~ 50% faster than interoplate_path
#' } 
#' 
#' @export 

interpolate_path2 <- function(det, trans = NULL, start_time = NULL,
                               int_time_stamp = 86400,
                               out_class = NULL){

  # check for output type specification
  if (!is.null(out_class)) {
    if (!(out_class %in% c("data.table", "tibble"))) {
      stop("out_class is not a \"data.table\" or \"tibble\"")
    }
  }
  # check for transition layer type
  if (!is.null(trans) & inherits(trans, c("TransitionLayer", 
                                          "TransitionStack")) == FALSE) {
    stop(paste0("Supplied object for 'trans' argument is not class ", 
                "TransitionLayer or TransitionStack."), call. = FALSE)
  }
  
  # check for start time and class
  if (is.null(start_time)) {
    start_time <- min(det$detection_timestamp_utc)
  }
  
  if (is.na(start_time) & length(start_time) > 0) {
    stop("start_time cannot be coerced to 'POSIXct' or 'POSIXt' class")
  }
  
  # convert time if it is provided as character string
  if (is.character(start_time)) {
    start_time <- as.POSIXct(start_time, tz = "UTC")
  }

  # check to make sure start time is before detections
  if (start_time > max(det$detection_timestamp_utc)) {
    stop("start_time is larger than last detection.  No data to interpolate!", 
         call. = FALSE)
  }

  # check to make sure more than one fish is in detections
  # error if only fish with one observation.
  if (nrow(det) == 0) stop("must have two observations to interpolate")

  # transform into data.table
  data.table::setDT(det) 

  # pull out columns needed
  det <- det[detection_timestamp_utc >= start_time,
             c("animal_id", "detection_timestamp_utc", "deploy_lat", "deploy_long", "glatos_array", "station_no")]

  # order detections
  data.table::setorder(det, animal_id, detection_timestamp_utc)

  # save original dataset to combine with interpolated data in the end
  dtc <- data.table::copy(det)

  # set names for output
  data.table::setnames(det, c("animal_id", "detection_timestamp_utc", "deploy_lat", "deploy_long"), c("animal_id", "bin_stamp", "i_lat", "i_lon"))
  det[, record_type := "detection"]


# if transition argument is null, only need to do linear interpolation:
if(is.null(trans)){

  # det contains animal_id, bin_stamp, i_lat, i_lon, record type,
  # start_tm is start_time
  # int_time_bin is int_time_stamp


  ## det_ln <- det
  ## start_tm <- start_time
  ## int_time_bin <- int_time_stamp

out <- ln_interpolation(det_ln = det, start_tm = start_time, int_time_bin = int_time_stamp)

  # fix output names 
  data.table::setnames(out, c("bin_stamp", "i_lat", "i_lon"), c("bin_timestamp", "latitude","longitude"))

  # return results- linear interpolation
  return(out)
}  

# routine for nonlinear interpolation

  # count number of rows- single observations are not interpolated
  dtc[, num_rows := nrow(.SD), by = animal_id]
  
  
  # remove any fish with only one detection
  dtc <- dtc[num_rows != 1]

  # extract and determine start time
  t_seq <- seq(start_time, max(dtc$detection_timestamp_utc),
               int_time_stamp)

  # bin data by time interval and add bin to dtc
  dtc[, bin := t_seq[findInterval(detection_timestamp_utc, t_seq)] ]

  # make all combinations of animals and detection bins
  dtc <- dtc[data.table::CJ(bin = t_seq, animal_id = unique(animal_id)),
             on = c("bin", "animal_id")]
  # setkey  
  data.table::setkey(dtc, animal_id, bin, detection_timestamp_utc)

# only do this if transition layer is provided.
  if(!is.null(trans)){
    message("checking for receivers on land- (step 1)")
    
    
    # check to see if all receivers are in the water
    # throws error if not.
    land_chk(dtc, trans)

    # start nln interpolation calculations
    message("starting non-linear interpolation- (step 2)") 
    
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
                           num_rows = x.num_rows,
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

    # extract unique movements to calculate nln movements
    nln_small <- dtc[, .SD[1,], by = "i.start"]
    lookup <- unique(nln_small, by = c("deploy_lat", "deploy_long", "t_lat", "t_lon"))

    # create igraph object of movements from transition layer
    graph <- igraph::graph.adjacency(gdistance::transitionMatrix(trans),
                                     mode = "undirected",
                                     weighted = TRUE)
    igraph::E(graph)$weight <- 1/igraph::E(graph)$weight

    # make interpolated lookup table
    lookup <- nln_inter(lookup, trans, graph)

    nln_small <- lookup[nln_small, on = .(deploy_lat, deploy_long, t_lat, t_lon),
                        allow.cartesian = TRUE]

    data.table::setkey(nln_small, i.start, seq_count)

    # calculate cumdist 
    nln_small[, latitude_lead := data.table::shift(nln_latitude, type = "lag", fill = NA),
              by = i.start]
    nln_small[, longitude_lead := data.table::shift(nln_longitude, type = "lag", fill = NA),
              by = i.start]

    nln_small[, cumdist :=  geosphere::distGeo(.SD[, c("nln_longitude", "nln_latitude")],
                                               .SD[,c("longitude_lead", "latitude_lead")]),
              by = i.start]
    
    nln_small[is.na(cumdist), cumdist := 0]
    nln_small[, cumdist := cumsum(cumdist), by = i.start]
    nln_small[, latitude_lead := NULL][, longitude_lead := NULL]

    # interpolate timestamp for each interpolated position using
    # cumdist number of interpolated points
    nln_small[nln_small[, .I[.N], by = i.start]$V1, i_time := t_timestamp]
    nln_small[nln_small[, .I[1], by = i.start]$V1,
              i_time := detection_timestamp_utc]
    

    # interpolate timestamps for interpolated points
    nln_small[, grp_row := .N, by = i.start]
    nln_small[grp_row > 1, i_time := as.POSIXct(approx(cumdist, i_time, xout = cumdist, ties = "ordered")$y,
                                                origin = "1970-01-01 00:00:00",
                                                tz = attr(nln_small$i_time, "tzone")),
              by = i.start]

    # create timestamp vector to interpolate on
    dtc[, bin_stamp := detection_timestamp_utc]
    dtc[is.na(detection_timestamp_utc), bin_stamp := bin]

    # calculate nln positions for each bin_timestamp based on interpolated positions 
    dtc[, i_lat := {tmp = nln_small[, .(.SD[, c("i_time", "nln_latitude")])];
      approx(tmp$i_time, tmp$nln_latitude, xout = bin_stamp,
             ties = "ordered")$y}, by = i.start]
    
    dtc[, i_lon := {tmp = nln_small[, .(.SD[, c("i_time", "nln_longitude")])];
      approx(tmp$i_time, tmp$nln_longitude, xout = bin_stamp,
             ties = "ordered")$y}, by = i.start]

    # create id to identify interpolated and detections
    dtc[is.na(detection_timestamp_utc), record_type := "interpolated"]
    dtc[!is.na(detection_timestamp_utc), record_type := "detection"]

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
    data.table::setorder(out, animal_id, bin_timestamp, -record_type)
    data.table::setnafill(out, "locf", cols = c("latitude", "longitude"))
    out <- unique(out)
    
  }

  # If out_class == NULL, then return data as data.table
  if(is.null(out_class)){ out <- as.data.frame(out)
    return(out)
  }

  # if out_class == "tibble", then return tibble object
  if(out_class == "tibble"){ out <- tibble::as_tibble(out)
    return(out)
  }

  # if out_class == NULL, then return data.frame object
  return(out)
}


# supporting function- not exported
# checks for receivers not in the water
land_chk <- function(dtc, trans){
  trans <- raster(trans)
  dtc <- unique(dtc[, c("deploy_lat", "deploy_long", "glatos_array", "station_no")], by = c("deploy_lat", "deploy_long"))
  dtc <- dtc[!is.na(deploy_lat)]
  sp::coordinates(dtc) <- c("deploy_long", "deploy_lat")
  raster::projection(dtc) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  tran_values <- data.table::as.data.table(raster::extract(trans, dtc, sp = TRUE))
  tran_na <- tran_values[is.na(layer)]

  if(nrow(tran_na) != 0){
    coordinates(tran_na) <- c("deploy_long", "deploy_lat")
    one <- union(extent(trans), extent(tran_na))
    out <- setExtent(trans, one) 
    #pdf("check1.pdf")
    plot(out)
    plot(tran_na, add = TRUE, pch = 16, col = "red")
    #dev.off()
    stop("Some coordinates are on land or beyond extent. \n Interpolation impossible! Check receiver locations or extents of transition layer of points on map", call. = FALSE)
  }
} 


ln_interpolation <- function(det_ln, start_tm, int_time_bin){

  det_dt <- data.table::copy(det_ln)
  
  t_seq <- seq(start_tm, max(det_dt$bin_stamp),
               int_time_bin)

  # bin data by time interval and add bin to dtc
  det_dt[, bin := t_seq[findInterval(bin_stamp, t_seq)] ]

  # make all combinations of animals and detection bins
  det_dt <- det_dt[data.table::CJ(bin = t_seq, animal_id = unique(animal_id)),
                   on = c("bin", "animal_id")]
  # setkey  
  data.table::setkey(det_dt, animal_id, bin, bin_stamp)

  # interpolate detections
  det_dt[is.na(record_type), record_type := "interpolated"]
  det_dt[is.na(bin_stamp), bin_stamp := bin]
  det_dt[, i_lat := approx(bin_stamp, i_lat, xout = bin_stamp, ties = "ordered")$y,
         by = animal_id]
  det_dt[, i_lon := approx(bin_stamp, i_lon, xout = bin_stamp, ties = "ordered")$y,
         by = animal_id]
  det_dt[, bin_stamp := t_seq[findInterval(bin_stamp, t_seq)]]
  det_dt <- unique(det_dt)
  det_dt <- det_dt[, c("animal_id", "bin_stamp", "i_lat", "i_lon", "record_type")]
  
  data.table::setorder(det_dt, animal_id, bin_stamp, -record_type)
  return(det_dt)
  
}

nln_inter <- function(in_dt, tran_layer, i_graph){
  
  # create copy of lookup for calculations
  in_dt1 <- data.table::copy(in_dt)
  
  # find start and end points for all non-linear interpolation
  in_dt1[, start_cell := raster::cellFromXY(tran_layer, in_dt1[, c("deploy_long", "deploy_lat")])]
  in_dt1[, end_cell := raster::cellFromXY(tran_layer, in_dt1[, c("t_lon", "t_lat")])]
  
  # add row number
  in_dt1[, num := 1:.N]

  # calculate interpolated positions (raster cell number)
  int_pos <- apply(in_dt1, MARGIN = 1, FUN = function(x){
    unlist(igraph::shortest_paths(i_graph, x["start_cell"], x[ "end_cell"], output = "vpath")$vpath)
  })

  # extract x and y coordinates from raster
  conv_xy <- lapply(int_pos, FUN = function(x){
    data.table::data.table(raster::xyFromCell(tran_layer, x))
  })

  # combine into a single object
  conv_xy <- data.table::rbindlist(conv_xy, idcol = "num")

  # add rows for each group
  conv_xy[, seq_count := 1:.N, by = .(num)]

  
  # add interpolated points to lookup table
  out <- in_dt1[conv_xy, on = "num"]

  # set first and last rows equal to actual detection
  start <- out[, .(start = .I[1]), by = num]$start
  end <- out[, .(end = .I[.N]), by = num]$end
  out[start, x := deploy_long][start, y := deploy_lat]
  out[end, x := t_lon][end, y := t_lat]
  
  # simplify output
  out <- out[,c("deploy_lat", "deploy_long", "t_lat", "t_lon", "x", "y", "seq_count")]
  data.table::setnames(out, c("x", "y"), c("nln_longitude", "nln_latitude"))

  return(out)
}





##########################

library(data.table)
tst <- data.table(bin = c(1:20), from_lat = seq(from = 41.1, to = 47, length.out = 20), from_lon = seq(from = -83, to = -85, length.out = 20))
#tst[c(2,3,5,7,8,9,12,17,20), c("from_lat", "from_lon") := .(NA, NA)]
tst[c(1,3,5,7,9), c("from_lat", "from_lon") := .(NA, NA)]



foo <- na.omit(tst, cols = c("from_lat", "from_lon"))
foo[, c("to_lat", "to_lon", "to_bin") := .(shift(from_lat, type = "lead"), shift(from_lon, type = "lead"), shift(bin, type = "lead"))][bin + 1 != to_bin]
