#' Interpolate new positions within a spatiotemporal path data 
#'
#' Interpolate new positions within a spatiotemporal path data set 
#'   (e.g., detections of tagged fish) at regularly-spaced time intervals   
#' 	 using linear or non-linear interpolation.
#' 
#' @param det An object of class \code{glatos_detections} or data frame
#'   containing spatiotemporal data with at least 4 columns containing
#'   'animal_id', 'detection_timestamp_utc', 'deploy_long', and
#'   'deploy_lat' columns.
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
#' @param res Cell size for hexagonal grid.  Units are set by
#'   \code{interpolate_crs}.  Default is meters.
#'
#' @param poly sp::SpatialPolygonsDataFrame or \code{sf} simple
#'   feature collection with polygon geometry type area to interpolate
#'   movements over.
#'
#' @param interpolate_crs Coordinate Reference System (CRS) used
#'   internally to calculate hexagonal grid.  Default is a coordinate
#'   system for the Great Lakes.
#' 
#' @details Performs non-linear interpolation to calculate the
#'   shortest path between two locations that avoid 'impossible'
#'   movements (e.g., over land for fish).  The shortest non-linear
#'   path between two locations is calculated by creating a hexagonal
#'   grid of size \code{res} (meters) within the user-supplied polygon
#'   outline of the study area.  The \code{shortest_paths} function
#'   from the \link{igraph} package is used to calculate the path
#'   between adjacent grid cells.
#' 
#' @return A dataframe with animal_id, bin_timestamp,
#'   latitude, longitude, and record_type.
#'
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
#'      "2000-02-01 00:00", "2000-03-01 00:00"), tz = "UTC"))
#' 
#' #add to plot
#' points(deploy_lat ~ deploy_long, data = pos, pch = 20, cex = 2, col = 'red')
#' 
#' # interpolate path using linear method
#' path1 <- interpolate_path(pos)
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
#' path2 <- interpolate_path(pos, trans = greatLakesTrLayer)
#' 
#' # add non-linear path to plot
#' points(latitude ~ longitude, data = path2, pch = 20, cex = 1, 
#'        col = 'green')
#'  
#' # can also force linear-interpolation with lnlThresh = 0
#' path3 <- interpolate_path(pos, trans = greatLakesTrLayer, lnl_thresh = 0)
#' 
#' # add new linear path to plot
#' points(latitude ~ longitude, data = path3, pch = 20, cex = 1, 
#'           col = 'magenta')
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
#' # Note: using make_transition2 here for simplicity, but 
#' #       make_transition is generally preferred for real application  
#' #       if your system can run it see ?make_transition
#' tran <- make_transition(maumee, res = c(0.1, 0.1))
#' 
#' plot(tran$rast, xlim = c(-83.7, -82.0), ylim = c(41.3, 42.7))
#' plot(maumee, add = TRUE)
#' 
#' # not high enough resolution- bump up resolution
#' tran1 <- make_transition(maumee, res = c(0.001, 0.001))
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
#' # call with "transition matrix" (non-linear interpolation), other options
#' # note that it is quite a bit slower due than linear interpolation
#' pos2 <- interpolate_path(det, trans = tran1$transition, out_class = "data.table")
#'
#' plot(maumee, col = "grey")
#' points(latitude ~ longitude, data = pos2, pch=20, col='red', cex=0.5)
#' 
#' } 
#' 
#' @export 


interpolate_path_poly <- function(det, poly, res = 1000,
interpolate_crs = 3175, start_time = NULL, int_time_stamp = 86400,
out_class = NULL){

  # stop if out_class is not NULL, data.table, or tibble
  if(!is.null(out_class)){ if( !(out_class %in% c("data.table",
  "tibble"))) {stop('out_class is not a "data.table" or "tibble"')}}

  # check to see that poly is an sp::spatialPolygonsDataFrame or
  sf::sfc polygon object if(inherits(poly,
  c("SpatialPolygonsDataFrame", "sp", "sf")) == FALSE){
  stop(paste0("Supplied object for 'poly' argument is not class ",
  "sp::spatialPolygonsDataFrame or sf::sfc"), call. = FALSE) }

  if(inherits(poly, c("SpatialPolygonsDataFrame", "sp"))){ poly <-
    sf::st_as_sf(poly) }

  if(inherits(sf::st_geometry(poly), c("sfc_POLYGON", "sfc")) ==
    FALSE){
    stop(paste0("Supplied sobject for 'poly' argument does not have a POLYGON geometry type"),
    call. = FALSE) }

 # check start_time if(is.null(start_time)){start_time <-
  min(det$detection_timestamp_utc) }if(is.na(start_time) &
  length(start_time) > 0){
  stop("start_time cannot be coerced to 'POSIXct' or 'POSIXt' class")
  }

  if(is.character(start_time)){ start_time <- as.POSIXct(start_time,
    tz = "UTC") }

  # make sure start_time < largest timestamp in dataset if(start_time
  > max(det$detection_timestamp_utc)){
  stop("start_time is larger than last detection.  No data to interpolate!",
  call. = FALSE)}

# create grid transform to 3175 poly <- sf::st_transform(poly, crs =
# interpolate_crs) hex <- sf::st_as_sf(sf::st_make_grid(poly, square =
# FALSE, cellsize = res) ) hex$id <- 1:nrow(hex) sf::st_precision(hex)
# <- 0.01 cent <- sf::st_as_sf(sf::st_centroid(st_geometry(hex)))
# cent$id <- 1:nrow(cent) touching <- sf::st_intersects(hex)

  # create edgelist edgelist <- as.matrix(as.data.frame(touching))

  # create igraph graph network gr <-
  igraph::graph_from_edgelist(edgelist, directed = FALSE) E(gr)$weight
  <- 1

  # make copy of detections for function dtc <-
  data.table::as.data.table(det)
    
  # subset only columns for function and rows >= start_time: dtc <-
  dtc[detection_timestamp_utc >= start_time, c("animal_id",
  "detection_timestamp_utc", "deploy_lat", "deploy_long")]
 
  dtc[, record_type := "detection"]
  
  # count number of rows- single observations are not interpolated
  dtc[, num_rows := nrow(.SD), by = animal_id]
  
  # Sort detections by transmitter id and then by detection timestamp
  data.table::setkey(dtc, animal_id, detection_timestamp_utc)

  # save original dataset to combine with interpolated data in the end
  det <- data.table::copy(dtc) data.table::setnames(det,
  c("animal_id", "bin_stamp", "i_lat", "i_lon", "record_type",
  "num_rows"))
  
  # remove any fish with only one detection dtc <- dtc[num_rows != 1]

  # error if only fish with one observation.  if (nrow(dtc) == 0)
    stop("must have two observations to interpolate")

  # extract and determine start time t_seq <- seq(start_time,
max(dtc$detection_timestamp_utc), int_time_stamp)

  # bin data by time interval and add bin to dtc dtc[, bin :=
  t_seq[findInterval(detection_timestamp_utc, t_seq)] ]

  # make all combinations of animals and detection bins dtc <-
  dtc[data.table::CJ(bin = t_seq, animal_id = unique(animal_id)), on =
  c("bin", "animal_id")]

  data.table::setkey(dtc, animal_id, bin, detection_timestamp_utc)
  
  # identify start and end rows for observations before and after NA
  ends <- dtc[!is.na(deploy_lat), .(start = .I[-nrow(.SD)], end =
  .I[-1]), by = animal_id][end - start > 1]

  # identify observations that are both start and ends dups <-
  c(ends$start, ends$end)[ ends[, duplicated(c(start, end))]]

  # create and append duplicate rows for observations that are both
  # start and end.  This is so each observation can be in only one
  # group

  # identifies rows and duplicate rows that need duplicated dtc[,
  c("rep", "num") := list(1L, 1:.N)][dups, rep := 2L] dtc <-
  dtc[rep(num, rep)] dtc[, rep := NULL] dtc[, num := NULL]

  # recalculate first and last rows- no duplicate rows this time...
  new_ends <- dtc[!is.na(deploy_lat), .(start = .I[-nrow(.SD)], end =
  .I[-1]), by = animal_id][end - start > 1]

  # create row index dtc[, start_dtc := 1:.N]

  # extract rows that need interpolated dtc <- dtc[new_ends,
  .(animal_id = x.animal_id, detection_timestamp_utc =
  x.detection_timestamp_utc, deploy_lat = x.deploy_lat, deploy_long =
  x.deploy_long, record_type = x.record_type, num_rows = x.num_rows,
  bin = x.bin, i.start = start), on = .(start_dtc >= start, start_dtc
  <= end)]

  ## # create keys for lookup dtc[!is.na(detection_timestamp_utc),
  t_lat := data.table::shift(deploy_lat, type = "lead"), by = i.start]
  dtc[!is.na(detection_timestamp_utc), t_lon :=
  data.table::shift(deploy_long, type = "lead"), by = i.start]
  dtc[!is.na(detection_timestamp_utc), t_timestamp :=
  data.table::shift(detection_timestamp_utc, type = "lead"), by =
  i.start] dtc[!is.na(detection_timestamp_utc), t_bin :=
  data.table::shift(bin, type = "lead"), by = i.start]

dtc[, bin_timestamp :=
detection_timestamp_utc][is.na(detection_timestamp_utc), bin_timestamp
:= bin]

## # extract all records that need interpolated nln_small <- dtc[
!is.na(detection_timestamp_utc)][!is.na(t_lat)]

# create lookup table- spatial only data.table::setkey(nln_small,
deploy_lat, deploy_long, t_lat, t_lon) lookup <- unique(nln_small[,
.(deploy_lat, deploy_long, t_lat, t_lon), allow.cartesian = TRUE])

start <- sf::st_transform(sf::st_as_sf(lookup, coords =
c("deploy_long", "deploy_lat"), crs = 4326, stringsAsFactors = FALSE),
crs = interpolate_crs) end <- sf::st_transform(sf::st_as_sf(lookup,
coords = c("t_lon", "t_lat"), crs = 4326, stringsAsFactors = FALSE),
crs = interpolate_crs)

start_v <- as.numeric(sf::st_intersects(start, hex)) end_v <-
as.numeric(sf::st_intersects(end, hex))

lookup[, start_v := as.numeric(st_intersects(start, hex))] lookup[,
end_v := as.numeric(st_intersects(end, hex))]

# find interpolated points nl_int <- function(nl, centroid =
cent){centroid[unlist(shortest_paths(gr, from = nl$start_v, to =
nl$end_v, predecessors = FALSE)$vpath),]}

lookup <- lookup[, nl_int(.SD[,c("start_v", "end_v")], centroid =
cent), by = .(deploy_lat, deploy_long, t_lat, t_lon)] lookup[, obs_num
:= 1:nrow(.SD), by = .(deploy_lat, deploy_long, t_lat, t_lon)] lookup
<- sf::st_transform(sf::st_sf(lookup), crs = 4326) coords <-
as.data.table(sf::st_coordinates(lookup)) lookup <-
sf::st_drop_geometry(lookup) lookup <- cbind(lookup, coords)
setnames(lookup, c("X", "Y"), c("nln_lon", "nln_lat")) lookup[,
num_obs := .N, by = .(deploy_lat, deploy_long, t_lat, t_lon)]

# added first/last rows lookup[lookup[, .I[1], by = .(deploy_lat,
deploy_long, t_lat, t_lon)]$V1, ':=' (nln_lon = deploy_long, nln_lat =
deploy_lat)] lookup[lookup[num_obs > 1, .I[.N], by = .(deploy_lat,
deploy_long, t_lat, t_lon)]$V1, ':=' (nln_lon = deploy_long, nln_lat =
deploy_lat)]

# calculate cumulative distance between each interpolated position for
lat and lon using lead lookup[, ':=' (latitude_lead =
data.table::shift(nln_lat, type = "lag", fill = NA), longitude_lead =
data.table::shift(nln_lon, type = "lag", fill = NA)) , by =
.(deploy_lat, deploy_long, t_lat, t_lon)] lookup[, cumdist :=
geosphere::distGeo(.SD[, c("nln_lon", "nln_lat")],
.SD[,c("longitude_lead", "latitude_lead")]), by = .(deploy_lat,
deploy_long, t_lat, t_lon)][is.na(cumdist), cumdist := 0]

# calculate cumlative distance between each interpolated point
lookup[, cumdist := cumsum(cumdist), by = .(deploy_lat, deploy_long,
t_lat, t_lon)]

# combine lookup with all spatial out <- lookup[nln_small,
.(deploy_lat, deploy_long, bin, t_bin, t_lat, t_lon,
detection_timestamp_utc = i.detection_timestamp_utc, t_timestamp =
i.t_timestamp, animal_id = i.animal_id, obs_num, nln_lon, nln_lat,
num_obs, cumdist), on = .(deploy_lat, deploy_long, t_lat, t_lon)]

out <- out[, no_move := sum(cumdist) == 0, by = .(deploy_lat,
deploy_long, t_lat, t_lon, detection_timestamp_utc,
t_timestamp)][no_move == FALSE][, no_move := NULL]
  
# add timeseries for interpolating nln movements out[out[, .I[1], by =
.(deploy_lat, deploy_long, t_lat, t_lon, detection_timestamp_utc,
t_timestamp)]$V1, i_time := detection_timestamp_utc]

out[out[num_obs > 1, .I[.N], by = .(deploy_lat, deploy_long, t_lat,
        t_lon, detection_timestamp_utc, t_timestamp)]$V1, i_time :=
        t_timestamp]


  # interpolate missing timestamps for interpolated coordinates out[,
i_time := as.POSIXct(approx(cumdist, i_time, xout = cumdist)$y, origin
= "1970-01-01 00:00:00", tz = attr(lookup$i_time, "tzone")), by =
.(deploy_lat, deploy_long, t_lat, t_lon, detection_timestamp_utc,
t_timestamp)]


# lookup values from "out" and interpolate values for bins setkey(out,
deploy_lat, deploy_long, detection_timestamp_utc, t_timestamp,
animal_id) setkey(dtc, deploy_lat, deploy_long,
detection_timestamp_utc, t_timestamp, animal_id) dtc[, longitude :=
{tmp = out[, .SD[, c("i_time", "nln_lon")]]; approx(tmp$i_time,
tmp$nln_lon, xout = bin_timestamp)$y}, by = .(i.start)] dtc[, latitude
:= {tmp = out[, .SD[, c("i_time", "nln_lat")]]; approx(tmp$i_time,
tmp$nln_lat, xout = bin_timestamp)$y}, by = .(i.start)]

final <- dtc[is.na(record_type), c("animal_id", "bin_timestamp",
"latitude", "longitude")] final[, record_type := "interpolated"]

setnames(det, c("bin_stamp", "i_lat", "i_lon"), c("bin_timestamp",
"latitude", "longitude")) out <- data.table(rbindlist(list(final,
det[, c("animal_id", "bin_timestamp", "latitude", "longitude",
"record_type")]))) data.table::setorder(out, animal_id, bin_timestamp,
-record_type)

# carry forward values if fish don't move and are not detected during
each bin.  out[, c("latitude", "longitude") := list(nafill(latitude,
"locf"), nafill(longitude, "locf")), by = "animal_id"]


  # If out_class == NULL, then return data as data.table
  if(is.null(out_class)){ out <- as.data.frame(out) return(out) }

  # if out_class == "tibble", then return tibble object if(out_class
  == "tibble"){ out <- tibble::as_tibble(out) return(out) }

  # if out_class == NULL, then return data.frame object return(out) }
 
