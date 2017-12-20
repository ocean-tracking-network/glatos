#' Interpolate new positions within a spatiotemporal path data 
#'
#' Interpolate new positions within a spatiotemporal path data set 
#'   (e.g., detections of tagged fish) at regularly-spaced time intervals   
#' 	 using linear or non-linear interpolation (via \code{\link{movePath}}).
#' 
#' @param dtc A data frame containing spatiotemporal data with at least 
#'   4 columns containing 'individual', 'timestamp','longitude', and 'latitude' 
#'   data and an optional fifth column with the 'type' of record 
#'   (e.g., detection). Default column names match the GLATOS 
#'   detection export file but other names can be specified with 
#'   \code{detColNames}.
#'   
#' @param intTimeStamp The time step size (in seconds) of interpolated 
#'   positions. Default is 86400 (one day).
#'   
#' @param rast An optional transition matrix with the "cost" of moving across 
#'   each cell within the map extent. Must be of class 
#'   \code{TransitionLayer} (See \code{gdistance} package). Passed to  
#'   \code{trans} in \code{\link{movePath}}. 
#'   
#' @param lnlThresh A numeric threshold for determining if linear or non-linear 
#'   interpolation will be used based on the ratio of linear-to-non-linear
#'   shortest path distances. Passed to \code{ithresh} in 
#'   \code{\link{movePath}}.
#'   
#' @param detColNames A list with names of columns in \code{dtc}:
#' \itemize{
#'   \item \code{individualCol} is a character string that uniquely identifies 
#'     an individual (e.g., tagged animal). Default is 'animal_id'.
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps. Default is 'detection_timestamp_utc'.
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude data. Default is 'deploy_lat'.
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver. Default is 'deploy_long'.
#'	 \item \code{typeCol} is a character string with the name of the optional 
#'     column that identifies the type of record. Default is 'record_type'.} 
#'
#' @details 
#' Interpolation is done by passing each consecutive pair of points to 
#' \code{\link{movePath}} for interpolation via linear or non-linear 
#' methods, depending on \code{rast}.
#' 
#' @details 
#' Non-linear interpolation uses the 'gdistance' package to find the shortest 
#' pathway between two locations (i.e., receivers) that avoid 'impossible' 
#' movements (e.g., over land for fish). The shortest non-linear path between 
#' two locations is calculated using a 'transition matrix layer' (\code{rast}) 
#' that represents the 'cost' of an animal moving between adjacent grid cells. 
#' For example, each cell in \code{rast} may be coded as water (1) or land (0)
#' to represent possible (1) and impossible (0) movement path. 
#' 
#' @details
#' Linear interpolation is used for all points when \code{rast} is not supplied.
#' When \code{rast} is supplied, then interpolation method is determined for
#' each pair of observed positions. For example, linear interpolation will be 
#' used if the two points are exactly the same and when the ratio of linear-to-
#  non-linear shortest path distances exceeds \code{lnlThresh}. \code{lnlThresh} 
#' can be used to control whether non-linear or linear interpolation is used
#' for all points. For example, non-linear interpolation will be used for all
#' points when \code{lnlThresh} = 1 and linear interpolation will be used for 
#' all points when \code{lnlThresh} = 0.        
#'
#' @return A dataframe with id, timestamp, lat, lon, and record type.
#'
#' @seealso \code{\link{movePath}}
#'
#' @author Todd Hayden
#' 
#' @examples
#'
#' --------------------------------------------------
#' #EXAMPLE #1 - simple example
#'
#' #example transition matrix
#' data(greatLakesTrLayer)
#'  
#' #example map background
#' data(greatLakesPoly)
#' library(sp) #to plot SpatialPolygon without error
#' plot(greatLakesPoly)
#' 
#' #make up points
#' pos <- data.frame(
#'   id=1,
#'   x=c(-87,-82.5, -78),
#'   y=c(44, 44.5, 43.5),
#'   time=as.POSIXct(c("2000-01-01 00:00",
#'     "2000-02-01 00:00", "2000-03-01 00:00")))
#'
#' #coerce to SpatialPoints object and plot
#' pts <- SpatialPoints(pos[,c("x","y")])
#' points(pts, pch=20, col='red', cex=3)
#'
#' #interpolate path using linear method
#' path1 <- interpolatePath(pos, 
#'   detColNames=list(individualCol="id", timestampCol="time", 
#' 		 longitudeCol="x", latitudeCol="y"))
#' 
#' #coerce to SpatialPoints object and plot
#' pts1 <- SpatialPoints(path1[,c("x","y")])
#' points(pts1, pch=20, col='blue', lwd=2, cex=1.5) 
#'
#' #example transition matrix
#' data(greatLakesTrLayer)
#' 
#' #interpolate path using non-linear method (requires 'trans')
#' path2 <- interpolatePath(pos, 
#'   rast=greatLakesTrLayer,
#'   detColNames=list(individualCol="id", timestampCol="time",
#'   longitudeCol="x", latitudeCol="y"))
#'
#' #coerce to SpatialPoints object and plot
#' pts2 <- SpatialPoints(path2[,c("x","y")])
#' points(pts2, pch=20, col='green', lwd=2, cex=1.5) 
#' 
#' #can also force linear-interpolation with lnlThresh=0
#' path3 <- interpolatePath(pos, 
#'   rast=greatLakesTrLayer, lnlThresh=0,
#'   detColNames=list(individualCol="id", timestampCol="time",
#'   longitudeCol="x", latitudeCol="y"))
#'
#' #coerce to SpatialPoints object and plot
#' pts3 <- SpatialPoints(path3[,c("x","y")])
#' points(pts3, pch=20, col='magenta', lwd=2, cex=1.5) 
#'
#' --------------------------------------------------
#' #EXAMPLE #2 - GLATOS detection data
#' data(walleye_detections) 
#' head(walleye_detections)
#' 
#' #call with defaults; linear interpolation
#' pos1 <- interpolatePath(walleye_detections)
#' 
#' #plot on example map background
#' data(greatLakesPoly)
#' library(sp) #to plot SpatialPolygon without error
#' plot(greatLakesPoly)
#'
#' #coerce to SpatialPoints object and plot
#' pts1 <- SpatialPoints(pos1[pos1$animal_id==3,c("deploy_long","deploy_lat")])
#' points(pts1, pch=20, col='red', cex=0.5)
#' 
#' #example transition matrix
#' data(greatLakesTrLayer)
#' 
#' #call with "transition matrix" (non-linear interpolation), other options
#' # note that it is quite a bit slower due than linear interpolation
#' pos2 <- interpolatePath(walleye_detections, rast=greatLakesTrLayer)
#'
#' #coerce to SpatialPoints object and plot
#' pts2 <- SpatialPoints(pos2[,c("deploy_long","deploy_lat")])
#' points(pts2, pch=20, col='blue', cex=0.5)
#'
#' @export 

## interpolatePath <- function(dtc, intTimeStamp=86400, rast=NULL, lnlThresh=0.9,  
## 	detColNames=list(individualCol="animal_id", 
## 		timestampCol="detection_timestamp_utc", latitudeCol="deploy_lat", 
## 		longitudeCol="deploy_long", typeCol="record_type")){

# major overhaul of "interpolatePath" function to inprove speed and
# stability.

x <- data.table(x = c(1,2,3), z = c(1,2,3))
tst(x)

    tst <- function(x){ x[, y := 1]
  return(x)
}

##########################################
# load packages and data for development
library(gdistance)
library(glatos)
library(data.table)

data(walleye_detections) 
data(greatLakesTrLayer)

 dtc = walleye_detections
 trans = greatLakesTrLayer
 intTimeStamp = 86400
 rast = greatLakesTrLayer
 lnlThresh = 0.9
setDT(dtc)

# subset only columns needed and add column for type of detection
# this is the required input for function. "animal_id", "detection_timestamp_utc", "deploy_lat",
# "deploy_long", and type of detection ("type" = real)
dtc <- dtc[, c("animal_id", "detection_timestamp_utc", "deploy_lat", "deploy_long")][, type := "real"]

## out <- interpolatePath2(dtc = walleye_detections, trans = greatLakesTrLayer, intTimeStamp = 86400/2, rast = greatLakesTrLayer, lnlThresh = 0.9)

#library(data.table)
#ptm <- proc.time()

#ptm <- proc.time() - ptm

out <- interpolatePath2(dtc = dtc, trans = greatLakesTrLayer, intTimeStamp = 86400,
                        lnlThresh = 0.9) 

#proc.time() - ptm



interpolatePath2 <- function(dtc, trans, intTimeStamp, lnlThresh){

# this function uses data.table extensively
setDT(dtc)

# Sort detections by transmitter id and then by detection timestamp
setkey(dtc, animal_id, detection_timestamp_utc)
 
# save original dataset to combine with interpolated data in the end
det <- dtc
names(det) <- c("animal_id", "bin_stamp", "i_lat", "i_lon", "type")

# create sequence of timestamps based on min/max timestamps in data
rng <- as.POSIXct(trunc(range(dtc$detection_timestamp_utc), units = 'days'), tz = 'GMT')
tSeq <- seq(rng[1], rng[2], intTimeStamp)

# bin data by time interval and add bin to dtc
dtc[, bin := tSeq[findInterval(detection_timestamp_utc, tSeq)] ]

# make all combinations of animals and detection bins
dtc <- merge(CJ(bin = tSeq, animal_id = unique(dtc$animal_id)), dtc, by = c("bin", "animal_id"), all.x = TRUE)
setkey(dtc, animal_id, bin, detection_timestamp_utc)

# identify start and end rows for observations before and after NA
ends <- dtc[!is.na(deploy_lat), .(start = .I[-nrow(.SD)], end = .I[-1]), by = animal_id][end - start > 1]

# identify observations that are both start and ends
dups <- start_end[ ends[, duplicated(c(start, end))] ]

# create and append duplicate rows for observations
# that are both start and end.
# This is so each observation can be in only one group

# identifies rows and duplicate rows that need duplicated
dtc[, c("rep", "num") := list(1L, 1:.N)][dups, rep := 2L]
dtc <- dtc[rep(num, rep)]
dtc[, rep := NULL]
dtc[, num := NULL]

########################

#recalculate first and last rows- no duplicate rows this time...

new_ends <- dtc[!is.na(deploy_lat), .(start = .I[-nrow(.SD)], end = .I[-1]), by = animal_id][end - start > 1]

# create row index needed for overlap join
dtc[, c("start", "end") := list(1:.N, 1:.N)]
setkey(new_ends, start, end)
setkey(dtc, start, end)

# extract rows that need interpolation
dtc <- foverlaps(new_ends[,-1], dtc, by.x = c("start", "end"), by.y = c("start", "end"))

#dtc <- dtc[!is.na(nrow)]
setkey(dtc, animal_id, bin, detection_timestamp_utc)

# Only need to calculate these for rows in which we need interpolation.  This can be moved down...
# calculate great circle distance between coords
#ptm <- proc.time()
dtc[, gcd := geosphere::distHaversine(as.matrix(.SD[1, c("deploy_long", "deploy_lat")]), as.matrix(.SD[.N, c("deploy_long", "deploy_lat")])), by = i.start]
#proc.time() - ptm

# calculate least cost (non-linear) distance between points
dtc[, lcd := costDistance(trans, fromCoords = as.matrix(.SD[1, c("deploy_long", "deploy_lat")]), toCoords = as.matrix(.SD[.N, c("deploy_long", "deploy_lat")])), by = i.start]

# calculate ratio of gcd:lcd
dtc[, crit := gcd/lcd]

# create keys for lookup
dtc[!is.na(detection_timestamp_utc), t_lat := data.table::shift(deploy_lat, type = "lead"), by = i.start]
dtc[!is.na(detection_timestamp_utc), t_lon := data.table::shift(deploy_long, type = "lead"), by = i.start]
dtc[!is.na(detection_timestamp_utc), t_timestamp := data.table::shift(detection_timestamp_utc, type = "lead"), by = i.start]

# extract rows that need non-linear interpolation based on ratio between gcd:lcd
nln <- dtc[crit < lnlThresh & !is.infinite(crit) & crit != 0]

# extract data for linear interpolation
# had to add "crit == 0" because lcd was unable to be calculated for one movement (nrow = 86).
# this was because movement was outside bounds of transition layer.  A check to make sure that all
# points to be interpolated are within the tranition layer is needed prior to interpolation.

ln <- dtc[crit >= lnlThresh | is.infinite(crit) | is.na(crit) | crit == 0]

ln[, bin_stamp := detection_timestamp_utc][is.na(detection_timestamp_utc), bin_stamp := bin]
ln[, i_lat := {tmp = .SD[c(1, .N), c("detection_timestamp_utc", "deploy_lat")]; approx(c(tmp$detection_timestamp_utc), c(tmp$deploy_lat), xout = c(bin_stamp))$y}, by = i.start]
ln[, i_lon := {tmp = .SD[c(1, .N), c("detection_timestamp_utc", "deploy_long")]; approx(c(tmp$detection_timestamp_utc), c(tmp$deploy_long), xout = c(bin_stamp))$y}, by = i.start]

# extract records to lookup
nln_small <- nln[ !is.na(detection_timestamp_utc)][!is.na(t_lat)]

#######################

# create lookup table
setkey(nln_small, deploy_lat, deploy_long, t_lat, t_lon)
lookup <- unique(nln_small[, .(deploy_lat, deploy_long, t_lat, t_lon),
                     allow.cartesian = TRUE])

# calculate non-linear interpolation for all unique movements in lookup table
lookup[, coord := sp::coordinates(gdistance::shortestPath(trans, as.matrix(.SD[1, c("deploy_long", "deploy_lat")]), as.matrix(.SD[1, c("t_lon", "t_lat")]), output = "SpatialLines")), by = 1:nrow(lookup)]
lookup[, grp := 1:.N]

# extract interpolated points from coordinate lists...
res <- lookup[, .(nln_longitude = lookup$coord[[.I]][, 1], nln_latitude = lookup$coord[[.I]][, 2]), by = grp]

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
nln_small[nln_small[, .I[1], by = i.start]$V1, iTime := detection_timestamp_utc]
nln_small[nln_small[, .I[.N], by = i.start]$V1, iTime := t_timestamp]

# calculate cumdist
nln_small[, cumdist := cumsum(c(0, sqrt(diff(nln_longitude)^2 + diff(nln_latitude)^2))), by = i.start]

# interpolate missing timestamps for interpolated coordinates
nln_small[, iTime := as.POSIXct(approx(cumdist, iTime, xout = cumdist)$y,
                                 origin = "1970-01-01 00:00:00",
                                 tz = attr(nln_small$iTime, "tzone")), by = i.start]

# create timestamp vector to interpolate on.
nln[, bin_stamp := detection_timestamp_utc]
nln[is.na(detection_timestamp_utc), bin_stamp := bin] 
nln[, grp := i.start]

# interpolate timestamps
setkey(nln_small, i.start)
setkey(nln, i.start)
nln[, i_lat := {tmp = nln_small[.(.SD[1, "i.start"]), c("iTime", "nln_latitude")];
    approx(tmp$iTime, tmp$nln_latitude, xout = bin_stamp)$y}, by = grp]

nln[, i_lon := {tmp = nln_small[.(.SD[1, "i.start"]), c("iTime", "nln_longitude")];
    approx(tmp$iTime, tmp$nln_longitude, xout = bin_stamp)$y}, by = grp]

#setkey(nln, nrow, bin_stamp)

det <- rbind(ln[is.na(deploy_long), c("animal_id", "bin_stamp", "i_lat", "i_lon", "type")],
             nln[is.na(type), c("animal_id", "bin_stamp", "i_lat", "i_lon", "type")], det)

det[is.na(type), type := "inter"]

return(det)
}

