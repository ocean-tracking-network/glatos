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

# load packages and data for development
library(gdistance)
library(glatos)
library(data.table)

data(walleye_detections) 
data(greatLakesTrLayer)
trans <- greatLakesTrLayer
dtc <- walleye_detections
intTimeStamp <- 86400/2
rast <- greatLakesTrLayer
lnlThresh = 0.9

# this script uses data.table extensively
setDT(dtc)

# subset only columns needed
dtc <- dtc[,c("animal_id", "detection_timestamp_utc", "glatos_array", "station_no", "deploy_lat", "deploy_long")]

# Sort detections by transmitter id and then by detection timestamp
setkey(dtc, animal_id, detection_timestamp_utc)

# create sequence of timestamps based on min/max timestamps in data
rng <- as.POSIXct(trunc(range(dtc$detection_timestamp_utc), units = 'days'), tz = 'GMT')
tSeq <- seq(rng[1], rng[2], intTimeStamp)

# create vector of individual ids
ids <- unique(dtc$animal_id)

# bin data by time interval and add bin to dtc
dtc[, bin := tSeq[findInterval(detection_timestamp_utc, tSeq)] ]

# make all combinations of animals and detection bins
dtc <- merge(CJ(bin = tSeq, animal_id = ids), dtc, by = c("bin", "animal_id"), all.x = TRUE)
setkey(dtc, animal_id, bin, detection_timestamp_utc)

# identify start and end rows for observations before and after NA
ends <- dtc[!is.na(deploy_lat), .(start =.I[-nrow(.SD)], end =.I[-1]), by = animal_id][end-start>1] #original

# check out using a foverlap join to extract all rows between start/end rows

# identify observations that are both start and ends
start_end <- ends[, c(start, end)]
dups <- start_end[ends[, duplicated(c(start, end))]]

# create and append duplicate rows for observations
# that are both start and end.
# This is so each observation can be in only one group
dtc[, rep := 1L][dups, rep := 2L][, num := 1:.N][rep(num, rep)]
dtc[, rep := NULL]

new <- ends[, .(row_idx = start:end), by = 1:nrow(ends)]
setkey(dtc, num)
setkey(new, row_idx)
dtc <- new[dtc]
dtc <- dtc[!is.na(nrow)]
setkey(dtc, animal_id, bin, detection_timestamp_utc)

#write.csv(dtc[animal_id == 3], "check.csv")

dtc[, gcd := geosphere::distHaversine(as.matrix(.SD[1, c("deploy_long", "deploy_lat")]), as.matrix(.SD[.N, c("deploy_long", "deploy_lat")])), by = nrow ]

# calculate least cost (non-linear) distance between points
dtc[, lcd := costDistance(trans, fromCoords = as.matrix(.SD[1, c("deploy_long", "deploy_lat")]), toCoords = as.matrix(.SD[.N, c("deploy_long", "deploy_lat")])), by = nrow]

# calculate ratio of gcd:lcd
dtc[, crit := gcd/lcd]

# extract rows that need non-linear interpolation based on ratio between gcd:lcd
nln <- dtc[crit >= lnlThresh & crit != Inf]

# create keys for lookup
nln[!is.na(detection_timestamp_utc), t_lat := shift(deploy_lat, type = "lead"), by = nrow]
nln[!is.na(detection_timestamp_utc), t_lon := shift(deploy_long, type = "lead"), by = nrow]
nln[!is.na(detection_timestamp_utc), t_timestamp := shift(detection_timestamp_utc, type = "lead"), by = nrow]

# extract records to lookup
nln_small <- nln[ !is.na(detection_timestamp_utc)][!is.na(t_lat)]

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
lookup[,coord := NULL]

# added first/last rows, number sequence for groups
lookup[lookup[, .I[1], by = grp]$V1, nln_longitude := deploy_long]
lookup[lookup[, .I[.N], by = grp]$V1, nln_longitude := t_lon]
lookup[lookup[, .I[1], by = grp]$V1, nln_latitude := deploy_lat]
lookup[lookup[, .I[.N], by = grp]$V1, nln_latitude := t_lat]
lookup[,seq_count := 1:.N, by = grp]

# combine lookup with original dataset
setkey(lookup, deploy_lat, deploy_long, t_lat, t_lon)
nln_small <- lookup[nln_small, allow.cartesian = TRUE]
setkey(nln_small, nrow, seq_count)

# add timeseries for interpolating nln movements
nln_small[nln_small[, .I[1], by = nrow]$V1, iTime := detection_timestamp_utc]
nln_small[nln_small[, .I[.N], by = nrow]$V1, iTime := t_timestamp]
nln_small[nln_small[, .I[c(-1, -.N)], by = nrow]$V1, iTime := NA]

# calculate cumdist
nln_small[, cumdist := cumsum(c(0, sqrt(diff(nln_longitude)^2 + diff(nln_latitude)^2))), by = nrow]

# interpolate missing timestamps for interpolated coordinates
nln_small[, i_time := as.POSIXct(approx(cumdist, iTime, xout = cumdist)$y, origin = "1970-01-01 00:00:00",
                                tz = attr(nln_small$iTime, "tzone")), by = nrow]

nln[, bin_stamp := detection_timestamp_utc]
nln[is.na(detection_timestamp_utc), bin_stamp := bin] 

################################

# extract "bin_stamp" from nln and use to calculate
# use nrow as key...


nln_small[, c(approx(i_time, nln_longitude, xout = c(tSeq[between(tSeq, detection_timestamp_utc[.I], t_timestamp[.I])]))), by = nrow]

setkey(nln_small, nrow)
#nln[, nln_long := print({tmp = nln[, nln_small[.(nln[1,"nrow"]), c("i_time", "nln_longitude")]]}), by = nrow]#;
#  approx(tmp$i_time, tmp$nln_longitude, xout = nln_tst$bin_stamp)$y}, by = nrow]


nln[, print(match(nln_small$nrow, nln$nrow)), by = nrow]#;

nln[, print(match(nln$nrow, nln_small)), by = nrow]

###############match example...
require(data.table)
a <- data.table(id=1:10,date=as.Date(1:10))
setkey(a,id)
b <- data.table(id=4:6)
setkey(b,id)

a[b]
#match example....


nln_small[nln]



                        , by = nrow]








                 nln_small[.(nln_tst[1, "nrow"]), "nln_longitude"], by = nrow]

                c(nln_small[.(.SD[1, "nrow"]), "nln_longitude"]), xout = c(nln_tst$bin_stamp))$y]
            
is.vector(c(nln_small[.(nln_tst$nrow[1]), "i_time"]))

nln_small_tst <- nln_small[nrow == 2] 

tSeq


nln_small[, num_rows := .N, by = nrow]



write.csv(nln, "check1.csv")
                       

#interpolate x and y locations based on timestamps
pathLon <- approx(out, path$x, xout = iTime)$y
pathLat <- approx(out, path$y, xout = iTime)$y

########


# calculate bins between timestamp & t_timestamp for each nrow, then calculate interpolated x and y...









nln_tst <- nln[nrow == 2]
setkey(nln_tst, deploy_lat, deploy_long, t_lat, t_lon)    
setkey(nln_small, deploy_lat, deploy_long, t_lat, t_lon)

out <- nln_tst[nln_small]
out <- nln_small[nln_tst]





write.csv(out, "check2.csv")


saveRDS(out, "out.rds")
out <- readRDS("out.rds")











