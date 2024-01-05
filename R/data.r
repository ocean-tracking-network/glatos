#' @title [Deprecated] A SpatialPolygonDataFrame with Great Lakes coastline and some major
#'   tributaries.
#'
#' @description A SpatialPolygonDataFrame with Great Lakes coastline and some
#'   major tributaries. This is used as a default map background in several
#'   \link{glatos} functions.
#'
#' @details This dataset is deprecated and will be removed in a future version.
#'   Use \code{\link{great_lakes_polygon}} instead.
#'
#' @author Todd Hayden
"greatLakesPoly"


#' @title An sf POLYGON object with Great Lakes coastline and some major 
#' tributaries.
#' 
#' @description Created from [greatLakesPoly()]. This is used as a
#'   default map background in several [glatos] functions.
#'
#' @author Todd Hayden (coerced to sf by C. Holbrook)
"great_lakes_polygon"


#' @title An sf POLYGON object with coastline of Higgins Lake
#' 
#' @description An sf POLYGON object with coastline of Higgins Lake, Michigan. 
#' Used as an example of a polygon representing a water body.
#'
#' @author Chris Holbrook
"higgins_lake_polygon"


#' @title An sf POLYGON object with coastline of Flynn Island
#' 
#' @description An sf POLYGON object with coastline of Flynn Island; and 
#' island within Higgins Lake, Michigan. 
#' Used as an example of a polygon representing a body of land (as opposed to 
#' water body).
#'
#' @author Chris Holbrook
"flynn_island_polygon"


#' @title A TransitionLayer object that only allows transitions to occur within 
#' water of the Great Lakes Basin.
#' 
#' @description A TransitionLayer object that only allows transitions to occur 
#' within water (i.e., prohibits movement onto land). This dataset was 
#' developed for non-linear interpolation of fish movement paths from telemetry 
#' data and is used by default in [interpolate_path].
#'
#' @seealso [interpolate_path], [gdistance]
#' @author Todd Hayden 
"greatLakesTrLayer"


#' Sea Lamprey positions from Lake George, St. Marys River, 2012
#' 
#' Sea Lamprey positions from a positional acoustic telemetry array in Lake 
#' George, North Channel of the St. Marys River during the 2012 spawning year.
#' 
#' @details Data were collected as part of the GLATOS project SMRSL 
#'   <http://glatos.glos.us/home/project/SMRSL>
#'   
#' @details Positions were calculated using the Vemco Positioning System.
#'   
#' @format A data frame with 21043 rows and 14 variables: 
#' \describe{ 
#'   \item{DETECTEDID}{transmitter identifier (channel, frequency, code space, 
#'      and ID code)} 
#'   \item{DATETIME}{position timestamp, in UTC} 
#'   \item{X,Y}{horizontal and vertical position on local grid, in meters} 
#'   \item{D}{assumed depth at time of detection, in meters (NOT from 
#'      depth/pressure sensor)} 
#'   \item{LAT,LON}{position latitude and longitude, decimal degrees (west 
#'      is negative); CRS: WGS84}
#'   \item{n}{?} 
#'   \item{HPE}{horizontal position error; calculated by VEMCO} 
#'   \item{HPEm}{horizontal position error, in meters; calculated by VEMCO} 
#'   \item{TEMP}{temperature at time of detection (from temperature sensor)} 
#'   \item{DEPTH}{depth at time of detection (from pressure sensor)} 
#'   \item{ACCEL}{acceleration at time of detection (from accelerometer)}
#'   \item{DRX}{receivers that detected the associated transmission} 
#'}
#'   
#' @source Chris Holbrook, US Geological Survey (cholbrook@usgs.gov)
#'   
"lamprey_tracks"

#' Detection range data set
#' 
#' Sample detection range data set from Lake Superior.
#'   
#' @details Data from a stationary detection range test conducted in 2018. Data
#'   are in standard GLATOS detection export format and are intened to 
#'   accompany detecton range analysis vignette.
#'   
#' @format A data frame with 58309 rows and 30 variables
#'   
#' @source F. Zomer, T. Hayden
#'   
"range_detection"

