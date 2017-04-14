#' Sea Lamprey positions from Lake George, St. Marys River, 2012
#' 
#' Sea Lamprey positions from a positional acoustic telemetry array in Lake 
#' George, North Channel of the St. Marys River during the 2012 spawning year.
#' 
#' @details Data were collected as part of the GLATOS project SMRSL 
#'   \url{http://glatos.glos.us/home/project/SMRSL}
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
#'}
#'   
#' @source Chris Holbrook, US Geological Survey (cholbrook@usgs.gov)
#'   
"lamprey_tracks"