#' @title A SpatialPolygonDataFrame with Great Lakes coastline and some major 
#' tributaries.
#' 
#' @description A SpatialPolygonDataFrame with Great Lakes coastline and some 
#' major tributaries. This is used as a default map background in several 
#' \link{glatos} functions.
#'
#' @author Todd Hayden
"greatLakesPoly"


#' @title A TransitionLayer object that only allows transitions to occur within 
#' water of the Great Lakes Basin.
#' 
#' @description A TransitionLayer object that only allows transitions to occur 
#' within water (i.e., prohibits movement onto land). This dataset was 
#' developed for non-linear interpolation of fish movement paths from telemetry 
#' data and is used by default in \link{interpolate_path}.
#'
#' @seealso \link{interpolate_path}, \link{gdistance}
#' @author Todd Hayden 
"greatLakesTrLayer"


#' @title 
#' A data frame containing receiver location data
#' 
#' @description 
#' A data frame containing receiver location data from the GLATOS Project 
#' HECSL. This is a sample of receiver location data in the standard 
#' GLATOS export format.
#'
#' @author Todd Hayden 
"recLoc_example"


#' @title
#' An example control data frame that determine order of location groups on the 
#' y-axis of an \link{abacusPlot} or \link{eventPlot}
#' 
#' @description
#' An example control data frame that determine order of location groups on the 
#' y-axis of an \link{abacusPlot} or \link{eventPlot}. This file is an example 
#' provided by Todd Hayden for use with example data \link{walleye_detections} 
#' with the functions \link{abacusPlot} and \link{eventPlot}.
#' 
#' @seealso \link{abacusPlot}, \link{eventPlot}, \link{walleye_detections}
#' @author Todd Hayden
"walleye_controlTable"


#' @title
#' A data frame containing fish detection data
#' 
#' @description
#' A data frame containing fish detection data from the GLATOS Project 
#' HECWL. This is a sample of detection data in the standard 
#' GLATOS export format.
#'
#' @author Todd Hayden 
"walleye_detections"


#' @title
#' An example plot control data frame that determines the color and marker of 
#' points png and mp4 files created by \link{animatePath} 
#' 
#' @description 
#' An example plot control data frame that determines the color and marker of 
#' points png and mp4 files created by \link{animatePath}. Provided by Todd 
#' Hayden for use with example file \link{walleye_detections}
#' with \link{animatePath}.
#' 
#' @seealso \link{interpolatePath}, \link{animatePath}, 
#' \link{walleye_detections}
#' @author Todd Hayden
"walleye_plotControl"


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

