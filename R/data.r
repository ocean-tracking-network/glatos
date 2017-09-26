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
#' data and is used by default in \link{interpolatePath}.
#'
#' @seealso \link{interpolatePath}, \link{gdistance}
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
#' HECSL. This is a sample of detection data in the standard 
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
#' with the functions \link{interpolatePath} and \link{animatePath}.
#' 
#' @seealso \link{interpolatePath}, \link{animatePath}, 
#' \link{walleye_detections}
#' @author Todd Hayden
"walleye_plotControl"


#' @title A SpatialPolygonDataFrame of the world's oceans.
#' 
#' @description A SpatialPolygonDataFrame of the world's oceans. This is 
#' used as a default map background in several 
#' \link{glatos} functions when \code{type = "OTN"}.
#'
#' @usage data(oceanPoly)
#'
#' @source 
#' http//www.naturalearthdata.com/download/110m/physical/ne_110m_ocean.zip
#' 
"oceanPoly"
