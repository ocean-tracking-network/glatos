#' glatos: A package for the Great Lakes Acoustic Telemetry Observation System
#'
#' The glatos package provides three categories of important functions:
#' simulation, processing and summarization, visualization and exploration, and random utility functions.
#' 
#' @section Simulation functions:
#' The function \code{\link{calcCollisionProb}} is useful for determing the
#'   number of fish to release or tag specifications (e.g., delay).\cr\cr
#' The function \code{\link{receiverLineDetSim}} is useful for determining
#'   optimal spacing of receviers in a line. \cr\cr
#' The functios \code{\link{crwInPolygon}}, \code{\link{transmitAlongPath}}, and 
#'   \code{\link{detectTransmissions}} can be used to explore theoretical 
#'   performance of custom receiver network arrangements. \cr\cr
#'
#' @section Processing and Summarization functions:
#' The function \code{\link{falseDetectionFilter}} identifies potential 
#'   false detections using the GLATOS min_lag column. \cr\cr
#' The function \code{\link{detectionEventFilter}} distills detection data
#'   down to a much smaller number of discrete detection events. \cr\cr
#'
#' @section Visualization and Exploration functions:
#' The function \code{\link{kmlWorkbook}} is useful for exploring receiver and 
#'   animal release locations in Google Earth. \cr\cr
#' The functions \code{\link{abacusPlot}} and \code{\link{detectionEventPlot}} 
#'   are useful for exploring movement patterns of individual tagged animals.
#' \cr\cr
#' The function \code{\link{detectionBubblePlot}} is useful for exploring  
#'   distribution of tagged individuals among receivers. \cr\cr
#'
#' @section Random Utility functions:
#' The functions \code{\link{vrl2csv}}, \code{\link{rotatePoints}},
#'   \code{\link{crw}}, and \code{\link{vectorHeading}} were needed to in
#'   functions in this package but might ve useful for other things too. 
#'
#' @docType package
#' @name glatos
NULL
