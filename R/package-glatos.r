#' glatos: A package for the Great Lakes Acoustic Telemetry Observation System
#'
#' The glatos package provides three categories of important functions:
#' simulation, visualization, and random utility functions.
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
#' @section Visualization functions:
#' The function \code{\link{kmlWorkbook}} is useful for exploring receiver and 
#'   animal release locations in Google Earth. \cr\cr
#'
#' @section Random Utility functions:
#' The functions \code{\link{vrl2csv}}, \code{\link{rotatePoints}},
#'   \code{\link{crw}}, and \code{\link{vectorHeading}} were needed to in
#'   functions in this package but might ve useful for other things too. 
#'
#' @docType package
#' @name glatos
NULL
