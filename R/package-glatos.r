#' An R package for the Great Lakes Acoustic Telemetry Observation System
#' 
#' \code{glatos} is an R package with functions useful to members of the 
#' Great Lakes Acoustic Telemetry Observation System 
#' ([http://glatos.glos.us](http://glatos.glos.us)). Functions may be 
#' generally useful for processing, analyzing, simulating, and visualizing 
#' acoustic telemetry data, but are not strictly limited to acoustic telemetry
#' applications.
#'
#' @section Package status:
#' This package is in early development. To access the package or contribute 
#' code, join the project at \url{
#' https://gitlab.oceantrack.org/GreatLakes/glatos}. If you encounter 
#' problems or have questions or suggestions, please post a new issue or email 
#' cholbrook@usgs.gov (maintainer: Chris Holbrook).
#'
#' @section Simulation functions for system design and evaluation:
#' \describe{\item{\link{calcCollisionProb}}{estimates the probability of 
#' collisions for pulse-position-modulation type co-located telemetry 
#' transmitters. This is useful for determining the number of fish to 
#' release or tag specifications (e.g., delay).}
#' 
#' \item{\link{receiverLineDetSim}}{simulates detection of acoustic-tagged 
#' fish crossing a receiver line (or single receiver). This is useful 
#' for determining optimal spacing of receviers in a line and tag 
#' specifications (e.g., delay).}
#' 
#' \item{\link{crwInPolygon}, \link{transmitAlongPath}, 
#' and \link{detectTransmissions}}{individually simulate random fish movement 
#' paths within a water body (\link{crwInPolygonR}: a random walk in a 
#' polygon), tag signal transmissions along those paths 
#' (\link{transmitAlongPath}: time series and locations of transmissions 
#' based on tag specs), and detection of those transmittions by receivers 
#' in a user-defined receiver network (\link{detectTransmissions}: time 
#' series and locations of detections based on detection range curve). 
#' Collectively, these functions can be used to explore, compare, and 
#' contrast theoretical performance of a wide range of transmitter and 
#' receiver network designs.}} 
#'
#' @section Data processing and summarization:
#' \describe{\item{\link{falseDetectionFilter}}{identifies potential false 
#' detections in the GLATOS standard data export package using "short interval"
#'  criteria (GLATOS min_lag column).}
#' \item{\link{detectionEventFilter}}{distills detection data down to a much 
#' smaller number of discrete detection events, defined as a change in 
#' location (defined by user) or time gap that exceeds a threshold 
#' (defined by user).}}
#'
#' @section Visualization and data exploration:
#' \describe{\item{\link{kmlWorkbook}}{is useful for exploring receiver and 
#'   animal release locations in Google Earth.}
#' \item{\link{abacusPlot} and \link{detectionEventPlot}}{ 
#'   are useful for exploring movement patterns of individual tagged animals.}
#' \item{\link{detectionBubblePlot}}{is useful for exploring  
#'   distribution of tagged individuals among receivers.}
#' \item{\link{interpolate_path} and \link{make_frames}}{can
#' be used together to interpolate movement paths between detections and 
#' save animated movement paths to a video file (mp4).}}
#'
#' @section Random Utility functions:
#' \describe{\item{\link{vrl2csv}}{converts a Vemco VRL file to a comma 
#'   separated values (CSV) file using a system call to VEMCO VUE 
#'   \code{convert} command.}
#' \item{\link{rotatePoints}}{will rotate a set of 2-d points about 
#'   another point.}
#' \item{\link{crw}}{will simulate an unconstrained correlated random walk.}
#' \item{\link{vectorHeading}}{will calculate (in degrees) the heading of the 
#'   vector between adjacent point-pairs in a set of positions (e.g., along 
#'   a track).}}
#' 
#' @section Installation:
#' The R package GLATOS is available from the Ocean Tracking Network's 
#' gitlab (\url{https://gitlab.oceantrack.org/GreatLakes/glatos}).\cr\cr
#' To install:\cr\cr
#' 1. install devtoools for R (if you haven't already)\cr
#' > \code{install.packages("devtools")}\cr\cr
#' 2. download the package and install \cr
#' > \code{library(devtools)}\cr
#' > \code{install_git("https://gitlab.oceantrack.org/GreatLakes/glatos.git")}
#'
#' @docType package
#' @name glatos
#' @import data.table sp
globalVariables(".")
