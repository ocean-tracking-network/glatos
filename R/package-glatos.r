#' An R package for the Great Lakes Acoustic Telemetry Observation System
#'
#' \code{glatos} is an R package with functions useful to members of the Great
#' Lakes Acoustic Telemetry Observation System (\url{http://glatos.glos.us}).
#' Functions may be generally useful for processing, analyzing, simulating, and
#' visualizing acoustic telemetry data, but are not strictly limited to acoustic
#' telemetry applications.
#'
#' @section Package status: \emph{This package is in early development and its
#'   content is evolving.} To access the package or contribute code, join the
#'   project at (\url{https://gitlab.oceantrack.org/GreatLakes/glatos}). If you
#'   encounter problems or have questions or suggestions, please post a new
#'   issue or email <cholbrook@usgs.gov> (maintainer: Chris Holbrook).
#'
#' @section Installation: Installation instructions can be found at
#'   \url{https://gitlab.oceantrack.org/GreatLakes/glatos/wikis/installation-instructions}.
#'
#'
#'
#' @section Data loading and processing: \describe{
#'
#'   \item{\link{read_glatos_detections} and \link{read_otn_detections}}{ Fast
#'   data loading from standard GLATOS and OTN data files to a single structure
#'   that is compatible with other glatos functions.}
#'
#'   \item{\link{read_glatos_receivers}}{ Reads receiver location histories from
#'   standard GLATOS data files to a single structure that is compatible with
#'   other glatos functions.}
#'
#'   \item{\link{read_glatos_workbook}}{ Reads project-specific receiver history
#'   and fish taggging and release data from a standard glatos workbook file.}
#'
#'   \item{\link{read_vemco_tag_specs}}{ Reads transmitter (tag) specifications
#'   and operating schedule.}
#'
#'   \item{\link{real_sensor_values}}{ Converts 'raw' transmitter sensor (e.g.,
#'   depth, temperature) to 'real'-scale values (e.g., depth in meters) using
#'   transmitter specification data (e.g., from read\_vemco\_tag\_specs).} }
#'
#' @section Filtering and summarizing: \describe{ 
#'   \item{\link{min_lag}}{
#'   Facilitates identification and removal of false positive detections by
#'   calculating the minimum time interval (min\_lag) between successive
#'   detections.}
#'
#'  \item{\link{detection_filter}}{ Removes potential false positive detections
#'  using "short interval" criteria (see \link{min_lag}).}
#'  
#'  \item{\link{detection_events}}{ Distills detection data down to a much
#'  smaller number of discrete detection events, defined as a change in location
#'  or time gap that exceeds a threshold.}
#'
#'  \item{\link{summarize_detections}}{ Calculates number of fish detected,
#'  number of detections, first and last detection timestamps, and/or mean
#'  location of receivers or groups, depending on specific type of summary
#'  requested.}
#' }
#' 
#' @section Visualization and data exploration: \describe{
#' \item{\link{abacus_plot}}{ Useful for exploring movement patterns of
#' individual tagged animals through time.}
#' 
#' \item{\link{detection_bubble_plot}}{ Useful for exploring distribution of
#' tagged individuals among receivers.}
#' 
#' \item{\link{interpolate_path}, \link{make_frames}, and \link{make_video}}{
#' Interpolate spatio-temporal movements, between detections, create video
#' frames, and stitch frames together to create animated video file using FFmpeg
#' software.}
#' 
#' \item{\link{adjust_playback_time}}{ Modify playback speed of videos and
#' optionally convert between video file formats. Requires FFmpeg.}
#' }
#'
#' @section Simulation functions for system design and evaluation: \describe{
#' \item{\link{calc_collision_prob}}{ Estimates the probability of collisions
#' for pulse-position-modulation type co-located telemetry transmitters. This is
#' useful for determining the number of fish to release or tag specifications
#' (e.g., delay).}
#'
#' \item{\link{receiver_line_det_sim}}{ Simulates detection of acoustic-tagged
#' fish crossing a receiver line (or single receiver). This is useful for
#' determining optimal spacing of receviers in a line and tag specifications
#' (e.g., delay).}
#'
#' \item{\link{crw_in_polygon}, \link{transmit_along_path}, and
#' \link{detect_transmissions}}{ Individually simulate random fish movement paths
#' within a water body (\link{crw_in_polygon}: a random walk in a polygon), tag
#' signal transmissions along those paths (\link{transmit_along_path}: time series
#' and locations of transmissions based on tag specs), and detection of those
#' transmittions by receivers in a user-defined receiver network
#' (\link{detect_transmissions}: time series and locations of detections based on
#' detection range curve). Collectively, these functions can be used to explore,
#' compare, and contrast theoretical performance of a wide range of transmitter
#' and receiver network designs.}
#' }
#'
#' @docType package
#' @name glatos
#' @import data.table sp
#' @importFrom graphics abline axis box legend lines mtext par points symbols
#'   text
#' @importFrom stats approx dnorm ecdf end fivenum median na.omit rbinom rnorm
#'   runif start step
#' @importFrom utils capture.output read.csv setTxtProgressBar txtProgressBar
#'   unzip write.csv write.table zip
#' @importFrom grDevices bmp colorRampPalette dev.new dev.off jpeg png rainbow
#'   tiff
globalVariables(".") #to avoid R CMD check note
