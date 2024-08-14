#' An R package for the Great Lakes Acoustic Telemetry Observation System
#'
#' `glatos` is an R package with functions useful to members of the Great
#' Lakes Acoustic Telemetry Observation System (<https://glatos.glos.us>).
#' Functions may be generally useful for processing, analyzing, simulating, and
#' visualizing acoustic telemetry data, but are not strictly limited to acoustic
#' telemetry applications.
#'
#' @section Package status: *This package is in early development and its
#'   content is evolving.* To access the package or contribute code, join the
#'   project at (<https://github.com/ocean-tracking-network/glatos>). If you
#'   encounter problems or have questions or suggestions, please post a new
#'   issue or email <cholbrook@usgs.gov> (maintainer: Chris Holbrook).
#'
#' @section Installation: Installation instructions can be found at
#'   <https://github.com/ocean-tracking-network/glatos/wiki/installation-instructions>.
#'
#'
#'
#' @section Data loading and processing: \describe{
#'
#'   \item{[read_glatos_detections] and [read_otn_detections]}{ Fast
#'   data loading from standard GLATOS and OTN data files to a single structure
#'   that is compatible with other glatos functions.}
#'
#'   \item{[read_glatos_receivers] and [read_otn_deployments]}{
#'   Reads receiver location histories from standard GLATOS and OTN data files
#'   to a single structure that is compatible with other glatos functions.}
#'
#'   \item{[read_glatos_workbook]}{ Reads project-specific receiver history
#'   and fish taggging and release data from a standard glatos workbook file.}
#'
#'   \item{[read_vemco_tag_specs]}{ Reads transmitter (tag) specifications
#'   and operating schedule.}
#'
#'   \item{[real_sensor_values]}{ Converts 'raw' transmitter sensor (e.g.,
#'   depth, temperature) to 'real'-scale values (e.g., depth in meters) using
#'   transmitter specification data (e.g., from [read_vemco_tag_specs]).} }
#'
#' @section Filtering and summarizing: \describe{
#'   \item{[min_lag]}{
#'   Facilitates identification and removal of false positive detections by
#'   calculating the minimum time interval (min_lag) between successive
#'   detections.}
#'
#'  \item{[false_detections]}{ Removes potential false positive detections
#'  using "short interval" criteria (see [min_lag]).}
#'
#'  \item{[detection_events]}{ Distills detection data down to a much
#'  smaller number of discrete detection events, defined as a change in location
#'  or time gap that exceeds a threshold.}
#'
#'  \item{[summarize_detections]}{ Calculates number of fish detected,
#'  number of detections, first and last detection timestamps, and/or mean
#'  location of receivers or groups, depending on specific type of summary
#'  requested.}
#'
#'  \item{[residence_index]}{ calculates the relative proportion of time
#'  spent at each location.}
#'
#'  \item{[REI]}{ calculates the relative activity at each receiver based
#'  on number of unique species and individual animals.}
#' }
#'
#' @section Visualization and data exploration: \describe{
#' \item{[abacus_plot]}{ Useful for exploring movement patterns of
#' individual tagged animals through time.}
#'
#' \item{[detection_bubble_plot]}{ Useful for exploring distribution of
#' tagged individuals among receivers.}
#'
#' \item{[interpolate_path], [make_frames], and [make_video]}{
#' Interpolate spatio-temporal movements, between detections, create video
#' frames, and stitch frames together to create animated video file using FFmpeg
#' software.}
#'
#' \item{[adjust_playback_time]}{ Modify playback speed of videos and
#' optionally convert between video file formats. Requires FFmpeg.}
#' }
#'
#' @section Simulation functions for system design and evaluation: \describe{
#' \item{[calc_collision_prob]}{ Estimates the probability of collisions
#' for pulse-position-modulation type co-located telemetry transmitters. This is
#' useful for determining the number of fish to release or tag specifications
#' (e.g., delay).}
#'
#' \item{[receiver_line_det_sim]}{ Simulates detection of acoustic-tagged
#' fish crossing a receiver line (or single receiver). This is useful for
#' determining optimal spacing of receviers in a line and tag specifications
#' (e.g., delay).}
#'
#' \item{[crw_in_polygon], [transmit_along_path], and
#' [detect_transmissions]}{ Individually simulate random fish movement paths
#' within a water body ([crw_in_polygon]: a random walk in a polygon), tag
#' signal transmissions along those paths ([transmit_along_path]: time series
#' and locations of transmissions based on tag specs), and detection of those
#' transmittions by receivers in a user-defined receiver network
#' ([detect_transmissions]: time series and locations of detections based on
#' detection range curve). Collectively, these functions can be used to explore,
#' compare, and contrast theoretical performance of a wide range of transmitter
#' and receiver network designs.}
#' }
#'
#' @section Convert glatos data objects to other package classes: \describe{
#' \item{[convert_glatos_to_att]}{ Converts glatos_detections and
#' glatos_receiver objects to ATT for compatibility with the Animal Tracking
#' Toolbox(https://github.com/vinayudyawer/ATT) and the VTrack package.}
#'
#' \item{[convert_otn_erddap_to_att]}{ Converts glatos_detections and
#' transmitter, receiver, and animal metadata from the OTN ERDDAP to ATT format
#' for compatibility with the Animal Tracking
#' Toolbox(https://github.com/vinayudyawer/ATT) and the VTrack package.}
#' }

#' @docType package
#' @name glatos
#' @import data.table sp
#' @importFrom graphics abline axis box legend lines mtext par points symbols
#'   text
#' @importFrom stats approx dnorm ecdf end fivenum median na.omit rbinom rnorm
#'   runif start step
#' @importFrom utils capture.output read.csv packageVersion setTxtProgressBar
#'   txtProgressBar unzip write.csv write.table zip
#' @importFrom grDevices bmp colorRampPalette dev.new dev.off jpeg png rainbow
#'   tiff
"_PACKAGE"

# avoid R CMD check note
globalVariables(".")

# package startup message
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0(
    "version ", utils::packageVersion("glatos"),
    " ('very-refreshing-lemonade')"
  ))
}
