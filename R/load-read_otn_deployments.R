#' Read data from a OTN detection file
#'
#' Read data from a standard OTN detection (csv) file and return
#' a data.frame of class \code{glatos_detections}.
#'
#' @param deployment_file A character string with path and name of detection file in
#'  OTN deploymeny format (*.csv). If only file name is given, then the
#'  file must be located in the working directory.
#'
#' @details
#' Data are loaded using \code{\link[data.table]{fread}} package and timestamps
#' are coerced to POSIXct using the \code{\link[fasttime]{fastPOSIXct}}. All
#' times must be in UTC timezone per GLATOS standard.
#'
#' @details
#' Column names are changed to match GLATOS standard columns when possible.
#' Otherwise, OTN columns and column names are retained.
#'
#' @return A data.frame of class \code{glatos_receivers} that includes OTN
#' columns that do not map directly to GLATOS columns.
#'
#' @author A. Nunes, \email{anunes@dal.ca}
#'
#' @examples
#' #get path to example detection file
#' det_file <- system.file("extdata", "blue_shark_detections.csv",
#'                          package = "glatos")
#' det <- read_otn_detections(det_file)
#'
#' @export
read_otn_deployments <- function(deployment_file) {
  # Create a named vector for the column classes

}
