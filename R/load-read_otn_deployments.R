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
#' #get path to example deployments file
#' deployment_file <- system.file("extdata", "blue_shark_detections.csv",
#'                          package = "glatos")
#' det <- read_otn_deploymentss(deployment_file)
#'
#' @export
read_otn_deployments <- function(deployment_file) {
  col_classes <- otn_deployments_schema$type
  names(col_classes) <- otn_deployments_schema$name
  timestamp_cols <- which(col_classes == "POSIXct")
  date_cols <- which(col_classes == "Date")
  col_classes[c(timestamp_cols, date_cols)] <- "character"
  
  #read data
  dtc <- data.table::fread(deployment_file, sep = ",", colClasses = col_classes,
                           na.strings = c("", "NA"))
  
  #coerce timestamps to POSIXct; note that with fastPOSIXct raw
  #  timestamp must be in UTC; and tz argument sets the tzone attr only
  for (j in timestamp_cols) data.table::set(dtc, j = otn_deployments_schema$name[j],
                                            value = fasttime::fastPOSIXct(dtc[[otn_deployments_schema$name[j]]], tz = "UTC"))
  #coerce dates to date
  for (j in date_cols) {
    data.table::set(dtc, j = otn_deployments_schema$name[j], value = ifelse(dtc[[otn_deployments_schema$name[j]]] == "", NA, dtc[[otn_deployments_schema$name[j]]]))
    data.table::set(dtc, j = otn_deployments_schema$name[j], value = as.Date(dtc[[otn_deployments_schema$name[j]]]))
  }
  data.table::setnames(dtc, old=otn_deployments_schema$name, new=otn_deployments_schema$mapping)
  dtc <- glatos_receivers(dtc)
  return(dtc)
}
