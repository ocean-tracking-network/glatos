#' Read data from a OTN detection file
#'
#' Read data from a standard OTN detection (csv) file and return
#' a data.frame of class \code{glatos_detections}.
#'
#' @param deployment_file A character string with path and name of detection file in
#'  OTN deploymeny format (*.csv). If only file name is given, then the
#'  file must be located in the working directory.
#'  
#'  @param deploy_date_col A character string representing the column
#'  name containing deploy_date data. Defaults to "deploy_date".
#'  
#'  @param recovery_date_col A character string representing the column
#'  name containing recovery_date. Defaults to "recovery_date."
#'  
#'  @param last_download_col A character string representing the column
#'  name containing the last_download date. Defaults to "last_download."
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
#' det <- read_otn_deployments(deployment_file)
#'
#' @importFrom lubridate parse_date_time
#' @importFrom tidyr extract
#' @importFrom dplyr mutate
#' @importFrom magrittr "%>%"
#' @export
read_otn_deployments <- function(deployment_file,
                                 deploy_date_col = 'deploy_date',
                                 recovery_date_col = 'recovery_date',
                                 last_download_col = 'last_download') {
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
  dtc <- dtc %>% tidyr::extract(deploy_date_col,into="deploy_date", regex="(\\d+-\\d+-\\d+)")
  dtc <- dtc %>% tidyr::extract(recovery_date_col,into="recovery_date", regex="(\\d+-\\d+-\\d+)")
  dtc <- dtc %>% tidyr::extract(last_download_col,into="last_download", regex="(\\d+-\\d+-\\d+)")
  
  options(lubridate.fasttime = TRUE)
  for (j in timestamp_cols) {
    data.table::set(dtc, j = otn_deployments_schema$name[j],
                    value = lubridate::parse_date_time(dtc[[otn_deployments_schema$name[j]]], orders="ymd", tz = "UTC"))
  }
  #coerce dates to date
  for (j in date_cols) {
    data.table::set(dtc, j = otn_deployments_schema$name[j], value = ifelse(dtc[[otn_deployments_schema$name[j]]] == "", NA, dtc[[otn_deployments_schema$name[j]]]))
    data.table::set(dtc, j = otn_deployments_schema$name[j], value = as.Date(dtc[[otn_deployments_schema$name[j]]]))
  }
  data.table::setnames(dtc, old=otn_deployments_schema$name, new=otn_deployments_schema$mapping)
  dtc <- glatos_receivers(dtc)
  return(dtc)
}
