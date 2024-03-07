#' Read data from a OTN detection file
#'
#' Read data from a standard OTN detection (csv) file and return
#' a data.frame of class `glatos_detections`.
#'
#' @param det_file A character string with path and name of detection file in
#'  OTN detection extract format (*.csv). If only file name is given, then the
#'  file must be located in the working directory.
#'
#' @details
#' Data are loaded using [data.table::fread()] package and timestamps
#' are coerced to POSIXct using [lubridate::fast_strptime()]. All
#' times must be in UTC timezone per GLATOS standard.
#'
#' @details
#' Column names are changed to match GLATOS standard columns when possible.
#' Otherwise, OTN columns and column names are retained.
#'
#' @return A data.frame of class `glatos_detections` that includes OTN
#' columns that do not map directly to GLATOS columns.
#'
#' @author A. Nunes, \email{anunes@dal.ca}
#'
#' @examples
#' # get path to example detection file
#' det_file <- system.file("extdata", "blue_shark_detections.csv",
#'   package = "glatos"
#' )
#' det <- read_otn_detections(det_file)
#'
#' @importFrom lubridate fast_strptime
#'
#' @export
read_otn_detections <- function(det_file) {
  # Create a named vector for the column classes
  col_classes <- otn_detection_schema$type
  names(col_classes) <- otn_detection_schema$name
  timestamp_cols <- which(col_classes == "POSIXct")
  date_cols <- which(col_classes == "Date")
  col_classes[c(timestamp_cols, date_cols)] <- "character"
  
  # read data, suppressWarnings because some columns could be missing
  dtc <- suppressWarnings(data.table::fread(det_file,
                                            sep = ",", colClasses = col_classes,
                                            na.strings = c("", "NA")
  ))
  # This check is for non-matched detection extracts. They are missing some required columns, this attempts to create them.
  # More info on OTN detection extracts here: https://members.oceantrack.org/data/otn-detection-extract-documentation-matched-to-animals
  if (all(otn_detection_schema_min_columns %in% colnames(dtc)) && !all(otn_detection_schema$name %in% colnames(dtc))) {
    dtc$commonname <- "Unknown"
    dtc$receiver_group <- substr(dtc$station, 1, nchar(dtc$station) - 3)
    dtc$receiver <- dtc$collectornumber
    dtc$tagname <- dtc$fieldnumber
    dtc$codespace <- purrr::map(dtc$fieldnumber, get_codemap)
  }
  # coerce timestamps to POSIXct
  for (j in timestamp_cols) {
    data.table::set(dtc,
                    j = otn_detection_schema$name[j],
                    value = lubridate::fast_strptime(
                      dtc[[otn_detection_schema$name[j]]], 
                      format = "%Y-%m-%d %H:%M:%S", tz = "UTC", lt = FALSE
                    )
    )
  }
  # coerce dates to date
  for (j in date_cols) {
    data.table::set(dtc, j = otn_detection_schema$name[j], value = ifelse(dtc[[otn_detection_schema$name[j]]] == "", NA, dtc[[otn_detection_schema$name[j]]]))
    data.table::set(dtc, j = otn_detection_schema$name[j], value = as.Date(dtc[[otn_detection_schema$name[j]]]))
  }
  data.table::setnames(dtc, old = otn_detection_schema$name, new = otn_detection_schema$mapping)
  dtc <- glatos_detections(dtc)
  return(dtc)
}

get_codemap <- function(x) {
  x0 <- unlist(strsplit(x, "-"))
  return(paste0(x0[1:2], collapse = "-"))
}
