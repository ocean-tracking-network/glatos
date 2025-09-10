#' Read data from a OTN detection file
#'
#' Read data from a standard OTN detection (csv) file and return
#' a data.frame of class `glatos_detections`.
#'
#' @param det_file A character string with path and name of detection file in
#'  OTN detection extract format (*.csv). If only file name is given, then the
#'  file must be located in the working directory.
#'
#' @param format Either 'new' or 'old', denoting whether or not the file being
#' loaded is a CSV predating OTN's parquet rollout (old) or not (new).
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
#' @importFrom nanoparquet read_parquet
#'
#' @export
read_otn_detections <- function(det_file, format = "new") {
  # We have to determine which schema to use since this function could conceivably take both types.
  if (format == "new") {
    det_schema <- otn_detection_schema
    min_cols <- otn_detection_schema_min_columns
  } else if (format == "old") {
    det_schema <- otn_detection_schema_old
    min_cols <- otn_detection_schema_old_min_columns
  } else {
    message("You must provide a valid format to the read_otn_detections function. Valid formats are either 'new' (CSVs and Parquet files from after OTN's Parquet rollout) or 'old' (CSVs from before the Parquet rollout. If no format is supplied, 'new' is the default.")
    return(0)
  }

  # Create a named vector for the column classes
  col_classes <- det_schema$type
  names(col_classes) <- det_schema$name
  timestamp_cols <- which(col_classes == "POSIXct")
  date_cols <- which(col_classes == "Date")
  col_classes[c(timestamp_cols, date_cols)] <- "character"

  # Check if file is zipped
  # `data.table::fread` can handle zipped CSVs if they are the only file in the
  #   directory. If there are multiple files, they need to be unzipped first.
  #   This code assumes that there is only one CSV within the zipped directory.
  #   The other file would be "data_description.txt"
  if (tools::file_ext(det_file) == "zip" && nrow(zip::zip_list(det_file)) > 1) {
    td <- tempdir()

    zip::unzip(
      det_file,
      exdir = file.path(td, tools::file_path_sans_ext(basename(det_file)))
    )
    det_file <- list.files(
      file.path(td, tools::file_path_sans_ext(basename(det_file))),
      pattern = "\\.csv$",
      full.names = TRUE
    )

    if (length(det_file) > 1) {
      message("It looks like there is more than one CSV in the zip archive you are trying to load.")
      message("read_otn_detections can only process zip files if they contain one CSV (or one CSV plus a description.txt file).")
      message("Please unzip the archive and load the CSV files individually.")
      return(FALSE)
    }

    det_file <- normalizePath(det_file)
  }

  # read data, suppressWarnings because some columns could be missing
  #If the file is a parquet file, read it using nanoparquet.
  if (tools::file_ext(det_file) == "parquet") {
    dtc <- nanoparquet::read_parquet(
      det_file
    )
  } else {
    dtc <- suppressWarnings(data.table::fread(
      det_file,
      sep = ",",
      colClasses = col_classes,
      na.strings = c("", "NA")
    ))
  }

  # This check is for non-matched detection extracts. They are missing some required columns, this attempts to create them.
  # More info on OTN detection extracts here: https://members.oceantrack.org/data/otn-detection-extract-documentation-matched-to-animals
  if (
    all(min_cols %in% colnames(dtc)) &&
      !all(det_schema$name %in% colnames(dtc))
  ) {
    dtc$commonname <- "Unknown"
    dtc$receiver_group <- substr(dtc$station, 1, nchar(dtc$station) - 3)
    dtc$receiver <- dtc$collectornumber
    dtc$tagname <- dtc$fieldnumber
    dtc$codespace <- get_codemap(dtc$fieldnumber)
  }
  # coerce timestamps to POSIXct
  for (j in timestamp_cols) {
    data.table::set(
      dtc,
      j = det_schema$name[j],
      value = lubridate::parse_date_time(
        dtc[[det_schema$name[j]]],
        orders = c("%Y-%m-%d %H:%M:%S")
      )
    )
  }
  # coerce dates to date
  for (j in date_cols) {
    data.table::set(
      dtc,
      j = det_schema$name[j],
      value = ifelse(
        dtc[[det_schema$name[j]]] == "",
        NA,
        dtc[[det_schema$name[j]]]
      )
    )
    data.table::set(
      dtc,
      j = det_schema$name[j],
      value = as.Date(dtc[[det_schema$name[j]]])
    )
  }
  data.table::setnames(
    dtc,
    old = det_schema$name,
    new = det_schema$mapping
  )
  dtc <- glatos_detections(dtc)
  return(dtc)
}

get_codemap <- function(x) {
  sapply(
    x,
    FUN = function(.) {
      x0 <- unlist(strsplit(., "-"))
      return(paste0(x0[1:2], collapse = "-"))
    },
    USE.NAMES = FALSE
  )
}
