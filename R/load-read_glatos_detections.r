#' @title Read data from a GLATOS detection file
#'
#' @description Read data from a standard GLATOS detection (csv) file and return
#'   a data.frame of class `glatos_detections`.
#'
#' @param det_file A character string with path and name of detection file in
#'   standard GLATOS format (*.csv). If only file name is given, then the file
#'   must be located in the working directory. File must be a standard GLATOS
#'   file (e.g., *xxxxx_detectionsWithLocs_yyyymmdd_hhmmss.csv*) submitted via
#'   GLATOSWeb Data Portal <https://glatos.glos.us>.
#'
#' @param version An optional character string with the glatos file version
#'   number. If NULL (default value) then version will be determined by
#'   evaluating file structure. The only allowed values currently are `NULL`,
#'   `"1.3"`, and `"1.4"`. Any other values will trigger an error. Users should
#'   never need to set this argument (`NULL` should work).
#'
#' @details Data are loaded using [fread][data.table::fread] and timestamps are
#' coerced to POSIXct using [fast_strptime][lubridate::fast_strptime]. All times
#' must be in UTC timezone per GLATOS standard.
#'
#' @details Column `animal_id` is considered a required column by many other
#'   functions in this package, so it will be created if any records are `NULL`.
#'   When created, it will be constructed from `transmitter_codespace` and
#'   `transmitter_id`, separated by '-'.
#'
#' @return A data.frame of class `glatos_detections`.
#'
#' @author C. Holbrook \email{cholbrook@@usgs.gov}
#'
#' @examples
#' # get path to example detection file
#' det_file <- system.file("extdata", "walleye_detections.csv",
#'   package = "glatos"
#' )
#'
#' # note that code above is needed to find the example file
#' # for real glatos data, use something like below
#' # det_file <- "c:/path_to_file/HECWL_detectionsWithLocs_20150321_132242.csv"
#'
#' det <- read_glatos_detections(det_file)
#'
#' @importFrom lubridate fast_strptime
#'
#' @export
read_glatos_detections <- function(det_file, version = NULL) {
  # Read detection file-------------------------------------------------------

  # see version-specific file specifications
  # internal data object; i.e., in R/sysdata.r

  # Identify detection file version
  id_det_version <- function(det_file) {
    det_col_names <- names(data.table::fread(det_file, nrows = 0))
    if (all(glatos_detection_schema$v1.4$name %in% det_col_names)) {
      return("1.4")
    } else if (all(glatos_detection_schema$v1.3$name %in% det_col_names)) {
      return("1.3")
    } else {
      stop("Detection file version could not be identified.")
    }
  }

  if (is.null(version)) {
    version <- id_det_version(det_file)
  } else if (
    !(paste0("v", version) %in%
      names(glatos_detection_schema))
  ) {
    stop(paste0("Detection file version ", version, " is not supported."))
  }

  #-Detections v1.3 or v1.4-----------------------------------------------------
  if (version %in% c("1.3", "1.4")) {
    vversion <- paste0("v", version)

    col_classes <- glatos_detection_schema[[vversion]]$type
    timestamp_cols <- which(col_classes == "POSIXct")
    date_cols <- which(col_classes == "Date")
    col_classes[c(timestamp_cols, date_cols)] <- "character"

    # read data
    dtc <- data.table::fread(
      det_file,
      sep = ",",
      colClasses = col_classes,
      na.strings = c("", "NA")
    )

    # coerce timestamps to POSIXct
    for (j in timestamp_cols) {
      data.table::set(
        dtc,
        j = glatos_detection_schema[[vversion]]$name[j],
        value = lubridate::fast_strptime(
          dtc[[glatos_detection_schema[[vversion]]$name[j]]],
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC",
          lt = FALSE
        )
      )
    }
    # coerce dates to date
    for (j in date_cols) {
      data.table::set(
        dtc,
        j = glatos_detection_schema[[vversion]]$name[j],
        value = ifelse(
          dtc[[glatos_detection_schema[[vversion]]$name[j]]] == "",
          NA,
          dtc[[glatos_detection_schema[[vversion]]$name[j]]]
        )
      )
      data.table::set(
        dtc,
        j = glatos_detection_schema[[vversion]]$name[j],
        value = as.Date(dtc[[glatos_detection_schema[[vversion]]$name[j]]])
      )
    }
  }
  #-end v1.3 or v1.4------------------------------------------------------------

  # create animal_id if missing
  anid_na <- is.na(dtc$animal_id)
  if (any(anid_na)) {
    dtc$animal_id[anid_na] <- with(
      dtc,
      paste0(transmitter_codespace[anid_na], "-", transmitter_id[anid_na])
    )
    warning(paste0(
      "Some or all values of required column 'animal_id' were ",
      "missing so they were created from 'transmitter_codespace' and ",
      "'transmitter_id'.)"
    ))
  }

  # strip data.table and assign glatos_detections class
  data.table::setDF(dtc)

  dtc <- as_glatos_detections(dtc)

  return(dtc)
}
