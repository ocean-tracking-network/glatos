#' Read data from a GLATOS receiver location file
#'
#' Read data from a standard GLATOS receiver location (csv) file and return a
#' data.frame of class `glatos_receivers`.
#'
#' @param rec_file A character string with path and name of receiver location
#'   file in standard GLATOS format (*.csv). If only file name is given, then
#'   the file must be located in the working directory. File must be a standard
#'   GLATOS file (e.g., *GLATOS_receiverLocations_yyyymmdd_xxxxxx.csv*) obtained
#'   from GLATOSWeb Data Portal <http://glatos.glos.us>.
#'
#' @param version An optional character string with the GLATOS file version
#'   number. If NULL (default value) then version will be determined by
#'   evaluating file structure. The only allowed values currently are `NULL` and
#'   `"1.0"`. Any other values will trigger an error.
#'
#' @details Data are loaded using [fread][data.table::fread] and timestamps are
#'   coerced to POSIXct using [fastPOSIXct][fasttime::fastPOSIXct]. All
#'   timestamps must be 'YYYY-MM-DD HH:MM' format and in UTC timezone per GLATOS
#'   standard.
#'
#' @return A data.frame of class `glatos_receivers`.
#'
#' @author C. Holbrook (cholbrook@usgs.gov)
#'
#' @examples
#' # get path to example receiver_locations file
#' rec_file <- system.file("extdata",
#'   "sample_receivers.csv",
#'   package = "glatos"
#' )
#'
#' # note that code above is needed to find the example file
#' # for real glatos data, use something like below
#' # rec_file <- "c:/path_to_file/GLATOS_receiverLocations_20150321_132242.csv"
#'
#' rcv <- read_glatos_receivers(rec_file)
#'
#' @importFrom lubridate parse_date_time
#'
#' @export
read_glatos_receivers <- function(rec_file, version = NULL) {
  # Read source file-------------------------------------------------------

  # see version-specific file specifications
  # internal data object; i.e., in R/sysdata.r


  # Identify file version
  id_file_version <- function(rec_file) {
    col_names <- names(data.table::fread(rec_file, nrows = 0))
    if (all(glatos:::glatos_receivers_schema$v1.1$name %in% col_names)) {
      return("1.1")
    } else if (all(glatos:::glatos_receivers_schema$v1.0$name %in% col_names)) {
      return("1.0")
    } else {
      stop("Receiver location file version could not be identified.")
    }
  }

  if (is.null(version)) {
    version <- id_file_version(rec_file)
  } else if (!(paste0("v", version) %in%
    names(glatos:::glatos_receivers_schema))) {
    stop(paste0(
      "Receiver locations file version ", version,
      " is not supported."
    ))
  }

  #-v1.x----------------------------------------------------------------
  if (version %in% c("1.0", "1.1")) {
    ver_txt <- paste0("v", version)

    col_classes <- glatos:::glatos_receivers_schema[[ver_txt]]$type
    timestamp_cols <- which(col_classes == "POSIXct")
    date_cols <- which(col_classes == "Date")
    col_classes[c(timestamp_cols, date_cols)] <- "character"

    # read data
    rec <- data.table::fread(rec_file, sep = ",", colClasses = col_classes)

    # coerce timestamps to POSIXct; note that with fastPOSIXct raw
    #  timestamp must be in UTC; and tz argument sets the tzone attr only
    options(lubridate.fasttime = TRUE)
    for (j in timestamp_cols) {
      data.table::set(rec,
        j = glatos_receivers_schema[[ver_txt]]$name[j],
        value = lubridate::parse_date_time(
          rec[[glatos_receivers_schema[[ver_txt]]$name[j]]],
          orders = "ymd HMS",
          tz = "UTC"
        )
      )
    }
    # coerce dates to date
    for (j in date_cols) {
      data.table::set(rec,
        j = glatos_receivers_schema[[ver_txt]]$name[j],
        value = ifelse(rec[[glatos_receivers_schema[[ver_txt]]$name[j]]] == "",
          NA, rec[[glatos_receivers_schema[[ver_txt]]$name[j]]]
        )
      )
      data.table::set(rec,
        j = glatos_receivers_schema[[ver_txt]]$name[j],
        value = as.Date(rec[[glatos_receivers_schema[[ver_txt]]$name[j]]])
      )
    }
  }
  #-end v1.x----------------------------------------------------------------

  # strip data.table and assign glatos_receivers class
  data.table::setDF(rec)

  rec <- as_glatos_receivers(rec)

  return(rec)
}
