#' Read receiver event data exported from Innovasea VUE software
#'
#' Read receiver event data exported from Innovasea VUE software
#'
#' @param src A character string with path and name of a CSV file produced
#'   containing receiver event data exported from Innovasea VUE software. If
#'   only file name is given, then the file must be located in the working
#'   directory.
#'
#' @param show_progress Optional argument passed to \code{data.table::fread}'s
#'   \code{showProgress}.
#'
#' @details Reading is done via \code{\link[data.table]{fread}}.
#'
#' @details All timestamp columns are assumed to be in UTC.
#'
#' @return A data.frame of class \code{vue_receiver_events}.
#'
#'
#' @author C. Holbrook (cholbrook@@usgs.gov)
#'
#' @examples
#' csv_file <- system.file("extdata",
#'   "VR2W_receiverEvents_109924_20110718_1.csv",
#'   package = "glatosDS"
#' )
#'
#' vue_evn <- read_vue_event_csv(csv_file)
#'
#' @export
read_vue_event_csv <- function(src,
                               show_progress = FALSE) {
  # Check if exists
  if (!file.exists(src)) {
    warning("File not found: ", src)
    return()
  }

  # Check if looks like VUE export format
  src_header <- data.table::fread(file = src, nrows = 1L, header = FALSE)

  vue_event_cols <- c(
    `Date and Time (UTC)` = "POSIXct",
    Receiver = "character",
    Description = "character",
    Data = "character",
    Units = "character"
  )

  missing_cols <- setdiff(
    names(vue_event_cols),
    src_header[1, ]
  )

  if (length(missing_cols) > 0) {
    stop(
      "Input file does not appear to be in VUE Export format.\n\ ",
      "The following columns are missing: \n  ",
      paste(missing_cols, collapse = "\n  ")
    )
  }


  # Read each list element separately
  vue_events <- data.table::fread(
    file = src,
    sep = ",",
    na.strings = "",
    colClasses = vue_event_cols,
    header = TRUE,
    encoding = "Latin-1",
    fill = TRUE,
    showProgress = show_progress
  )

  # Assign class
  vue_events <- structure(vue_events,
    class = c(
      "vue_receiver_events",
      class(vue_events)
    )
  )

  return(vue_events)
}
