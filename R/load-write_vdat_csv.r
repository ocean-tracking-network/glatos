#' Write a vdat_list object to disk in Innovasea Fathom VDAT CSV format
#'
#' Write a vdat_list object to disk in Innovasea Fathom VDAT CSV format
#'
#' @param vdat A \code{vdat_list} object; e.g., produced by
#'  \code{\link{read_vdat_csv}}..
#'
#' @param record_types An optional vector of character strings with names of
#'  record types to include in output. E.g., "DET" for detection records.
#'  Default (\code{NULL}) will write all record types present in input CSV
#'  \code{vdat}.
#'
#' @param out_file Character string with name of CSV file to be written. If
#'  \code{NULL} (default), or if \code{out_file} only contains a path, then file
#'  name will be derived from the data source file name using
#'  \code{tail(vdat$DATA_SOURCE_FILE$`File Name`, 1)}.
#'
#' @param output_format Character string with output format. Options are:
#'  \code{"csv.fathom"} (default) writes a single CSV file (for each input file)
#'  with multiple record types interleaved; \code{"csv.fathom.split"} writes a
#'  folder (for each input file) containing a separate CSV for each record type.
#'
#' @param include_empty Logical (default = \code{FALSE}). If \code{output_format
#'  = "csv.fathom.split"}, should files be written for empty objects.
#'
#' @param overwrite Logical. If \code{TRUE}, output CSV file(s) will overwrite
#'  existing CSV file(s) with same name in \code{out_dir}. If \code{FALSE}
#'  (default), any output files that already exist in \code{out_dir} will be
#'  skipped, with warning.
#'
#' @param recursive Logical. If \code{TRUE} and \code{src} is a directory, then
#'  all VRL/VDAT files in all subdirectories of \code{src} will be converted.
#'  Default is \code{FALSE}. Ignored if \code{src} is a not directory.
#'
#' @param export_settings (NOT YET IMPLEMENTED). Placeholder for future
#'  specification of other options available via Fathom Data Export app. (E.g.,
#'  'Data Types to Include', 'Data Filter', 'Filename Suffix', 'Time Offset in
#'  Hours', 'Split CSV by UTC Day'.)
#'
#' @details Writing is done via \code{\link[data.table]{fwrite}}.
#'
#' @return A character string with full path and file name to output file.
#'
#' @author C. Holbrook (cholbrook@@usgs.gov)
#'
#' @examples
#'
#' # Example 1. Read and write a single file
#'
#' vrl_file <- system.file("extdata", "detection_files_raw",
#'   "VR2W_109924_20110718_1.vrl",
#'   package = "glatos"
#' )
#'
#' temp_dir <- tempdir()
#'
#' csv_file <- vdat_convert(vrl_file, out_dir = temp_dir)
#'
#' # utils::browseURL(temp_dir)
#'
#' # read all record types
#' vdat <- read_vdat_csv(csv_file)
#'
#' # write to single file (output_format = "csv.fathom")
#' write_vdat_csv(vdat)
#'
#' # write to multiple files
#' write_vdat_csv(vdat, output_format = "csv.fathom.split")
#'
#' @export
write_vdat_csv <- function(vdat,
                           record_types = NULL,
                           out_file = NULL,
                           output_format = "csv.fathom",
                           include_empty = FALSE) {
  # Check input class
  if (!inherits(vdat, "vdat_list")) {
    stop(
      "Input 'vdat' must have class ",
      "'vdat_list'"
    )
  }

  # Check and/or make output file path

  # Is path or file?
  out_file <- if (is.null(out_file)) {
    getwd()
  } else {
    normalizePath(out_file,
      mustWork = FALSE
    )
  }

  out_file_ext_in <- tools::file_ext(out_file)

  out_file_type <- ifelse(out_file_ext_in == "", "dir", "file")

  # File extension (and type) depends on output_format
  out_file_ext <-
    data.table::fcase(
      output_format == "csv.fathom", ".csv",
      output_format == "csv.fathom.split", ".csv-fathom-split"
    )

  if (out_file_type == "dir") {
    out_file_name <- gsub("\\.vrl$|\\.vdat$",
      out_file_ext,
      tail(vdat$DATA_SOURCE_FILE$`File Name`, 1),
      ignore.case = TRUE
    )

    out_file <- file.path(out_file, out_file_name)
  } else {
    # Change ext. if warranted
    out_file <- gsub(
      paste0("\\.", out_file_ext_in, "$"),
      out_file_ext,
      out_file
    )
  }


  # Make vdat csv format version and identify data generating mechanism
  src_version <- paste0(
    "VEMCO DATA LOG,",
    attr(vdat, "fathom_csv_version"), ",",
    paste0(
      "glatos-",
      packageVersion("glatos")
    )
  )


  # Subset record_types
  if (is.null(record_types)) {
    record_types <- names(vdat)
  } else {
    vdat <- vdat[record_types]
  }

  # Compress each list element into a character vector

  vdat_lines_body <- setNames(
    object = vector("list", length(record_types)),
    record_types
  )

  vdat_lines_header <- setNames(
    object = vector("list", length(record_types)),
    record_types
  )

  for (i in 1:length(record_types)) {
    # Make a deep copy
    x_i <- data.table::as.data.table(vdat[[i]])

    # Make record_type identifer column
    record_type_i <- names(vdat[i])
    record_type_i_desc <- paste0(record_type_i, "_DESC")
    x_i[, record_type := record_type_i]

    data.table::setcolorder(x_i, "record_type")


    if ("Device Time (UTC)" %in% names(x_i)) {
      x_i[, dt2 := `Device Time (UTC)`] # for sort later
    } else {
      x_i[, dt2 := as.POSIXct(NA)]
    }

    # format timestamp columns for output
    timestamp_cols <- names(which(sapply(x_i, inherits, what = "POSIXct")))

    # Exclude dtc if present
    timestamp_cols <- setdiff(timestamp_cols, "dt2")


    # Round and format all timestamp cols

    if (length(timestamp_cols) > 0) {
      x_i[, (timestamp_cols) := lapply(.SD, format_POSIXt,
        digits = 6,
        drop0trailing = TRUE
      ),
      .SDcols = timestamp_cols
      ]
    }

    if ("Time Correction (s)" %in% names(x_i)) {
      x_i[, `Time Correction (s)` := format(
        round(`Time Correction (s)`,
          digits = 9
        ),
        nsmall = 9,
        trim = TRUE,
        drop0trailing = FALSE,
        scientific = FALSE
      )]

      x_i[
        `Time Correction (s)` == "0.000000000",
        `Time Correction (s)` := "0"
      ]
    }

    if ("Ambient (deg C)" %in% names(x_i)) {
      x_i[, `Ambient (deg C)` := format(
        round(`Ambient (deg C)`,
          digits = 1
        ),
        nsmall = 1,
        trim = TRUE,
        drop0trailing = FALSE,
        scientific = FALSE
      )]

      x_i[`Ambient (deg C)` == "NA", `Ambient (deg C)` := NA_character_]
    }

    if ("Ambient Min (deg C)" %in% names(x_i)) {
      x_i[, `Ambient Min (deg C)` := format(
        round(`Ambient Min (deg C)`,
          digits = 2
        ),
        nsmall = 2,
        trim = TRUE,
        drop0trailing = FALSE,
        scientific = FALSE
      )]

      x_i[`Ambient Min (deg C)` == "NA", `Ambient Min (deg C)` := NA_character_]
    }

    if ("Ambient Max (deg C)" %in% names(x_i)) {
      x_i[, `Ambient Max (deg C)` := format(
        round(`Ambient Max (deg C)`,
          digits = 2
        ),
        nsmall = 2,
        trim = TRUE,
        drop0trailing = FALSE,
        scientific = FALSE
      )]

      x_i[`Ambient Max (deg C)` == "NA", `Ambient Max (deg C)` := NA_character_]
    }

    if ("Ambient Mean (deg C)" %in% names(x_i)) {
      x_i[, `Ambient Mean (deg C)` := format(
        round(`Ambient Mean (deg C)`,
          digits = 2
        ),
        nsmall = 2,
        trim = TRUE,
        drop0trailing = FALSE,
        scientific = FALSE
      )]

      x_i[`Ambient Mean (deg C)` == "NA", `Ambient Mean (deg C)` := NA_character_]
    }

    if ("Internal (deg C)" %in% names(x_i)) {
      x_i[, `Internal (deg C)` := format(
        round(`Internal (deg C)`,
          digits = 1
        ),
        nsmall = 1,
        trim = TRUE,
        drop0trailing = FALSE,
        scientific = FALSE
      )]

      x_i[`Internal (deg C)` == "NA", `Internal (deg C)` := NA_character_]
    }


    # Create text string
    txt_cols <- setdiff(names(x_i), c("dt2", "txt"))


    if (nrow(x_i) > 0) {
      # write to temp file; read back in, ignore delim

      temp_file_i <- tempfile()

      fwrite(x_i[, ..txt_cols], file = temp_file_i)

      x_i[, txt := fread(temp_file_i, sep = "")]
    } else {
      x_i[, txt := ""]
    }

    # Subset
    vdat_lines_body[[i]] <- x_i[, c("record_type", "dt2", "txt")]

    vdat_lines_header[[i]] <- paste(
      gsub(
        "record_type",
        record_type_i_desc,
        txt_cols
      ),
      collapse = ","
    )

    # Fix version-specific bugs

    if (record_type_i == "EVENT") {
      event_colnames_to_drop <- paste0("V", 12:17)
      vdat_lines_header[[i]] <- gsub(
        paste(event_colnames_to_drop,
          collapse = "|"
        ),
        "",
        vdat_lines_header[[i]]
      )
    }

    if (record_type_i == "PING") {
      ping_colnames_to_drop <- paste0("V", 14:17)
      vdat_lines_header[[i]] <- gsub(
        paste(ping_colnames_to_drop,
          collapse = "|"
        ),
        "",
        vdat_lines_header[[i]]
      )
    }
  } # end i


  # Combine among record types

  vdat_lines_header2 <- unname(do.call(c, vdat_lines_header))

  vdat_lines_header3 <- paste0(
    "RECORD TYPE",
    paste(rep(",FIELD", max(sapply(vdat, ncol)) - 1),
      collapse = ""
    )
  )

  if (output_format == "csv.fathom") {
    vdat_lines_body2 <- data.table::rbindlist(vdat_lines_body)

    data.table::setkey(vdat_lines_body2, dt2)


    vdat_out <- data.table::data.table(
      c(
        src_version,
        vdat_lines_header3,
        vdat_lines_header2,
        vdat_lines_body2$txt
      )
    )

    data.table::fwrite(
      x = vdat_out,
      file = out_file,
      col.names = FALSE,
      quote = FALSE
    )
  }

  if (output_format == "csv.fathom.split") {
    # Create folder if not exist
    if (!dir.exists(out_file)) dir.create(out_file)


    # Write csv for each record type

    for (i in 1:length(vdat_lines_body)) {
      if (nrow(vdat_lines_body[[i]]) > 0 | include_empty) {
        out_file_i <- file.path(
          out_file,
          paste0(
            names(vdat_lines_body[i]),
            ".csv"
          )
        )

        vdat_out_i <- data.table::data.table(c(
          src_version,
          "",
          vdat_lines_header2[[i]],
          vdat_lines_body[[i]]$txt
        ))

        data.table::fwrite(
          x = vdat_out_i,
          file = out_file_i,
          col.names = FALSE,
          quote = FALSE
        )
      } # end if
    } # end i
  }

  return(out_file)
}


#' Round timestamp by fractional second and coerce to character
#'
#' @param x A \code{POSIXct} or \code{POSIXlt} object.
#'
#' @param digits The number of decimal places to which seconds is rounded.
#'
#' @param drop0trailing logical (default = TRUE), indicating if trailing zeros,
#'   i.e., "0" after the decimal mark, should be removed. Passed to
#'   \link{format} which passes to \link{prettyNum}.
#'
#' @return A character vector in format like \code{"\%Y-\%m-\%d \%H:\%M:\%OSn"}
#'   (see \link{strptime} but see 'detail' for differences).
#'
#' @details Differs from e.g., \code{format(x, format = "\%Y-\%m-\%d
#'   \%H:\%M:\%OS6")} in that (1) rounding is used (not truncation) and (2)
#'   trailing 0s can be omitted (via \code{drop0trailing}).
#'
#' @details Differs from \code{lubridate::round_Date} in that it is accurate for
#'   < 1 sec (see example 1 below for motivating example) but requires coercion
#'   to POSIXlt before rounding and coercing to character.
#'
#'
#' @examples
#'
#' # Example 1 - motivating example: e.g., trouble with lubridate::round_Date
#' t1 <- as.POSIXct("2011-05-08 05:37:39.245541", tz = "UTC")
#' format(t1, digits = 6)
#'
#' t2 <- lubridate::round_date(t1, unit = "0.00001s")
#' format(t2, digits = 6)
#'
#' t3 <- format_POSIXt(t1, digits = 5)
#' format(t3, digits = 6)
#'
#' # Example 2
#' t1 <- as.POSIXct(
#'   c(
#'     "2011-03-08 23:59:58",
#'     "2011-03-08 23:59:58.828867"
#'   ),
#'   tz = "UTC"
#' )
#' format_POSIXt(t1, digits = 5, drop0trailing = FALSE)
#' format_POSIXt(t1, digits = 5, drop0trailing = TRUE)
#'

#' @export
format_POSIXt <- function(x, digits = 0, drop0trailing = TRUE) {
  digits <- as.integer(digits) # for as.character

  stopifnot(
    "'digits' must be >= 0 and <= 14" =
      data.table::between(digits, 0, 14)
  )

  # Coerce to POSIXlt
  xlt <- if (inherits(x, "POSIXct")) {
    as.POSIXlt(x)
  } else if (inherits(x, "POSIXlt")) {
    x
  } else {
    stop("class of input 'x' must be 'POSIXct' or 'POSIXlt'.")
  }

  # Round seconds value
  xlt$sec <- round(xlt$sec, digits = digits)

  # Format fractional seconds for output
  frac_sec <- if (digits > 0) {
    do.call(
      format,
      c(list(x = xlt$sec %% 1),
        drop0trailing = drop0trailing,
        trim = TRUE
      )
    )
  } else {
    character()
  }

  frac_sec <- gsub("^0|^NA$", "", frac_sec)


  # Format as string and truncate
  xch <- do.call(format, c(list(x = xlt),
    format = "%Y-%m-%d %H:%M:%S",
    digits = 0
  ))

  xch[is.na(xch)] <- ""

  xout <- paste0(xch, frac_sec)
  xout[xout == ""] <- NA_character_

  return(xout)
}


#' Subset method for vdat_list that retains attributes
#' @export
`[.vdat_list` <- function(x, i, ...) {
  attrs <- attributes(x)
  out <- unclass(x)
  out <- out[i]
  if (!is.null(attrs$names)) attrs$names <- names(x)[i]
  attributes(out) <- attrs
  out
}
