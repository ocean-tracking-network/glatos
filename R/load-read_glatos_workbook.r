#' Read data from a GLATOS project workbook
#'
#' Read data from a GLATOS project workbook (xlsm or xlsx file).
#'
#' @param wb_file A character string with path and name of workbook in standard
#'   GLATOS format (.xlsm or .xlsx). If only file name is given, then the file
#'   must be located in the working directory. See the GLATOSWeb Data
#'   Portal <https://glatos.org> for file format definitions.
#'
#' @param wb_version An optional character string with the workbook version
#'   number. If NULL (default value) then version will be determined by
#'   evaluating workbook structure. Currently, the only allowed values are
#'   `NULL`, `"1.3"`, and `"1.4"`. See Details. Any other values will trigger
#'   an error.
#'
#' @param read_all If TRUE, then all columns and sheets (e.g., user-created
#'   "project-specific" columns or sheets) in the workbook will be imported. If
#'   FALSE (default value) then only columns and sheets in the standard GLATOS
#'   workbook will be imported (project-specific columns will be ignored).
#'
#' @param simplify If TRUE (default value), then the returned object is a
#'   `glatos_workbook` object. If FALSE, then the returned object is a list with
#'   an element for each sheet in `wb_file`. See Details.
#'
#' @details When `simplify = TRUE`, data in workbook sheets 'Deployment',
#'   'Recovery', and 'Location' are merged on columns 'GLATOS_PROJECT',
#'   'GLATOS_ARRAY', 'STATION_NO', 'CONSECUTIVE_DEPLOY_NO', AND 'INS_SERIAL_NO'
#'   to produce the output data frame `receivers`. Data in workbook sheets
#'   'Project' and 'Tagging' are passed through to new data frames named
#'   'project' and 'animals', respectively, and data from workbook sheet
#'   'Proposed' is not included in result. When `simplify = FALSE`, data in all
#'   sheets in the standard workbook are passed through to new data frames with
#'   like names (e.g., 'location', 'deployment', 'recovery').
#'
#' @details If `read_all = TRUE` then each sheet not included in the standard
#'   workbook (e.g., not named 'Project', 'Location', 'Deployment', 'Recovery',
#'   or 'Tagging') will be included as an element of the returned list; and in
#'   standard workbook sheets, any non-standard columns (i.e, 'project-specific
#'   fields') will be included in the result. Names of non-standard columns
#'   may be changed (e.g., for uniqueness), with warnings.
#'
#' @details Data are read from the input file using
#'   [read_excel][readxl::read_excel] in the 'readxl' package. If `read_all =
#'   TRUE` then the type of data in each user-defined column (and sheet) will be
#'   'guessed' by [read_excel][readxl::read_excel]. Therefore, if `read_all =
#'   TRUE` then the structure of those columns should be carefully reviewed in
#'   the result. See [read_excel][readxl::read_excel] for details.
#'
#' @details Column `animal_id` is considered a required column by many other
#'   functions in this package, so it will be created if any records are `NULL`.
#'   When created, it will be constructed from `tag_code_space` and
#'   `tag_id_code`, separated by '-'.
#'
#' @details Timezone attribute of all timestamp columns (class `POSIXct`) in
#'   output will be "UTC" and all 'glatos-specific' timestamp and timezone
#'   columns will be omitted from result.
#'
#' @details As of glatos 0.9.0, if a sheet contains two columns with the same
#'   name, then the sheet is not loaded and an error is returned. In earlier
#'   versions, a suffix was added to all but the first in each set of
#'   duplicate column names.
#'
#' @details As of glatos 0.9.8, time zones (e.g., columns named
#' "GLATOS_TIMEZONE") are checked against [OlsonNames()] after prepending "US/".
#' E.g., "Eastern" becomes "US/Eastern". Matching is not case sensitive, so
#' "EASTERN" is valid, but is replaced by "US/Eastern", with a warning. Invalid
#' time zones will result in `NA` timestamp values, with a warning.
#'
#' @note ***On warnings and errors about date and timestamp formats.*** Date and
#'   time columns are sometimes stored as text in Excel. When those records are
#'   loaded by this function, there are two possible outcomes. \cr \cr 1. If the
#'   records are formatted according to the GLATOS Data Dictionary specification
#'   (e.g., "YYYY-MM-DD" for dates and "YYYY-MM-DD HH:MM" for timestamps; see
#'   <https:\\glatos.org>) those records should be properly loaded into R,
#'   but the user is encouraged to verify that they were loaded correctly, so a
#'   warning points the user to those records in the workbook. Users may want to
#'   format as custom date in the workbook to avoid warnings in the future. \cr
#'   \cr 2. If the format of a date-as-text column is not consistent with GLATOS
#'   specification, then no data will be loaded and an error will alert the user
#'   to this condition. Similarly, if a date or date-time column is stored
#'   as a number in Excel, then no data will be loaded and an error will alert
#'   the user to this condition.\cr \cr
#'   ***On cells with locked formatting in Excel:*** Occasionally the
#'   format of a cell in Excel will be locked. In those cases, it is sometimes
#'   possible to force date formatting in Excel by (1) highlighting the columns
#'   that need reformatting, (2) select 'Text-to-columns' in the 'Data' menu,
#'   (3) select 'Delimited' and 'next', (4) uncheck all delimiters and 'next',
#'   (5) choose 'Date: YMD' in the 'Column data format' box, and (6) 'Finish'.
#'
#' @return If `simplify = TRUE`, a list of class `glatos_workbook` with three
#'   elements (described below) containing data from the standard GLATOS
#'   Workbook sheets. If `read_all = TRUE`, then additional elements will be
#'   added with names corresponding to non-standard sheet names.
#' \describe{
#'   \item{metadata}{A list with data about the project and workbook.}
#'   \item{animals}{A data frame of class `glatos_animals` with data about
#'   tagged animals.}
#'   \item{receivers}{A data frame of class `glatos_receivers` with data
#'   about telemetry receivers.}
#'
#'   If `simplify = FALSE`, a list (class = `list`) with one element for each
#'   sheet in `wb_file`.
#' }
#'
#' @author C. Holbrook \email{cholbrook@glfc.org}
#'
#' @seealso [read_excel][readxl::read_excel]
#'
#' @examples
#'
#' # Example 1: Version 1.3 (xlsm)
#'
#' # get path to example GLATOS Data Workbook
#' wb_file <- system.file("extdata",
#'   "walleye_workbook.xlsm",
#'   package = "glatos"
#' )
#'
#' # note that code above is needed to find the example file
#' # for real glatos data, use something like below
#' # wb_file <- "c:/path_to_file/HECWL_GLATOS_20150321.xlsm"
#'
#' wb <- read_glatos_workbook(wb_file)
#'
#' wba <- read_glatos_workbook(wb_file, read_all = TRUE)
#'
#' wbr <- read_glatos_workbook(wb_file, simplify = FALSE)
#'
#'
#' # Example 2: Version 1.4 (xlsx)
#'
#' wb2_file <- system.file("extdata",
#'   "walleye_workbook.xlsx",
#'   package = "glatos"
#' )
#'
#' wb2 <- read_glatos_workbook(wb2_file)
#'
#' wb2a <- read_glatos_workbook(wb2_file, read_all = TRUE)
#'
#' wbr2 <- read_glatos_workbook(wb2_file, simplify = FALSE)
#'
#' @import readxl
#'
#' @export
read_glatos_workbook <- function(
  wb_file,
  read_all = FALSE,
  wb_version = NULL,
  simplify = TRUE
) {
  # See version-specific file specifications
  # internal glatos_workbook_spec in R/sysdata.r

  # Get and check version if specified
  wb_version <- identify_workbook_version(
    wb_file,
    wb_version
  )


  #-Workbook v1.3, v1.4--------------------------------------------------------------
  if (wb_version %in% c("1.3", "1.4")) {
    # Get sheet names in external file
    wb_sheets <- readxl::excel_sheets(wb_file)

    # Subset sheet names to read
    sheets_to_read <-
      if (read_all) {
        wb_sheets
      } else {
        wb_sheets[tolower(wb_sheets) %in% names(glatos_workbook_schema$v1.3)]
      }

    # Identify extra (project-specific) sheets
    extra_sheets <- setdiff(
      tolower(sheets_to_read),
      names(glatos_workbook_schema$v1.3)
    )

    # Check that extra sheet names do not match any standard sheet names
    invalid_sheet_names <-
      if (simplify) {
        intersect(
          extra_sheets,
          c(
            "metadata",
            "animals",
            "receivers"
          )
        )
      } else {
        NULL
      }

    if (length(invalid_sheet_names) > 0) {
      stop("When `read_all = TRUE` and ",
        "`simplify = TRUE`, ",
        "a project specific sheet cannot ",
        "be named 'metadata', 'animals' ",
        "or 'receivers'.",
        call. = FALSE
      )
    }


    # Preallocate glatos_workbook object
    wb <- stats::setNames(
      vector("list", length(sheets_to_read)),
      tolower(sheets_to_read)
    )


    # Read project data (non-standard structure)
    wb$project <- read_workbook_project(wb_file)


    # Read all sheets except project
    sheets_to_read <- grep("project", sheets_to_read,
      ignore.case = TRUE,
      value = TRUE,
      invert = TRUE
    )


    for (i in 1:length(sheets_to_read)) {
      schema_i <- glatos_workbook_schema[["v1.3"]][[tolower(sheets_to_read[i])]]

      # Specify first row to read (with headers)
      xl_first_row <- switch(wb_version,
        "1.3" = 2,
        "1.4" = 1
      )

      # Specify last column to read (based on 'read_all' arg)
      xl_last_col <- if (read_all) NA_integer_ else nrow(schema_i)

      # is the ith sheet an extra?
      extra_sheet_i <- tolower(sheets_to_read[i]) %in% extra_sheets

      # Read sheet data
      sheet_i <-
        tryCatch(
          readxl::read_excel(
            wb_file,
            sheet = sheets_to_read[i],
            range = if (extra_sheet_i) {
              NULL
            } else {
              readxl::cell_limits(
                ul = c(xl_first_row, 1),
                lr = c(NA, xl_last_col)
              )
            },
            col_types = if (extra_sheet_i) NULL else "list",
            na = c("", "NA"),
            # n_max = 0,
            guess_max = 1048576,
            .name_repair = "minimal"
          ),
          error = function(e) e
        )

      # Add context (sheet name) to errors
      if (inherits(sheet_i, "error")) {
        stop("In workbook '", basename(wb_file),
          "' sheet '", sheets_to_read[i], "':",
          "\n ", sheet_i$message,
          call. = FALSE
        )
      }

      # Preallocate error messages for ith sheet

      err_i <- NULL


      # Check column names

      col_names_i <- names(sheet_i)


      # Check for duplicate column names

      dupl_cols <- unique(col_names_i[duplicated(tolower(col_names_i))])

      if (length(dupl_cols) > 0) {
        err_i <-
          c(
            err_i,
            paste0(
              "\n\nIn sheet '", sheets_to_read[i], "':",
              "\n Duplicate column names are not allowed: \n  '",
              paste0(dupl_cols, collapse = "'\n  '"), "'\n"
            )
          )
      }


      # Check for required columns

      missing_cols <- setdiff(schema_i$name, tolower(col_names_i))

      if (length(missing_cols) > 0) {
        err_i <-
          c(
            err_i,
            paste0(
              "\n\nIn sheet '", sheets_to_read[i], "':",
              "\n Required columns not found: \n  '",
              paste0(missing_cols, collapse = "'\n  '"), "'\n"
            )
          )
      }


      # Check and cast standard columns in standard sheets

      if (tolower(sheets_to_read[i]) %in% names(glatos_workbook_schema$v1.3)) {
        # Preallocate new object for parsed/cast values
        # keep as tibble here to preserve structure
        sheet_i2 <- sheet_i[]

        # Add attribute for warnings and errors
        # warnings are warning_cast_to_check
        # errors are error_input_class_skipped and error_cast_failed

        attr(sheet_i2, "warning_cast_to_check") <- list()
        attr(sheet_i2, "error_input_class_skipped") <- list()
        attr(sheet_i2, "error_cast_failed") <- list()


        # Coerce by expected column type

        # character
        char_cols <- col_names_i[tolower(col_names_i) %in%
          with(schema_i, name[type == "character"])]

        for (j in char_cols) {
          sheet_i2[[j]] <-
            if (nrow(sheet_i) > 0) {
              cast(sheet_i[[j]],
                new_class = "character"
              )
            } else {
              as.character(sheet_i[[j]])
            }
        }


        # numeric
        num_cols <- col_names_i[tolower(col_names_i) %in%
          with(schema_i, name[type == "numeric"])]

        for (j in num_cols) {
          sheet_i2[[j]] <-
            if (nrow(sheet_i) > 0) {
              cast(sheet_i[[j]],
                new_class = "numeric"
              )
            } else {
              as.numeric(sheet_i[[j]])
            }
        }


        # POSIXct

        # Only support POSIXct or character string that parses correctly
        # Do not accept numeric input.

        posix_cols <- col_names_i[tolower(col_names_i) %in%
          with(schema_i, name[type == "POSIXct"])]

        for (j in posix_cols) {
          # cast existing POSIXct or character to character
          sheet_i2[[j]] <-
            if (nrow(sheet_i) > 0) {
              cast(sheet_i[[j]],
                new_class = "character",
                old_class = c("character", "POSIXct")
              )
            } else {
              as.character(sheet_i[[j]])
            }


          # cast character to POSIXct, enforce timezone, but return UTC

          args_ij <- schema_i$args[schema_i$name == tolower(j)]

          # strip spaces (for formatting consistency)
          args_ij <- gsub(" ", "", args_ij)

          if (grepl("tz=REFCOL", args_ij)) {
            tz_col <- gsub("tz=REFCOL\\(|\\)", "", args_ij)

            tz_ij <- paste0("US/", sheet_i[[grep(tz_col,
              names(sheet_i),
              ignore.case = TRUE
            )]])
          } else {
            tz_ij <- gsub("tz=|tz=\"|\"", "", args_ij)
          }

          tzv_ij <- check_timezone(tz_ij, ignore.case = TRUE)

          tz_changed <- which(tzv_ij != tz_ij & !is.na(tzv_ij))

          tz_changed <- unique(cbind(tz_ij[tz_changed], tzv_ij[tz_changed]))

          if (length(tz_changed) > 0L) {
            warning(
              "The following time zones were changed in '",
              sheets_to_read[i], "' sheet:\n ",
              "'", j, "' column:\n",
              paste0(
                paste0(
                  "   ", shQuote(tz_changed[, 1]), " --> ",
                  shQuote(tz_changed[, 2])
                ),
                collapse = "\n"
              )
            )
          }


          tz_invalid <- unique(tz_ij[is.na(tzv_ij)])

          if (length(tz_invalid) > 0L) {
            warning(
              "Invalid time zones found in '",
              sheets_to_read[i], "' sheet:\n",
              paste0(paste0("  ", shQuote(tz_invalid)),
                collapse = "\n"
              )
            )
          }


          sheet_i2[[j]] <-
            if (nrow(sheet_i) > 0 & length(tz_invalid) == 0) {
              cast(sheet_i2[[j]],
                new_class = "POSIXct",
                old_class = c(
                  "character",
                  "POSIXct"
                ),
                tzv = tzv_ij
              )
            } else {
              as.POSIXct(NA, tz = "UTC")[0]
            }

          attr(sheet_i2[[j]], "tzone") <- "UTC"
        } # end j


        # Date

        # Only support POSIXct or character string that parses correctly
        # Do not accept numeric input.

        date_cols <- col_names_i[tolower(col_names_i) %in%
          with(schema_i, name[type == "Date"])]

        for (j in date_cols) {
          # cast existing POSIXct or character to character
          sheet_i2[[j]] <-
            if (nrow(sheet_i) > 0) {
              cast(sheet_i[[j]],
                new_class = "Date",
                old_class = c("character", "POSIXct")
              )
            } else {
              as.Date(NA)[0]
            }
        } # end j


        # Handle 'extra' columns (not in schema)
        # if multiple classes present in a column, cast to "highest-level" class

        extra_cols <- col_names_i[!(tolower(col_names_i) %in% schema_i$name)]

        if (read_all) {
          supported_classes <- c(
            "POSIXct",
            "Date",
            "numeric",
            "character",
            "logical"
          )

          for (j in extra_cols) {
            types_ij <- unique(unlist(lapply(sheet_i[[j]], class)))

            # expect 'highest-level' observed class
            type_exp <- intersect(supported_classes, types_ij)[1]

            # cast to type_exp
            # but if type_exp is POSIXct, cast to character

            sheet_i2[[j]] <-
              if (nrow(sheet_i2) > 0) {
                cast(sheet_i[[j]],
                  new_class = ifelse(type_exp == "POSIXct",
                    "character",
                    type_exp
                  )
                )
              } # if no rows, default to char
              else {
                as.character(NA)[0]
              }
          } # end j
        } else {
          std_names_i <- names(sheet_i2)[tolower(names(sheet_i2)) %in%
            schema_i$name]

          sheet_i2 <- sheet_i2[, std_names_i]
        }

        # Append to wb
        wb[[tolower(sheets_to_read[i])]] <- as.data.frame(sheet_i2)
      } else {
        # Append to wb
        wb[[tolower(sheets_to_read[i])]] <- as.data.frame(sheet_i)
      }


      if (simplify) {
        # Change names of all standard columns to lowercase

        std_names_index <-
          match(
            schema_i$name,
            tolower(names(wb[[tolower(sheets_to_read[i])]]))
          )

        names(wb[[tolower(sheets_to_read[i])]])[std_names_index] <- schema_i$name
      }


      if (i == 1) {
        wb_err <- err_i
      } else {
        wb_err <- c(wb_err, err_i)
      }
    } # end i


    # Collect warnings across sheets

    # attr(sheet_i2, "warning_cast_to_check"); #cast
    # attr(sheet_i2, "error_input_class_skipped")
    # attr(sheet_i2, "error_cast_failed")

    warn <- c(
      attr(wb[["locations"]], "warn"),
      attr(wb[["proposed"]], "warn"),
      attr(wb[["deployment"]], "warn"),
      attr(wb[["recovery"]], "warn"),
      attr(wb[["tagging"]], "warn")
    )

    if (length(warn) > 0) warning(warn)


    # Collect errors across sheets
    err <- c(
      wb_err,
      attr(wb[["locations"]], "err"),
      attr(wb[["proposed"]], "err"),
      attr(wb[["deployment"]], "err"),
      attr(wb[["recovery"]], "err"),
      attr(wb[["tagging"]], "err")
    )

    if (length(err) > 0) {
      stop(err,
        call. = FALSE
      )
    }

    if (simplify == FALSE) {
      return(wb)
    }


    # Merge to glatos_workbook list object

    ins_key <- list(
      by.x = c(
        "glatos_project", "glatos_array", "station_no",
        "consecutive_deploy_no", "ins_serial_no"
      ),
      by.y = c(
        "glatos_project", "glatos_array", "station_no",
        "consecutive_deploy_no", "ins_serial_number"
      )
    )

    wb$recovery$rec_row_id <- 1:nrow(wb$recovery) + xl_first_row

    wb2 <- with(wb, list(
      metadata = project,
      animals = tagging,
      receivers = merge(
        wb$deployment,
        recovery[
          ,
          unique(c(
            ins_key$by.y,
            setdiff(names(recovery), names(deployment))
          ))
        ],
        by.x = ins_key$by.x,
        by.y = ins_key$by.y,
        all.x = TRUE, all.y = TRUE
      )
    ))

    # Check for recoveries without matching deployments
    rec_missing_dep_rows <- which(is.na(wb2$receivers$glatos_deploy_date_time))

    n_rmdr <- length(rec_missing_dep_rows)

    if (n_rmdr > 0) {
      disp_rmdr <- if (n_rmdr > 6) {
        paste0(
          paste0(rec_missing_dep_rows[1:5], collapse = ", "),
          "...", tail(rec_missing_dep_rows, 1), " (+ ", n_rmdr - 6, " more)."
        )
      } else {
        paste0(rec_missing_dep_rows, collapse = ", ")
      }


      warning(
        n_rmdr, " row(s) in 'Recovery' sheet have no matching record in",
        " 'Deployment' sheet:",
        "\n See Recovery sheet, rows: ", disp_rmdr
      )
    }

    # drop temp col
    wb2$receivers$rec_row_id <- NULL


    # add location descriptions
    # note that sort arg prevents error with 0 rows in x, y
    wb2$receivers <- merge(
      x = wb2$receivers,
      y = wb$locations,
      by = "glatos_array",
      sort = (nrow(wb2$receivers) > 0 &
        nrow(wb$locations) > 0),
      all.x = TRUE
    )

    # Drop unwanted columns from receivers

    # coalesce deploy_date_time and glatos_deploy_date_time
    attr(wb2$receivers$glatos_deploy_date_time, "tzone") <- "UTC"
    ddt_na <- is.na(wb2$receivers$deploy_date_time)
    wb2$receivers$deploy_date_time[ddt_na] <-
      wb2$receivers$glatos_deploy_date_time[ddt_na]

    # coalesce recover_date_time and glatos_recover_date_time
    attr(wb2$receivers$glatos_recover_date_time, "tzone") <- "UTC"
    rdt_na <- is.na(wb2$receivers$recover_date_time)
    wb2$receivers$recover_date_time[rdt_na] <-
      wb2$receivers$glatos_recover_date_time[rdt_na]

    drop_cols_rec <- c(
      "glatos_deploy_date_time", "glatos_timezone",
      "glatos_recover_date_time"
    )
    wb2$receivers <- wb2$receivers[, -match(
      drop_cols_rec,
      names(wb2$receivers)
    )]

    # sort rows by deploy_date_time
    if (nrow(wb2$receivers) > 0) {
      wb2$receivers <- wb2$receivers[with(
        wb2$receivers,
        order(deploy_date_time, glatos_array, station_no)
      ), ]
      row.names(wb2$receivers) <- NULL
    }


    # Drop unwanted columns from animals

    # coalesce release_date_time and utc_release_date_time
    attr(wb2$animals$glatos_release_date_time, "tzone") <- "UTC"
    ardt_na <- is.na(wb2$animals$utc_release_date_time)
    wb2$animals$utc_release_date_time[ardt_na] <-
      wb2$animals$glatos_release_date_time[ardt_na]

    drop_cols_anim <- c("glatos_release_date_time", "glatos_timezone")
    wb2$animals <- wb2$animals[, -match(
      drop_cols_anim,
      names(wb2$animals)
    )]

    # sort animals
    # sort rows by deploy_date_time
    wb2$animals <- wb2$animals[with(
      wb2$animals,
      order(utc_release_date_time, animal_id)
    ), ]
    row.names(wb2$animals) <- NULL

    # create animal_id if missing
    anid_na <- is.na(wb2$animals$animal_id)
    wb2$animals$animal_id[anid_na] <- with(
      wb2$animals[anid_na, ],
      paste0(tag_code_space, "-", tag_id_code)
    )

    # Append new sheets if required
    if (read_all) {
      for (i in 1:length(extra_sheets)) {
        wb2[extra_sheets[i]] <- wb[extra_sheets[i]]
      }
    }
  }

  #-end v1.3, v1.4----------------------------------------------------------------

  # assign classes
  wb2$animals <- as_glatos_animals(wb2$animals)
  wb2$receivers <- as_glatos_receivers(wb2$receivers)

  wb2 <- glatos_workbook(wb2)

  return(wb2)
}


#' Cast a list of scalars to a new class
#'
#' Cast a list of scalars, with potentially mixed classes, to a new class.
#'
#' @param x A list of scalars.
#'
#' @param new_class A text string with name of new class.
#'
#' @param old_class A character vector with names of classes in `x` that will
#'  be cast to `new_class`. Any record with a different class will result in NA
#'  (with error). Default value is `c("logical", "numeric", "character", "Date",
#'  "POSIXct")`.
#'
#' @param defer_exceptions If TRUE (default value) then errors and warnings will
#'  be returned as attributes with prefix "error_" or "warning_".
#'
#' @param ... Other arguments passed to the casting function (e.g., `tz =
#'   "US/Eastern"` when `new_class` is `POSIXct`).
#'
#' @details
#'  Written specifically for `readxl::read_excel` with `col_types = "list"` to
#'  evaluate class of each record/row independently and then present user with
#'  a single report of all errors (instead of sequential one. at. a. time).
#'
#' @returns A vector of length same as `x` and class as `new_class`.
#'
#' @examples
#'
#' x <- list(TRUE, "A", NA, 3.1415, Sys.time(), Sys.Date(), "1997-05-13 12:43:21")
#'
#' sapply(x, class)
#'
#' cast(x, "character")
#'
#' cast(x, "numeric")
#'
#' cast(x, "Date")
#'
#' cast(x, "POSIXct")
#'
#' cast(x, "POSIXct", tz = "US/Pacific")
#'
#' # separate tz for each element
#' cast(x, "POSIXct", tz = c("US/Eastern", rep("US/Pacific", 5)))
#'
#' # Only cast from if class is character
#' cast(x, "POSIXct", old_class = "character")
#'
#' # Only cast from if class is character or POSIXct
#' cast(x, "character", old_class = c("character", "POSIXct"))
#'
#' \dontrun{
#'
#' # Bad (unsupported) new_class
#' cast(x, "foo")
#'
#' # Bad (unsupported) old_class
#' cast(x, "character", old_class = c("character", "foo"))
#' }
#'
#' @export
cast <- function(x,
                 new_class,
                 old_class = c(
                   "logical",
                   "character",
                   "numeric",
                   "Date",
                   "POSIXct"
                 ),
                 defer_exceptions = TRUE,
                 ...) {
  # args_in <- list()
  args_in <- list(...)

  x_class <- sapply(x, function(x) class(x)[1])
  x_na <- sapply(x, is.na)

  # Check if new_class is valid
  # (only default values of old_class are supported)
  invalid_new_class <- setdiff(
    new_class,
    eval(formals(cast)$old_class)
  )

  if (length(invalid_new_class) > 0) {
    stop(
      "Invalid 'new_class': \"",
      paste0(invalid_new_class,
        collapse = ", "
      ), "\""
    )
  }

  # Check if old_class is valid
  # (only default values of old_class are supported)
  invalid_old_class <- setdiff(
    old_class,
    eval(formals(cast)$old_class)
  )

  if (length(invalid_old_class) > 0) {
    stop(
      "Invalid 'old_class': \"",
      paste0(invalid_old_class,
        collapse = ", "
      ), "\""
    )
  }

  # Select casting function; based on new_class
  cast_with <- switch(new_class,
    logical = as.logical,
    numeric = as.numeric,
    character = as.character,
    Date = as.Date,
    POSIXct = as.POSIXct
  )


  # Cast; return NA on error
  x2 <- suppressWarnings(
    lapply(seq_along(x), function(k) {
      if (inherits(x[[k]], old_class)) {
        args_in_k <- args_in
        if (length(args_in_k) > 0) {
          for (m in 1:length(args_in_k)) args_in_k[[m]] <- args_in[[m]][k]
        }
        tryCatch(
          do.call(cast_with, c(
            list(x = x[[k]]),
            args_in_k
          )),
          error = function(e) cast_with(NA)
        )
      } else {
        cast_with(NA)
      }
    })
  )

  # combine
  x2 <- do.call(c, x2)


  # Assign code indicating status of the cast
  cast_status <- data.table::fcase(
    # 1 = success; record not cast because old and new class are same
    # or input was NA
    # result is same as input
    (x_class == new_class & x_class %in% old_class) | x_na,
    1, # old and new class same
    # 2 = apparent success (not NA before and after)
    !x_na & !is.na(x2),
    2, # apparent successful cast (non-NA result)
    # 3 = record not cast; unsupported class (not in old_class)
    # result is NA
    !(x_class %in% old_class),
    3, # unsupported class
    # 4 = failed cast (NA after cast; but not before)
    # result in NA
    !x_na & is.na(x2),
    4, # failed cast
    default = NA
  )


  # Handle exceptions (warnings and errors)

  # preallocate vector for each specific error and warning by type

  error_input_class_skipped <- NULL
  error_cast_failed <- NULL
  warning_cast_to_check <- NULL

  # function to print a message that shows only first n records... & last
  print_first_n <- function(x, n = 3) {
    if (length(x) <= n) {
      paste(x, collapse = ", ")
    } else {
      paste0(
        paste(x[1]:min(length(x), n), collapse = ", "),
        ", ...",
        utils::tail(x, 1),
        " (+ ", length(x) - n - 1, " others)"
      )
    }
  }


  # Invalid input class (cast not supported)
  if (any(cast_status == 3)) {
    eiic_index <- which(cast_status == 3)

    plural <- "s"[length(eiic_index) > 1]

    error_input_class_skipped <- c(
      error_input_class_skipped,
      paste0(
        "row", plural, " ",
        print_first_n(eiic_index, 5)
      )
    )
  }

  # Cast failed (bad input value?)
  if (any(cast_status == 4)) {
    ecf_index <- which(cast_status == 4)

    plural <- "s"[length(ecf_index) > 1]

    error_cast_failed <- c(
      error_cast_failed,
      paste0(
        "row", plural, " ",
        print_first_n(ecf_index, 5)
      )
    )
  }


  # Cast succeeded but needs to be checked

  if (any(cast_status == 2)) {
    wctc_index <- which(cast_status == 2)

    plural <- "s"[length(wctc_index) > 1]

    warning_cast_to_check <- c(
      warning_cast_to_check,
      paste0(
        "row", plural, " ",
        print_first_n(wctc_index, 5)
      )
    )
  }


  if (!defer_exceptions) {
    if (length(warning_cast_to_check) > 0) {
      warning(
        "\nCast was successful but should be verified: ",
        paste0(warning_cast_to_check, collapse = "\n ")
      )
    }

    if (length(error_input_class_skipped) > 0 |
      length(error_cast_failed) > 0) {
      stop(c(
        paste0(
          "\n\nInvalid input class: ",
          paste(error_input_class_skipped, collapse = "\n")
        ),
        paste0(
          "\n\nCast failed: ",
          paste(error_cast_failed, collapse = "\n")
        )
      ))
    }
  }

  return(structure(x2,
    error_input_class_skipped = error_input_class_skipped,
    error_cast_failed = error_cast_failed,
    warning_cast_to_check = warning_cast_to_check
  ))
}


#' Identify and check GLATOS workbook file version
#'
#' Identify and check version of a GLATOS workbook file (xlsm or xlsx) based on
#' its structure.
#'
#' @inheritParams read_glatos_workbook
#'
#' @returns A character string with version number ("1.3", "1.4").
#'
#' @examples
#'
#' # Example 1: Version 1.3 (xlsm)
#'
#' # get path to example GLATOS Data Workbook
#' wb_file <- system.file("extdata",
#'   "walleye_workbook.xlsm",
#'   package = "glatos"
#' )
#'
#' identify_workbook_version(wb_file)
#'
#'
#' # Example 2: Version 1.4 (xlsx)
#'
#' wb2_file <- system.file("extdata",
#'   "walleye_workbook.xlsx",
#'   package = "glatos"
#' )
#'
#' identify_workbook_version(wb2_file)
#'
#' @export
identify_workbook_version <- function(wb_file,
                                      wb_version = NULL) {
  # Get sheet names
  sheets <- tolower(readxl::excel_sheets(wb_file))


  # v1.3 and v1.4 have same sheet names
  if (all(names(glatos_workbook_schema$v1.3) %in% sheets)) {
    # Identify version based on structure of Project sheet
    # Get project data
    prj <- tryCatch(
      readxl::read_excel(wb_file,
        sheet = "Project",
        col_names = FALSE,
        .name_repair = "minimal"
      ),
      error = function(e) {
        if (e$message ==
          "Expecting a single string value: [type=character; extent=0].") {
          stop(
            "There was a problem reading from input file specified. It may ",
            "be protected.\n  Try again after opening, saving, and closing the ",
            "file."
          )
        } else {
          stop(e)
        }
      }
    )

    if (all(prj[[1]][1:3] == c(
      "GLATOS Project Code:",
      "Principal Investigator (PI):",
      "PI E-Mail Address:"
    ))) {
      if (ncol(prj) > 2 & nrow(prj) > 3) {
        if (prj[[3]][5] == "DataForm Version 1.4") {
          wbv <- "1.4"
        }
      } else {
        wbv <- "1.3"
      }
    } else {
      wbv <- NULL
    }
  } else {
    wbv <- NULL
  }

  if (is.null(wbv)) {
    stop(
      "Workbook version could not be identified. Double check ",
      "that you are using a standard GLATOS Workbook file. The ",
      "names of sheets must match standard file.",
      call. = FALSE
    )
  }

  # Check against input, if given

  if (is.null(wb_version) == FALSE) {
    if (wb_version != wbv) {
      stop("Workbook version looks like ",
        wbv, ", which is different than input ",
        "'wb_version' (", wb_version, ").",
        call. = FALSE
      )
    }
  }

  return(wbv)
}


#' Read Project sheet from GLATOS workbook file
#'
#' Read Project sheet from GLATOS workbook file
#'
#' @inheritParams read_glatos_workbook
#'
#' @returns A list with six elements:
#' 1. __project_code:__ GLATOS Project Code.
#' 2. __principle_investigator:__ Name of Principle Investigator.
#' 3. __pi_email:__ Email address of Principle Investigator.
#' 4. __source_file:__ Name of input `wb_file`.
#' 5. __wb_version:__ Version of GLATOS workbook file.
#' 6. __created:__ Timestamp when source file metadata was changed
#'  (from `file.info(wb_file)$ctime`).
#'
#' @examples
#'
#' # Example 1: Version 1.3 (xlsm)
#'
#' # get path to example GLATOS Data Workbook
#' wb_file <- system.file("extdata",
#'   "walleye_workbook.xlsm",
#'   package = "glatos"
#' )
#'
#' read_workbook_project(wb_file)
#'
#'
#' # Example 2: Version 1.4 (xlsx)
#'
#' wb2_file <- system.file("extdata",
#'   "walleye_workbook.xlsx",
#'   package = "glatos"
#' )
#'
#' read_workbook_project(wb2_file)
#'
#' @export
read_workbook_project <- function(wb_file) {
  # Get workbook version
  wb_version <- identify_workbook_version(wb_file)

  # Get project data
  prj_raw <- tryCatch(
    readxl::read_excel(wb_file,
      sheet = "Project",
      col_names = FALSE,
      .name_repair = "minimal"
    ),
    error = function(e) {
      if (e$message ==
        "Expecting a single string value: [type=character; extent=0].") {
        stop(
          "There was a problem reading from input file specified. It may ",
          "be protected.\n  Try again after opening, saving, and closing the ",
          "file."
        )
      } else {
        stop(e)
      }
    }
  )

  # Restructure as list
  prj <- list(
    project_code = prj_raw[[2]][1],
    principle_investigator = prj_raw[[2]][2],
    pi_email = prj_raw[[2]][3],
    source_file = basename(wb_file),
    wb_version = wb_version,
    created = file.info(wb_file)$ctime
  )

  return(prj)
}


#' Check a time zone string against OlsonNames()
#'
#' @param tz a character string. The time zone specification to be used for the
#'   conversion. Only values in [OlsonNames()] are allowed. `""` is not allowed.
#'
#' @param ignore.case logical. if FALSE, the pattern matching is case sensitive
#'   and if TRUE (default), case is ignored during matching. Passed to [grep()].
#'
#' @returns If `tz` is valid (depends on `ignore.case`), then it is returned.
#'   Otherwise, `NA` is returned.
#'
#' @examples
#'
#' x <- c("UTC", "US/Eastern", "US/EASTERN", "foo")
#'
#' check_timezone(tz = x)
#'
#' check_timezone(tz = x, ignore.case = TRUE)
#'
check_timezone <- function(tz, ignore.case = FALSE) {
  tz2 <- sapply(paste0("^", tz, "$"),
    FUN = grep,
    x = OlsonNames(),
    ignore.case = ignore.case,
    value = TRUE
  )

  tz2 <- unname(sapply(
    tz2,
    function(x) if (length(x) == 0L) NA_character_ else x
  ))

  return(tz2)
}
