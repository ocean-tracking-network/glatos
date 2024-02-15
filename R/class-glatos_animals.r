#' Construct, check, and validate a glatos_animals object
#'
#' @description Creates, checks, or validates a glatos_animals object.
#'
#' @param ... Named vectors, minimally one for each required column of the
#'   specified class:
#'
#'   \describe{
#'   \item{`animal_id`}{must be character, uniquely identifies each animal.}
#'   \item{`tag_id_code`}{must be character, identification code transmitted by the tag
#'   (e.g., "1363" for Innovasea PPM coding").}
#'   \item{`tag_code_space`}{must be character, code space transmitted by the tag (e.g.,
#'   "A69-9002").}
#'   \item{`utc_release_date_time`}{must be POSIXct, timestamp (in UTC) when animal was
#'   released (i.e., start of telemetry sampling interval.)}
#'   }
#'
#' @param x A data.frame or object that inherits from data.frame (e.g.,
#'   data.table, tibble) and contains all required columns (see `...`).
#'
#' @param validate logical, indicates if column names and classes should be
#'   checked against requirements.
#'
#' @examples
#'
#' #  glatos_animals
#' x <- data.frame(
#'   animal_id = c("120", "107", "109"),
#'   tag_id_code = c("32024", "32012", "32014"),
#'   tag_code_space = c("A69-9001", "A69-9001", "A69-9001"),
#'   utc_release_date_time = as.POSIXct(
#'     c(
#'       "2011-03-28 00:00:00",
#'       "2011-03-28 00:01:00",
#'       "2011-03-28 00:05:00"
#'     ),
#'     tz = "UTC"
#'   ),
#'   release_latitude = c(41.56093, 41.56093, 41.56093),
#'   release_longitude = c(-83.645, -83.645, -83.645)
#' )
#'
#' ga_df1 <- glatos_animals(
#'   animal_id = x$animal_id,
#'   tag_id_code = x$tag_id_code,
#'   tag_code_space = x$tag_code_space,
#'   utc_release_date_time = x$utc_release_date_time
#' )
#'
#'
#' # as_glatos_animals
#' ga_df2 <- as_glatos_animals(x)
#'
#'
#' # sf input
#'
#' library(sf)
#'
#' x_sf <- sf::st_as_sf(x,
#'   coords = c("release_longitude", "release_latitude")
#' )
#'
#' ga_sf <- as_glatos_animals(x_sf)
#'
#'
#' # tibble input
#' library(tibble)
#'
#' x_tbl <- as_tibble(x)
#'
#' ga_tbl <- as_glatos_animals(x_tbl)
#'
#'
#' # All below will error as invalid
#' 
#' \dontrun{
#' # data.frame input; missing column name
#' library(dplyr) # for rename
#' x2 <- rename(x,
#' fish_name = animal_id,
#' release_timestamp = utc_release_date_time
#' )
#'
#' ga2 <- as_glatos_animals(x2)
#'
#'
#' data.frame input; wrong column class
#' x3 <- mutate(x,
#' animal_id = as.integer(animal_id),
#' utc_release_date_time = as.character(utc_release_date_time)
#' )
#'
#' ga3 <- as_glatos_animals(x3)
#' }
#'
#' # Validation and checking
#'
#' validate_glatos_animals(x)
#'
#' is_glatos_animals(x) # FALSE
#'
#' is_glatos_animals(ga_df1) # TRUE

#' @section Construction: `glatos_animals()` creates a `glatos_animals` from
#'   individual vectors (one for each column) and optionally checks for required
#'   column names and classes using `validate_glatos_animals()`.
#' @export
glatos_animals <- function(..., validate = TRUE) {
  inargs <- list(...)

  x <- as.data.frame(inargs)

  x <- as_glatos_animals(x, validate = validate)

  return(x)
}

#' @section Coercion: `as_glatos_animals()` coerces a data.frame, or object that
#'   inherits from data.frame, to `glatos_animals` and optionally checks for
#'   required column names and classes using `validate_glatos_animals()`.
#' @rdname glatos_animals
#' @export
as_glatos_animals <- function(x, validate = TRUE) {
  # Input must inherit from data frame
  if (!inherits(x, "data.frame")) stop("Input x must inherit from data.frame.")

  # add new class as first but keep existing (e.g., data.frame)
  class(x) <- c("glatos_animals", class(x))

  if (validate) validate_glatos_animals(x)

  return(x)
}

#' @section Validation:
#' `is_glatos_animals()` checks class attribute for `"glatos_animals"`
#' @rdname glatos_animals
#' @export
is_glatos_animals <- function(x) inherits(x, "glatos_animals")


#' @section Validation:
#' `validate_glatos_animals()` checks for required column names and classes
#' @rdname glatos_animals
#' @export
validate_glatos_animals <- function(x) {
  req_cols <- list(
    animal_id = "character",
    tag_id_code = "character",
    tag_code_space = "character",
    utc_release_date_time = "POSIXct"
  )

  glatos_check_col_names(x, req_cols)

  # Check column classes

  glatos_check_col_classes(x, req_cols)

  return(TRUE)
}

#' Check column names and classes of a list or data.frame against requirements
#'
#' @param x a data.frame, or object that inherits from data.frame, to check
#'
#' @param req_cols a named list containing a character string with the class of
#'   each required column; each element name is a required column name
#'
#' @export
glatos_check_col_names <- function(x, req_cols) {
  # Check column names
  missing_cols <- setdiff(names(req_cols), names(x))

  if (length(missing_cols) > 0) {
    stop("Required column(s) missing from ",
      "input x:\n ",
      paste0(missing_cols, collapse = "\n "),
      call. = FALSE
    )
  }

  return(TRUE)
}


#' Check column classes of a list or data.frame against requirements
#'
#' @rdname glatos_check_col_names
#'
#' @export
glatos_check_col_classes <- function(x, req_cols) {
  wrong_class <- sapply(
    seq_along(req_cols),
    function(k) {
      !inherits(
        x = x[[names(req_cols[k])]],
        what = req_cols[[k]]
      )
    }
  )

  wrong_class <- names(req_cols)[wrong_class]

  if (length(wrong_class) > 0) {
    stop("The following column(s) have wrong class: ",
      "\n ",
      paste0(
        paste0(
          wrong_class,
          " (must be '",
          req_cols[wrong_class],
          "')"
        ),
        collapse = "\n "
      ),
      call. = FALSE
    )
  }

  return(TRUE)
}
