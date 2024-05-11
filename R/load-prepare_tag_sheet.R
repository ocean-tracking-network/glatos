#' Loads the OTN tagging metadata sheet to prepare it for use in
#' `convert_otn_to_att`
#'
#' @param path the path to the tagging sheet
#'
#' @param header_line what line the headers are on
#'
#' @param sheet_name the sheet name or number containing the metadata
#'
#'
#' @details The function takes the path to the tagging sheet, what line to start
#' reading the headers from, and what sheet in the excel file to use. It converts column names
#' to be used by `convert_otn_to_att`.
#'
#' @author Ryan Gosse
#'
#' @return a data.frame created from the excel file.
#'
#' @examples
#'
#' #--------------------------------------------------
#' # EXAMPLE #1 - loading from NSBS tagging
#'
#' library(glatos)
#' tag_path <- system.file("extdata", "otn_nsbs_tag_metadata.xls",
#'   package = "glatos"
#' )
#'
#' tags <- prepare_tag_sheet(tag_path, 5, 2)
#'
#' @export

prepare_tag_sheet <- function(path, header_line = 5, sheet_name = 2) {
  ##  Declare global variables for NSE & R CMD check
  TAG_CODE_SPACE <- TAG_ID_CODE <- EST_TAG_LIFE <- UTC_RELEASE_DATE_TIME <- 
    SEX <- RELEASE_LATITUDE <- RELEASE_LONGITUDE <- SCIENTIFIC_NAME <- NULL
  
  tag_sheet <- readxl::read_excel(path, sheet = sheet_name, skip = header_line - 1)
  tag_sheet <- tag_sheet %>% dplyr::mutate(
    transmitter_id = paste(TAG_CODE_SPACE, TAG_ID_CODE, sep = "-"),
    est_tag_life = as.integer(purrr::map(EST_TAG_LIFE, convert_life_to_days))
  )
  tag_sheet <- tag_sheet %>% dplyr::rename(
    animal_id = "ANIMAL_ID   (floy tag ID, pit tag code, etc.)",
    time = UTC_RELEASE_DATE_TIME,
    sex = SEX,
    latitude = RELEASE_LATITUDE,
    longitude = RELEASE_LONGITUDE,
    sci_name = SCIENTIFIC_NAME
  )
  return(tag_sheet)
}

# For converting tag life column to a count of days
convert_life_to_days <- function(tagLife) {
  if (!grepl("[a-zA-Z]", tagLife)) {
    return(as.integer(tagLife))
  } else if (grepl("days?", tolower(tagLife))) {
    days <- gsub("days?", "", tolower(tagLife))
    return(as.integer(days))
  } else {
    stop(
      sprintf(
        "Cannot convert %s to time days. Please change the est_tag_life in the tagging metadata sheet to days",
        tagLife
      )
    )
  }
}
