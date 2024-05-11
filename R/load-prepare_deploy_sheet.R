#' Loads the OTN receiver deployment metadata sheet to prepare it for use in
#' \code{convert_otn_to_att}
#'
#' @param path the path to the deployment sheet
#'
#' @param header_line what line the headers are on
#'
#' @param sheet_name the sheet name or number containing the metadata
#'
#' @param combine_arr_stn whether or not to to join the station and array columns. Format depends on OTN node
#'
#'
#' @details The function takes the path to the deployment sheet, what line to start
#' reading from, and what sheet in the excel file to use. It converts column names
#' to be used by \code{convert_otn_to_att}.
#'
#' @author Ryan Gosse
#'
#' @return a data.frame created from the excel file.
#'
#' @examples
#'
#' #--------------------------------------------------
#' # EXAMPLE #1 - loading from NSBS simplified Deployments
#'
#' library(glatos)
#' deploy_path <- system.file("extdata", "hfx_deploy_simplified.xlsx",
#'   package = "glatos"
#' )
#'
#' deploy <- prepare_deploy_sheet(deploy_path,
#'   header_line = 1,
#'   sheet_name = 1
#' )
#'
#' @export

prepare_deploy_sheet <- function(path, header_line = 5, sheet_name = 1, combine_arr_stn = TRUE) {
  ##  Declare global variables for NSE & R CMD check
  DEPLOY_LAT <- DEPLOY_LONG <- INS_MODEL_NO <-
    `DEPLOY_DATE_TIME   (yyyy-mm-ddThh:mm:ss)` <-
    `RECOVER_DATE_TIME (yyyy-mm-ddThh:mm:ss)` <- STATION_NO <- 
    OTN_ARRAY <- station <- ins_model_no <- deploy_lat <- deploy_long <-
    deploy_date_time <- recover_date_time <- NULL
  
  deploy_sheet <- readxl::read_excel(path,
    sheet = sheet_name,
    skip = header_line - 1,
    col_names = TRUE
  )

  deploy_sheet <- deploy_sheet %>% dplyr::rename(
    deploy_lat = DEPLOY_LAT,
    deploy_long = DEPLOY_LONG,
    ins_model_no = INS_MODEL_NO,
    deploy_date_time = `DEPLOY_DATE_TIME   (yyyy-mm-ddThh:mm:ss)`,
    recover_date_time = `RECOVER_DATE_TIME (yyyy-mm-ddThh:mm:ss)`,
    station = STATION_NO
  )
  if (combine_arr_stn) {
    deploy_sheet <- deploy_sheet %>% dplyr::mutate(
      station = paste(OTN_ARRAY, station, sep = "")
    )
  }
  deploy_sheet <- deploy_sheet %>% dplyr::select(
    station, ins_model_no, deploy_lat, deploy_long,
    deploy_date_time, recover_date_time
  )
  return(deploy_sheet)
}
