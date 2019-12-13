#' Loads the OTN receiver deployment metadata sheet to prepare it for use in 
#' \code{convert_otn_to_att}
#' 
#' @param path the path to the deployment sheet
#' 
#' @param start what line to start reading data in from
#' 
#' @param sheet the sheet name or number containing the metadata
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
#'                         package = "glatos")
#'
#' deploy <- prepare_deploy_sheet(deploy_path, 5, 2)
#' 
#' @export

prepare_deploy_sheet <- function(path, start = 1, sheet = 1) {
    sheet <- readxl::read_excel(path, sheet = sheet, skip = start - 1)
    sheet <- sheet %>% dplyr::rename(
        deploy_lat = DEPLOY_LAT,
        deploy_long = DEPLOY_LONG,
        ins_model_no = INS_MODEL_NO,
        deploy_date_time = `DEPLOY_DATE_TIME   (yyyy-mm-ddThh:mm:ss)`,
        recover_date_time = `RECOVER_DATE_TIME (yyyy-mm-ddThh:mm:ss)`,
        
    )
    sheet <- sheet %>% dplyr::mutate(
        station = paste(OTN_ARRAY, STATION_NO, sep = '')
    )
    sheet <- sheet %>% select(
        station, ins_model_no, deploy_lat, deploy_long,
        deploy_date_time, recover_date_time
    )
    return(sheet)

}