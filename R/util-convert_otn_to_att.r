#' Convert detections, tagging metadata, and deployment metadata to a format
#' that ATT accepts.
#'
#' Convert \code{glatos_detections}, OTN tagging metadata and OTN deployment
#' metadata to \code{ATT} format for use in the Animal Tracking Toolbox
#' (\url{https://github.com/vinayudyawer/ATT}).
#'
#' @param detectionObj a data frame from \code{read_otn_detections}
#'
#' @param taggingSheet a data frame from \code{prepare_tag_sheet}
#'
#' @param deploymentObj a data frame from \code{read_otn_deployments}
#'
#' @param deploymentSheet a data frame from \code{prepare_deploy_sheet}
#'
#' @param timeFilter Whether the data should be filtered using the deployment
#'   and recovery/last download times of receivers. Defaults to TRUE, if not all
#'   receiver metadata is available, this should be set to FALSE otherwise there
#'   will be data loss.
#'
#' @param crs a object of class `crs` (see [sf::st_crs][st_crs] with geographic
#'   coordinate system for all spatial information (latitude/longitude). If none
#'   provided or `crs` is not recognized, defaults to WGS84 (EPSG:4326).
#'
#'
#' @details This function takes 3 data frames containing detections, tagging
#'   metadata, and deployment metadata from either \code{read_otn_deployments}
#'   or \code{prepare_deploy_sheet} and transforms them into 3
#'   \code{tibble} objects inside of a list. The input that AAT uses to
#'   get this data product is located here:
#'   https://github.com/vinayudyawer/ATT/blob/master/README.md and our mappings
#'   are found here: https://github.com/ocean-tracking-network/glatos/issues/75#issuecomment-982822886
#'   in a comment by Ryan Gosse.
#'
#' @author Ryan Gosse
#'
#' @return a list of 3 tibbles containing tag detections, tag metadata,
#'   and station metadata, to be ingested by VTrack/ATT
#'
#' @examples
#' \dontrun{
#' #--------------------------------------------------
#' # EXAMPLE #1 - loading from Deployment Object
#'
#' library(glatos)
#'
#' dets_path <- system.file("extdata", "blue_shark_detections.csv",
#'   package = "glatos"
#' )
#' deploy_path <- system.file("extdata", "hfx_deployments.csv",
#'   package = "glatos"
#' )
#' tag_path <- system.file("extdata", "otn_nsbs_tag_metadata.xls",
#'   package = "glatos"
#' )
#'
#' dets <- read_otn_detections(dets_path)
#' tags <- prepare_tag_sheet(tag_path, 5, 2)
#' deploy <- read_otn_deployments(deploy_path)
#'
#' ATTdata <- convert_otn_to_att(dets, tags, deploymentObj = deploy)
#' }
#' #--------------------------------------------------
#' # EXAMPLE #2 - loading from Deployment Sheet
#'
#' library(glatos)
#'
#' dets_path <- system.file("extdata", "blue_shark_detections.csv",
#'   package = "glatos"
#' )
#' deploy_path <- system.file("extdata", "hfx_deploy_simplified.xlsx",
#'   package = "glatos"
#' )
#' tag_path <- system.file("extdata", "otn_nsbs_tag_metadata.xls",
#'   package = "glatos"
#' )
#'
#' dets <- read_otn_detections(dets_path)
#' tags <- prepare_tag_sheet(tag_path, 5, 2)
#' deploy <- prepare_deploy_sheet(deploy_path, 1, 1)
#'
#' ATTdata <- convert_otn_to_att(dets, tags, deploymentSheet = deploy)
#'
#' @export

convert_otn_to_att <- function(detectionObj,
                               taggingSheet,
                               deploymentObj = NULL,
                               deploymentSheet = NULL,
                               timeFilter = TRUE,
                               crs = sf::st_crs(4326)) {
  ##  Declare global variables for R CMD check
  station <- receiver_sn <- deploy_lat <- deploy_long <- detection_timestamp_utc <-
    deploy_date_time <- recover_date_time <- last_download <- instrumenttype <-
    ins_model_no <- Tag.ID <- Sex <- NULL


  if (is.null(deploymentObj) && is.null(deploymentSheet)) {
    stop("Deployment data must be supplied by either 'deploymentObj' or 'deploymentSheet'")
  } else if ((!is.null(deploymentObj)) && (!is.null(deploymentSheet))) {
    stop("Deployment data must be supplied by either 'deploymentObj' or 'deploymentSheet', not both")
  } else if (!is.null(deploymentSheet)) {
    deploymentObj <- deploymentSheet
  }

  detectionObj <- detectionObj %>% # Remove (lost/found)
    dplyr::mutate(
      station = gsub("\\(lost\\/found\\)", "", station),
      receiver_sn = gsub("\\(lost\\/found\\)", "", receiver_sn)
    )

  transmitters <-
    if (all(grepl("-", detectionObj$transmitter_id, fixed = TRUE))) {
      detectionObj$transmitter_id
    } else {
      concat_list_strings(detectionObj$transmitter_codespace, detectionObj$transmitter_id)
    }

  tagMetadata <- unique(dplyr::tibble( # Start building Tag.Metadata table
    Tag.ID = detectionObj$animal_id,
    Transmitter = as.factor(transmitters),
    Common.Name = as.factor(detectionObj$common_name_e),
    Sci.Name = as.factor(detectionObj$scientificname)
  ))

  tagMetadata <- unique(tagMetadata) # Cut out dupes

  detectionObj <- dplyr::left_join(detectionObj, taggingSheet %>% dplyr::select(-c("animal_id")), by = "transmitter_id")

  detectionObj <- dplyr::left_join(detectionObj %>% dplyr::select(-deploy_lat, -deploy_long), deploymentObj, by = "station")
  if (timeFilter) {
    if (is.null(deploymentSheet)) {
      detectionObj <- detectionObj %>% dplyr::filter(
        detection_timestamp_utc >= deploy_date_time,
        detection_timestamp_utc <= dplyr::coalesce(recover_date_time, last_download),
        instrumenttype == "rcvr"
      )
    } else {
      detectionObj <- detectionObj %>% dplyr::filter(
        detection_timestamp_utc >= deploy_date_time,
        detection_timestamp_utc <= recover_date_time | recover_date_time %in% c(NA)
      )
    }
  }


  detectionObj <- detectionObj %>%
    dplyr::mutate(
      ReceiverFull = paste(ins_model_no, receiver_sn, sep = "-")
    )

  detectionObj$est_tag_life[detectionObj$est_tag_life == "NULL"] <- NA

  releaseData <- dplyr::tibble( # Get the rest from detectionObj
    Tag.ID = detectionObj$animal_id,
    Tag.Project = as.factor(detectionObj$collectioncode),
    Release.Latitude = as.double(detectionObj$latitude),
    Release.Longitude = as.double(detectionObj$longitude),
    Release.Date = as.Date(detectionObj$time),
    Sex = as.factor(detectionObj$sex),
    Tag.Life = as.integer(detectionObj$est_tag_life)
  ) %>% dplyr::filter(!Tag.ID %in% NA)

  releaseData <- dplyr::mutate(releaseData,
    # Convert sex text and null missing columns
    Sex = purrr::map(Sex, convert_sex),
    Tag.Status = as.factor(NA),
    Bio = as.factor(NA)
  ) %>% unique()

  detections <- dplyr::tibble(
    Date.Time = detectionObj$detection_timestamp_utc,
    Transmitter = as.factor(detectionObj$transmitter_id),
    Station.Name = as.factor(detectionObj$station),
    Receiver = as.factor(detectionObj$ReceiverFull),
    Latitude = as.double(detectionObj$deploy_lat),
    Longitude = as.double(detectionObj$deploy_long),
    Sensor.Value = as.integer(detectionObj$sensorvalue),
    Sensor.Unit = as.factor(detectionObj$sensorunit)
  )

  tagMetadata <- dplyr::left_join(tagMetadata, releaseData, by = "Tag.ID")

  animal_sex <- tagMetadata$Sex
  animal_sex[animal_sex == "NULL"] <- NA
  tagMetadata <- tagMetadata %>% dplyr::mutate(
    Sex = as.factor(as.character(animal_sex))
  )

  stations <- unique(dplyr::tibble(
    Station.Name = as.factor(detectionObj$station),
    Receiver = as.factor(detectionObj$ReceiverFull),
    Installation = as.factor(NA),
    Receiver.Project = as.factor(detectionObj$collectioncode),
    Deployment.Date = detectionObj$deploy_date_time,
    Recovery.Date = detectionObj$recover_date_time,
    Station.Latitude = as.double(detectionObj$deploy_lat),
    Station.Longitude = as.double(detectionObj$deploy_long),
    Receiver.Status = as.factor(NA)
  ))

  att_obj <- list(
    Tag.Detections = detections,
    Tag.Metadata = tagMetadata,
    Station.Information = stations
  )

  class(att_obj) <- "ATT"

  if (inherits(crs, "crs")) {
    attr(att_obj, "crs") <- crs
  } else {
    message("Geographic projection for detection positions not recognised, reverting to WGS84 global coordinate reference system")
    attr(att_obj, "crs") <- eval(formals()$crs)
  }

  return(att_obj)
}


# Simple query to WoRMS based on the common name and returns the sci name
query_worms_common <- function(commonName) {
  url <- utils::URLencode(
    sprintf(
      "https://www.marinespecies.org/rest/AphiaRecordsByVernacular/%s",
      commonName
    )
  )

  sciname <- tryCatch(
    {
      print(url)
      payload <- jsonlite::fromJSON(url)
      sciname <- payload$scientificname
    },
    error = function(e) {
      print(geterrmessage())
      stop(sprintf(
        "Error in querying WoRMS, %s was probably not found.",
        commonName
      ))
    }
  )

  return(sciname)
}

convert_sex <- function(sex) {
  if (toupper(sex) %in% c("F", "FEMALE")) {
    return("FEMALE")
  }
  if (toupper(sex) %in% c("M", "MALE")) {
    return("MALE")
  }
  return(sex)
}
