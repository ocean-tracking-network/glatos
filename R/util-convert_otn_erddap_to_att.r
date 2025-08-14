#' Convert detections, transmitter, receiver, and animal metadata to a format
#' that ATT accepts.
#'
#' Convert `glatos_detections` and transmitter, receiver, and animal metadata
#' from the OTN ERDDAP to `ATT` format for use in the Animal Tracking Toolbox
#' <https://github.com/vinayudyawer/ATT>, now part of `VTrack`
#' <https://github.com/RossDwyer/VTrack>.
#'
#' @param detectionObj A `glatos_detections` object (e.g., created by
#'   [read_otn_detections] or [read_glatos_detections]) or a `data.frame`
#'   containing required columns (see [glatos_detections]).
#'
#' @param erdTags a data frame with tag release data from the OTN ERDDAP
#'
#' @param erdRcv a data frame with receiver station data from the OTN ERDDAP
#'
#' @param erdAni a data frame with animal data from the OTN ERDDAP
#'
#' @param crs an object of class `crs` (see [sf::st_crs][st_crs]) with
#'   geographic coordinate system for all spatial information
#'   (latitude/longitude). If none provided or `crs` is not recognized,
#'   defaults to WGS84.
#'
#'
#' @details This function takes 4 data frames containing detection, and ERDDAP
#'   data from the tags, receivers, and animals tables, and transforms them into
#'   3 `tibble` objects inside of a list. The input that AAT uses to get this
#'   data product is located here:
#'   <https://github.com/vinayudyawer/ATT/blob/master/README.md> and our
#'   mappings are found here:
#'   <https://github.com/ocean-tracking-network/glatos/issues/75#issuecomment-982822886>
#'   in a comment by Ryan Gosse. The OTN ERDDAP instance is here:
#'   <https://members.oceantrack.org/erddap/tabledap/index.html> but please note
#'   that this only contains public data.
#'
#' @author Ryan Gosse
#'
#' @return a list of 3 tibbles containing tag detections, tag metadata,
#'   and station metadata, to be ingested by VTrack/ATT
#'
#' @examples
#'
#' #--------------------------------------------------
#' # EXAMPLE #1 - loading from the OTN ERDDAP + vignettes
#'
#' library(glatos)
#'
#' # get path to example files from OTN ERDDAP
#' ani_erd_file <- system.file("extdata", "otn_aat_animals.csv",
#'   package = "glatos"
#' )
#' animals <- read.csv(ani_erd_file) # load the CSVs from ERDDAP
#'
#' tags_erd_file <- system.file("extdata", "otn_aat_tag_releases.csv",
#'   package = "glatos"
#' )
#' tags <- read.csv(tags_erd_file)
#'
#' rcv_erd_file <- system.file("extdata", "otn_aat_receivers.csv",
#'   package = "glatos"
#' )
#' stations <- read.csv(rcv_erd_file)
#'
#' # Remove first row; (blank or metadata about the column)
#' animals <- animals[-1, ]
#' tags <- tags[-1, ]
#' stations <- stations[-1, ]
#'
#' # get blue shark example data
#' shrk_det_file <- system.file("extdata", "blue_shark_detections.csv",
#'   package = "glatos"
#' )
#' blue_shark_detections <- read_otn_detections(shrk_det_file) # load shark data
#'
#' ATTdata <- convert_otn_erddap_to_att(
#'   detectionObj = blue_shark_detections,
#'   erdTags = tags,
#'   erdRcv = stations,
#'   erdAni = animals
#' )
#' @export
#'
convert_otn_erddap_to_att <- function(
    detectionObj,
    erdTags,
    erdRcv,
    erdAni,
    crs = sf::st_crs(4326)) {
  ##  Declare global variables for R CMD check
  Sex <- latitude <- longitude <- station <- receiver_model <-
    receiver_serial_number <- dummy <- time <- recovery_datetime_utc <-
    deploy_datetime_utc <- detection_timestamp_utc <- NULL

  transmitters <-
    if (all(grepl("-", detectionObj$transmitter_id, fixed = TRUE))) {
      detectionObj$transmitter_id
    } else {
      concat_list_strings(
        detectionObj$transmitter_codespace,
        detectionObj$transmitter_id
      )
    }

  # Start building Tag.Metadata table
  tagMetadata <- unique(dplyr::tibble(
    Tag.ID = detectionObj$animal_id,
    Transmitter = as.factor(transmitters),
    Common.Name = as.factor(detectionObj$common_name_e)
  ))

  # Cut out dupes
  tagMetadata <- unique(tagMetadata)

  nameLookup <- dplyr::tibble(
    # Get all the unique common names
    Common.Name = unique(tagMetadata$Common.Name)
  )

  # Add scinames to the name lookup
  nameLookup <- dplyr::mutate(
    nameLookup,
    Sci.Name = as.factor(
      query_worms_common(nameLookup$Common.Name, silent = TRUE)
    )
  )

  # Apply sci names to frame
  tagMetadata <- dplyr::left_join(tagMetadata, nameLookup, by = "Common.Name")

  # Matching cols that have different names
  colnames(erdTags)[colnames(erdTags) == "tag_device_id"] <- "transmitter_id"
  detectionObj <- dplyr::left_join(detectionObj, erdTags, by = "transmitter_id")
  erdRcv <- dplyr::mutate(
    erdRcv,
    station = as.character(extract_station(
      erdRcv$receiver_reference_id
    ))
  )

  #If the detection file is formatted in the new style, we need to match scientificname to scientificName.
  #Otherwise we'll use the old verison. 
  if("scientificName" %in% colnames(detectionObj)) {
    join_by = c(
      "animal_id" = "animal_id",
      "scientificName" = "scientificname"
      "datacenter_reference" = "datacenter_reference"
    )
  }
  else {
    join_by = c(
      "animal_id",
      "scientificname",
      "datacenter_reference"
    )
  }
  
  # Matching cols that have different names
  colnames(erdAni)[colnames(erdAni) == "animal_reference_id"] <- "animal_id"
  detectionObj <- dplyr::left_join(detectionObj,
    erdAni,
    by = join_by
  )

  # Get the rest from detectionObj
  releaseData <- dplyr::tibble(
    Tag.ID = detectionObj$animal_id,
    Tag.Project = as.factor(detectionObj$animal_project_reference),
    Release.Latitude = as.double(detectionObj$latitude),
    Release.Longitude = as.double(detectionObj$longitude),
    Release.Date = as.Date(detectionObj$time),
    Sex = as.factor(detectionObj$sex)
  )

  releaseData <- dplyr::mutate(
    releaseData,
    # Convert sex text and null missing columns
    Sex = as.factor(convert_sex(Sex)),
    Tag.Life = as.integer(NA),
    Tag.Status = as.factor(NA),
    Bio = as.factor(NA)
  )
  # Final version of Tag.Metadata
  tagMetadata <- unique(dplyr::left_join(
    tagMetadata,
    releaseData,
    by = "Tag.ID"
  ))

  datetime_timezone <- unique(detectionObj$timezone)

  detectionObj <- detectionObj %>%
    dplyr::mutate(dummy = TRUE) %>%
    dplyr::left_join(
      dplyr::select(
        erdRcv %>% dplyr::mutate(dummy = TRUE),
        rcv_latitude = latitude,
        rcv_longitude = longitude,
        station,
        receiver_model,
        receiver_serial_number,
        dummy,
        deploy_datetime_utc = time,
        recovery_datetime_utc
      ),
      by = c("station", "dummy"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      deploy_datetime_utc = as.POSIXct(
        deploy_datetime_utc,
        format = "%Y-%m-%dT%H:%M:%OS",
        tz = datetime_timezone
      ),
      recovery_datetime_utc = as.POSIXct(
        recovery_datetime_utc,
        format = "%Y-%m-%dT%H:%M:%OS",
        tz = datetime_timezone
      )
    ) %>%
    dplyr::filter(
      detection_timestamp_utc >= deploy_datetime_utc,
      detection_timestamp_utc <= recovery_datetime_utc
    ) %>%
    dplyr::mutate(
      ReceiverFull = concat_list_strings(
        receiver_model,
        receiver_serial_number
      )
    ) %>%
    dplyr::select(-dummy)

  detections <- dplyr::tibble(
    Date.Time = detectionObj$detection_timestamp_utc,
    Transmitter = as.factor(detectionObj$transmitter_id),
    Station.Name = as.factor(detectionObj$station),
    Receiver = as.factor(detectionObj$ReceiverFull),
    Latitude = detectionObj$deploy_lat,
    Longitude = detectionObj$deploy_long,
    Sensor.Value = as.integer(detectionObj$sensorvalue),
    Sensor.Unit = as.factor(detectionObj$sensorunit)
  )

  stations <- unique(dplyr::tibble(
    Station.Name = as.factor(detectionObj$station),
    Receiver = as.factor(detectionObj$ReceiverFull),
    Installation = as.factor(NA),
    Receiver.Project = as.factor(detectionObj$collectioncode),
    Deployment.Date = detectionObj$deploy_datetime_utc,
    Recovery.Date = detectionObj$recovery_datetime_utc,
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

  # Note that sf::st_crs() uses class name 'crs' but this is changed to 'CRS'
  #  because VTrack/ATT are using sp::CRS()
  if (inherits(crs, "crs")) {
    attr(att_obj, "CRS") <- crs
  } else {
    message(
      "Geographic projection for detection positions not recognised, ",
      "reverting to WGS84 global coordinate reference system."
    )
    attr(att_obj, "CRS") <- eval(formals()$crs)
  }

  return(att_obj)
}


# Function for taking 2 lists of string of the same length and concatenating
#  the columns, row by row.
concat_list_strings <- function(list1, list2, sep = "-") {
  if (length(list1) != length(list2)) {
    stop(sprintf(
      "Lists are not the same size. %d != %d.",
      length(list1),
      length(list2)
    ))
  }
  return(paste(list1, list2, sep = sep))
}


# Converts the receiver reference id to station name
extract_station <- function(receiver_ref) {
  sapply(
    receiver_ref,
    FUN = function(x) {
      x <- as.character(x)
      return(
        # Split the string by _ and drop the array name
        unlist(
          strsplit(c(x), c("_"))
        )[-1]
      )
    },
    USE.NAMES = FALSE
  )
}
