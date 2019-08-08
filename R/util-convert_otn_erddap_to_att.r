#' Convert detections and receiver metadata to a format that 
#' ATT (https://github.com/vinayudyawer/ATT) accepts.
#'
#' @param glatosObj a list from \code{read_glatos_detections}
#'
#' @param erdTags a list from the OTN ERDDAP of tags
#'
#' @param erdRcv a list from the OTN ERDDAP of receivers
#'
#' @param erdAni a list from the OTN ERDDAP of animals
#'
#'
#' @details This function takes 4 lists containing detection, and
#' ERDDAP data from the tags receivers and animals tables, and transforms them into 3 \code{tibble} objects 
#' inside of a list. The input that AAT uses to get this data product
#' is located here: https://github.com/vinayudyawer/ATT/blob/master/README.md
#' and our mappings are found here: https://gitlab.oceantrack.org/GreatLakes/glatos/issues/83
#' in a comment by Ryan Gosse. The OTN ERDDAP instance is here: https://members.oceantrack.org/erddap/tabledap/index.html
#' but please note that this only contains public data.
#'
#' @author Ryan Gosse
#'
#' @return a list of 3 tibbles containing tag dectections, tag metadata, and
#' station metadata, to be injested by VTrack/ATT
#'
#' @examples
#'
#' #--------------------------------------------------
#' # EXAMPLE #1 - loading from the OTN ERDDAP + vignettes
#'
#' library(glatos)
#' animals <- read.csv("/path/to/animal/csv/otn_aat_animals.csv") # load the CSVs from ERDDAP
#' tags <- read.csv("/path/to/tag/csv/otn_aat_tags.csv")
#' stations <- read.csv("/path/to/station/csv/otn_aat_stations.csv")
#'
#' animals <- animals[-1,] # First row is garbage (blank or metadata about the column) so we can cut it
#' tags <- tags[-1,]
#' stations <- stations[-1,]
#'
#' shrk_det_file <- system.file("extdata", "blue_shark_detections.csv",
#'      package = "glatos")
#' blue_shark_detections <- read_glatos_detections(shrk_det_file) # load shark data
#'
#' ATTData <- convert_otn_erddap_to_att(blue_shark_detections, tags, stations, animals)
#' @export

convert_otn_erddap_to_att <- function(glatosObj, erdTags, erdRcv, erdAni) {

    transmitters <- if(all(grepl("-", glatosObj$transmitter_id, fixed=TRUE))) glatosObj$transmitter_id else  concat_list_strings(glatosObj$transmitter_codespace, glatosObj$transmitter_id)

    tagMetadata <- unique(tibble( # Start building Tag.Metadata table
        Tag.ID=glatosObj$animal_id,
        Transmitter=as.factor(transmitters),
        Common.Name=as.factor(glatosObj$common_name_e)
    ))
    
    tagMetadata <- unique(tagMetadata) # Cut out dupes
    
    nameLookup <- tibble( # Get all the unique common names
        Common.Name=unique(tagMetadata$Common.Name)
    )
    nameLookup <- mutate(nameLookup, # Add scinames to the name lookup
        Sci.Name=as.factor(map(nameLookup$Common.Name, query_worms_common))
    )
    tagMetadata <- left_join(tagMetadata, nameLookup) # Apply sci names to frame

    colnames(erdTags)[colnames(erdTags)=="tag_device_id"] <- "transmitter_id" # Matching cols that have different names
    glatosObj <- left_join(glatosObj, erdTags)
    erdRcv <- mutate(erdRcv,
        station=as.character(map(erdRcv$receiver_reference_id, extract_station))
    )
    colnames(erdAni)[colnames(erdAni)=="animal_reference_id"] <- "animal_id" # Matching cols that have different names
    glatosObj <- left_join(glatosObj, erdAni)

    releaseData <- tibble( # Get the rest from glatosObj
        Tag.ID=glatosObj$animal_id, 
        Tag.Project=as.factor(glatosObj$animal_project_reference), 
        Release.Latitude=as.double(glatosObj$latitude), 
        Release.Longitude=as.double(glatosObj$longitude), 
        Release.Date=as.Date(glatosObj$time),
        Sex=as.factor(glatosObj$sex)
    )

    releaseData <- mutate(releaseData, # Convert sex text and null missing columns
        Sex=as.factor(map(Sex, convert_sex)),
        Tag.Life=as.integer(NA),
        Tag.Status=as.factor(NA),
        Bio=as.factor(NA)
    ) 
    tagMetadata <- unique(left_join(tagMetadata, releaseData)) # Final version of Tag.Metadata

    glatosObj <- glatosObj %>%
        mutate(dummy=TRUE) %>%
        left_join(select(erdRcv %>% mutate(dummy=TRUE), rcv_latitude=latitude, rcv_longitude=longitude, station, receiver_model, receiver_serial_number, dummy, deploy_datetime_utc=time, recovery_datetime_utc)) %>%
        mutate(deploy_datetime_utc=as.POSIXct(deploy_datetime_utc, format="%Y-%m-%dT%H:%M:%OS"), recovery_datetime_utc=as.POSIXct(recovery_datetime_utc, format="%Y-%m-%dT%H:%M:%OS")) %>%
        filter(detection_timestamp_utc >= deploy_datetime_utc, detection_timestamp_utc <= recovery_datetime_utc) %>%
        mutate(ReceiverFull=concat_list_strings(receiver_model, receiver_serial_number)) %>%
        select(-dummy)

    detections <- tibble(
        Date.Time=glatosObj$detection_timestamp_utc,
        Transmitter=as.factor(glatosObj$transmitter_id),
        Station.Name=as.factor(glatosObj$station),
        Receiver=as.factor(glatosObj$ReceiverFull),
        Latitude=glatosObj$deploy_lat,
        Longitude=glatosObj$deploy_long,
        Sensor.Value=as.integer(glatosObj$sensorvalue),
        Sensor.Unit=as.factor(glatosObj$sensorunit)
    )

    stations <- unique(tibble(
        Station.Name=as.factor(glatosObj$station),
        Receiver=as.factor(glatosObj$ReceiverFull),
        Installation=as.factor(NA),
        Receiver.Project=as.factor(glatosObj$collectioncode),
        Deployment.Date=glatosObj$deploy_datetime_utc,
        Recovery.Date=glatosObj$recovery_datetime_utc,
        Station.Latitude=as.double(glatosObj$deploy_lat),
        Station.Longitude=as.double(glatosObj$deploy_long),
        Receiver.Status=as.factor(NA)
    ))
    att_obj <- list(
        Tag.Detections=detections,
        Tag.Metadata=tagMetadata,
        Station.Information=stations
    )

    class(att_obj) <- "ATT"

    return(att_obj)
}


# Function for taking 2 lists of string of the same length and concatenating the columns, row by row.
concat_list_strings <- function(list1, list2, sep = "-") {
    if (length(list1) != length(list2)) {
        stop(sprintf("Lists are not the same size. %d != %d.", length(list1), length(list2)))
    }
    return (paste(list1, list2, sep = sep))
}

# Simple query to WoRMS based on the common name and returns the sci name
query_worms_common <- function(commonName) {

    url <- URLencode(sprintf("http://www.marinespecies.org/rest/AphiaRecordsByVernacular/%s", commonName))
    tryCatch({
        print(url)
        payload <- fromJSON(url)
        return(payload$scientificname)
    }, error = function(e){
        print(geterrmessage())
        stop(sprintf('Error in querying WoRMS, %s was probably not found.', commonName))
    })
}

# Convert the sex from 'F' and 'M' to 'FEMALE' and 'MALE'
convert_sex <- function(sex) {
    if (sex == "F") return("FEMALE")
    if (sex == "M") return("MALE")
    return(sex)
}

# Converts the reciever reference id to station name
extract_station <- function(reciever_ref) {
    reciever_ref <- as.character(reciever_ref)
    return( # Split the string by _ and drop the array name
        unlist(
            strsplit(c(reciever_ref), c("_"))
        )[-1] 
    )
}
