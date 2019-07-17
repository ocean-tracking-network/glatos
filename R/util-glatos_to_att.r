#' Glatos To ATT Conversion
#'
#' Function to translate the glatos format to the ATT format

glatos_to_att <- function(glatosObj, receiverObj) {
    library(tibble)
    library(purrr)
    library(dplyr)
  

    tagMetadata <- unique(tibble( # Start building Tag.Metadata table
        Tag.ID=as.integer(glatosObj$animal_id),
        Transmitter=as.factor(concat_list_strings(glatosObj$transmitter_codespace, glatosObj$transmitter_id)),
        Common.Name=as.factor(glatosObj$common_name_e)
    ))
    # tagMetadata <- join_transmitter_to_frame(tagMetadata, glatosObj$transmitter_codespace, glatosObj$transmitter_id) # Add the Transmitter string
    
    tagMetadata <- unique(tagMetadata) # Cut out dupes
    
    nameLookup <- tibble( # Get all the unique common names
        Common.Name=unique(tagMetadata$Common.Name)
    )
    nameLookup <- mutate(nameLookup, # Add scinames to the name lookup
        Sci.Name=as.factor(map(nameLookup$Common.Name, query_worms_common))
    )
    tagMetadata <- left_join(tagMetadata, nameLookup) # Apply sci names to frame

    releaseData <- tibble( # Get the rest from glatosObj
        Tag.ID=as.integer(glatosObj$animal_id), 
        Tag.Project=as.factor(glatosObj$glatos_project_transmitter), 
        Release.Latitude=glatosObj$release_latitude, 
        Release.Longitude=glatosObj$release_longitude, 
        Release.Date=as.Date(glatosObj$utc_release_date_time),
        Sex=as.factor(glatosObj$sex)
    )

    releaseData <- mutate(releaseData, # Convert sex text and null missing columns
        Sex=as.factor(map(Sex, convert_sex)),
        Tag.Life=as.integer(NA),
        Tag.Status=as.factor(NA),
        Bio=as.factor(NA)
    ) 
    tagMetadata <- left_join(tagMetadata, releaseData) # Final version of Tag.Metadata

    glatosObj <- glatosObj %>%
        mutate(dummy=TRUE) %>%
        left_join(select(receiverObj %>% mutate(dummy=TRUE), glatos_array, station_no, deploy_lat, deploy_long, station, dummy, ins_model_no, ins_serial_no, deploy_date_time, recover_date_time)) %>%
        filter(detection_timestamp_utc >= deploy_date_time, detection_timestamp_utc <= recover_date_time) %>%
        mutate(ReceiverFull=concat_list_strings(ins_model_no, ins_serial_no)) %>%
        select(-dummy)

    detections <- tibble(
        Date.Time=glatosObj$detection_timestamp_utc,
        Transmitter=as.factor(concat_list_strings(glatosObj$transmitter_codespace, glatosObj$transmitter_id)),
        Station.Name=as.factor(glatosObj$station),
        Receiver=as.factor(glatosObj$ReceiverFull),
        Latitude=glatosObj$deploy_lat,
        Longitude=glatosObj$deploy_long,
        Sensor.Value=as.integer(glatosObj$sensor_value),
        Sensor.Unit=as.factor(glatosObj$sensor_unit)
    )

    stations <- tibble(
        Station.Name=as.factor(receiverObj$station),
        Receiver=as.factor(concat_list_strings(receiverObj$ins_model_no, receiverObj$ins_serial_no)),
        Installation=as.factor(NA),
        Receiver.Project=as.factor(receiverObj$glatos_project),
        Deployment.Date=receiverObj$deploy_date_time,
        Recovery.Date=receiverObj$recover_date_time,
        Station.Latitude=receiverObj$deploy_lat,
        Station.Longitude=receiverObj$deploy_long,
        Receiver.Status=as.factor(NA)
    )
    
    return(list(
        Tag.Detections=detections,
        Tag.Metadata=tagMetadata,
        Station.Information=stations
    ))
}

concat_list_strings <- function(list1, list2, sep = "-") {
    if (length(list1) != length(list2)) {
        stop(sprintf("Lists are not the same size. %d != %d.", length(list1), length(list2)))
    }
    return (paste(list1, list2, sep = sep))
}

join_transmitter_to_frame <- function(frame, list1, list2) {
    if (length(list1) != length(list2) | nrow(frame) != length(list1)) {
        print(length(frame))
        stop(sprintf("Lists are not the same size", length(list1), length(list2)))
    }
    newFrame <- mutate(frame,
        Transmitter=paste(list1, list2, sep = "-")
    )
    return(newFrame)
}

query_worms_common <- function(commonName) {
    library(jsonlite) 
    library(httr)
    url <- sprintf("http://www.marinespecies.org/rest/AphiaRecordsByVernacular/%s", commonName)
    tryCatch({
        payload <- fromJSON(url)
        return(payload$scientificname)
    }, error = function(e){
        stop(sprintf('Error in querying WoRMS, %s was probably not found.', commonName))
    })
}

convert_sex <- function(sex) {
    if (sex == "F") return("FEMALE")
    if (sex == "M") return("MALE")
    return(sex)
}

get_reciever_from_station_and_time <- function(row, receiverObj=NA) {
    station_record <- filter(receiverObj, between(row[['detection_timestamp_utc']]))
    station_record <- filter(receiverObj, station==row[['station']] & deploy_date_time <= row[['detection_timestamp_utc']] & recover_date_time >= row[["detection_timestamp_utc"]])
    if (nrow(station_record) > 1) {
        stop(sprintf("Too many stations match to a detection. %d is more than 1. See station '%s'", length(station_record), row$station))
    }
    return(paste(station_record$ins_model_no, station_record$ins_serial_no, sep='-'))
}