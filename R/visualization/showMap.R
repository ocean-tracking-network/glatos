library(shiny)
library(xts)
library(dplyr)
library(leaflet)

#http://www.r-graph-gallery.com/2017/03/14/4-tricks-for-working-with-r-leaflet-and-shiny/

#' Shows leaflet maps as a shiny app with timelines of animal movements
#' Can also filter by id and show different types of maps


#' showMapIcon: shows an animated leaflet map with the movements of all the
#'   animals, depicts animals with icons on local disk
#'   
#' @param detections A data frame containing detection data with at least
#'   4 columns containing 'animal', 'timestamp', 'latitude', and 'longitude'.
#'   Column names are specified with \code{detColNames} by \code{type}.
#' 
#' @param type A character string that contains the type of data that is being 
#'   passed in, for example, "OTN", "GLATOS", or "sample".
#' 
#' @param iconFiles optional parameter, a list of Strings containing the locations of files that 
#'  hold images of icons, must be same length as number of unique animal ids
#' 
#' @param typeOfMap optional parameter, a character string containing the type of map to use
#' 
#' @param meanLongitude optional parameter, an integer containing the longitude of the middle of
#'   the base map
#' 
#' @param meanLatitude optional parameter, an integer containing the latitude of the middle of
#'   the base map
#' 
#' @param zoom optional parameter, an integer containing the zoom of the view of the base map
#' 
#' @details detColNames is defined as a list with names of required columns in 
#'   \code{detections}, defined by \code{type}: 
#' \itemize{
#'   \item \code{animalCol} is a character string with the name of the column 
#' 		 containing the individual animal identifier ('animal_id' for GLATOS data,
#' 		 'catalognumber' for OTN data, or 'animalId' for sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' for
#'     OTN data, or 'timestamp' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 'latitude'
#'     for OTN data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 'longitude'
#'     for OTN data, or 'longitude' for sample data).
#' 
#' @details Uses \code{movePath} to get locations every second/minute
#'
#' @author Angela Dini
#' 
#' @export

# Show map of all animals with icons
showMapIcon <- function(detections, type="sample",
                        iconFiles=c("/Users/dinian/Desktop/glatos-git/R/visualization/Icons/redFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/blueFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/greenFish.png"), 
                        meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  
  if(type == "GLATOS") { #Set column names for GLATOS data
    detColNames = list(animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
  } else if (type == "OTN") { #Set column names for OTN data
    detColNames = list(animalCol="catalognumber", timestampCol="datecollected",latCol="latitude", longCol="longitude")
  } else if (type == "sample") { #Set column names for sample data
    detColNames = list(animalCol="animalId", timestampCol="timestamp", latCol="latitude", longCol="longitude")
  } else { # Other type
    stop(paste0("The type '",type,"' is not defined."), call.=FALSE)
  }
  
  # Check that the specified columns appear in the detections dataframe
  missingCols <- setdiff(unlist(detColNames), names(detections))
  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }
  
  # Subset detections with only user-defined columns and change names
  # this makes code more easy to understand (esp. ddply)
  detections <- detections[,unlist(detColNames)] #subset
  names(detections) <- c("animalId","time","latitude","longitude")
  
  # Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(detections$time))){
    stop(paste0("Column '",detColNames$timestampCol,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  # Check that length of iconFiles is same as number of unique animal ids
  uniq <- unique(detections$animalId)
  nr <- length(uniq)
  if (length(iconFiles) != nr) {
    stop(paste0("length of colourNames does not match number of unique '", detColNames$animalCol,"'"), call.=FALSE)
  }
  
  # Get location at every second
  mdSplit <- split(detections, detections$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    anId <- mdI$animalId[[1]]
    for (t in 1:(length(mdI$time)-1)) {
      if(length(mdI$time) >=2 && !is.na(mdI$time[[t]]) && !is.na(mdI$time[[t+1]]) && !is.na(mdI$latitude[[t]]) && !is.na(mdI$longitude[[t]]) && !is.na(mdI$latitude[[t+1]]) && !is.na(mdI$longitude[[t+1]])) {
        # Get sequence of time of seconds between the two times
        tS <- seq(mdI$time[[t]], mdI$time[[t+1]], by="sec")
        if(length(tS) > 1) {
          # Get a data frame containing the longitude and latitude at each time
          locs <- movePath(mdI$longitude[[t]], mdI$latitude[[t]], mdI$longitude[[t+1]], mdI$latitude[[t+1]], tS)

          # Clean up data to match detections dataframe format
          locs$latitude <- locs$lat
          locs$lat <- NULL
          locs$longitude <- locs$lon
          locs$lon <- NULL
          locs$animalId <- rep(x=anId, times=nrow(locs))
          locs$time <- locs$timestamp
          locs$timestamp <- NULL
          
          # Merge two dataframes
          detections <- rbind(detections, locs)
        }
      }
    }
  }
  
  #Order detections by animal id and time
  sMapData <- detections[order(detections$animalId, detections$time),]
  
  #Add icons column to data
  sMapData$icon <- apply(sMapData, 1, function(x) {
    listUniq <- unique(sMapData$animalId)
    n <- match(x["animalId"], listUniq)
    return(iconFiles[[n]])
  })

  # From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
  ui <- fluidPage(
    # Application page should have slider and map as defined
    sliderInput("time", "date", min(sMapData$time),
                max(sMapData$time),
                value=max(sMapData$time),
                step=1,
                animate=animationOptions(interval=400, loop=TRUE)),
    leafletOutput("mymap")
  )

  server <- function(input, output, session) {
    points <- reactive ({
      sMapData %>%
        filter(sMapData$time==input$time)
    })
    # Define the leaflet map as described
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>%
        addTiles() %>%
        addMarkers(lat = points()$latitude, lng=points()$longitude, label=as.character(points()$a), icon=makeIcon(points()$icon, iconWidth = 39, iconHeight=24))
    })
  }
  #Run the application
  runApp(list(ui=ui, server=server))
}


#' showMapCircle: shows an animated leaflet map with the movements of all the
#'   animals, depicts animals with circles of different colours
#'   
#' @param detections A data frame containing detection data with at least
#'   4 columns containing 'animal', 'timestamp', 'latitude', and 'longitude'.
#'   Column names are specified with \code{detColNames} by \code{type}.
#' 
#' @param type A character string that contains the type of data that is being 
#'   passed in, for example, "OTN", "GLATOS", or "sample".
#' 
#' @param colourNames optional parameter, a list of Strings containing the name of the
#'   colours used for each animal id, must be same length as number of unique animal ids
#' @param meanLongitude optional parameter, an integer containing the longitude of the middle of
#'   the base map
#' 
#' @param meanLatitude optional parameter, an integer containing the latitude of the middle of
#'   the base map
#' 
#' @param zoom optional parameter, an integer containing the zoom of the view of the base map
#' 
#' @details detColNames is defined as a list with names of required columns in 
#'   \code{detections}, defined by \code{type}: 
#' \itemize{
#'   \item \code{animalCol} is a character string with the name of the column 
#' 		 containing the individual animal identifier ('animal_id' for GLATOS data,
#' 		 'catalognumber' for OTN data, or 'animalId' for sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' for
#'     OTN data, or 'timestamp' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 'latitude'
#'     for OTN data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 'longitude'
#'     for OTN data, or 'longitude' for sample data).
#'
#' @details Uses \code{movePath} to get locations every second/minute
#'
#' @author Angela Dini
#' 
#' @export

#Show map with coloured circle markers (Alice = red, Bob = blue, Eve = green)
showMapCircle <- function(detections, type="sample", colourNames=list(), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  
  if(type == "GLATOS") { #Set column names for GLATOS data
    detColNames = list(animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
  } else if (type == "OTN") { #Set column names for OTN data
    detColNames = list(animalCol="catalognumber", timestampCol="datecollected",latCol="latitude", longCol="longitude")
  } else if (type == "sample") { #Set column names for sample data
    detColNames = list(animalCol="animalId", timestampCol="timestamp", latCol="latitude", longCol="longitude")
  } else { # Other type
    stop(paste0("The type '",type,"' is not defined."), call.=FALSE)
  }
  
  # Check that the specified columns appear in the detections dataframe
  missingCols <- setdiff(unlist(detColNames), names(detections))
  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }
  
  # Subset detections with only user-defined columns and change names
  # this makes code more easy to understand (esp. ddply)
  detections <- detections[,unlist(detColNames)] #subset
  names(detections) <- c("animalId","time","latitude","longitude")
  
  # Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(detections$time))){
    stop(paste0("Column '",detColNames$timestampCol,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  uniq <- unique(detections$animalId)
  nr <- length(uniq)
  len <- length(colourNames)
  # Set 'colourNames' if not already set
  if(len==0) {
    colourNames=palette(rainbow(nr))
  } else if (len != nr) { # Check that length of colourNames is same as number of unique animal ids
    stop(paste0("length of colourNames does not match number of unique '", detColNames$animalCol, "'"), call.=FALSE)
  }
  
  # Get location at every second
  mdSplit <- split(detections, detections$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    anId <- mdI$animalId[[1]]
    for (t in 1:(length(mdI$time)-1)) {
      if(length(mdI$time)>=2 && !is.na(mdI$time[[t]]) && !is.na(mdI$time[[t+1]]) && !is.na(mdI$latitude[[t]]) && !is.na(mdI$longitude[[t]]) && !is.na(mdI$latitude[[t+1]]) && !is.na(mdI$longitude[[t+1]])) {
        # Get sequence of time of seconds between the two times
        tS <- seq(mdI$time[[t]], mdI$time[[t+1]], by="sec")
        if(length(tS) > 1) {
          # Get dataframe of longitude and latitude at each time
          locs <- movePath(mdI$longitude[[t]], mdI$latitude[[t]], mdI$longitude[[t+1]], mdI$latitude[[t+1]], tS)
          
          # Clean up data to match detections dataframe format
          locs$latitude <- locs$lat
          locs$lat <- NULL
          locs$longitude <- locs$lon
          locs$lon <- NULL
          locs$animalId <- rep(x=anId, times=nrow(locs))
          locs$time <- locs$timestamp
          locs$timestamp <- NULL
          
          #Merge the two dataframes
          detections <- rbind(detections, locs)
        }
      }
    }
  }
  
  # Order detections by animal id and time
  sMapData <- detections[order(detections$animalId, detections$time),]
  
  #Add colour column to sMapData
  sMapData$colour <- apply(sMapData, 1, function(x) {
    listUniq <- unique(sMapData$animalId)
    n <- match(x["animalId"], listUniq)
    return(colourNames[[n]])
  })
  
  # From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
  ui <- fluidPage(
    # Application page should have slider and map as defined
    sliderInput("time", "date", min(sMapData$time),
                max(sMapData$time),
                value=max(sMapData$time),
                step=1,
                animate=animationOptions(interval=400, loop=TRUE)),
    leafletOutput("mymap")
  )
  
  server <- function(input, output, session) {
    points <- reactive ({
      sMapData %>%
        filter(sMapData$time==input$time)
    })
    # Define leaflet map as described
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>%
        addTiles() %>%
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=as.character(points()$a), color=points()$colour)
    })
  }
  # Run the application
  runApp(list(ui=ui, server=server))
}


#' showIdMap: shows an animated leaflet map with the movements of
#'   animals with a specific id defined by \code{id}, depicts these 
#'   animals with circles of different colours
#'   
#' @param detections A data frame containing detection data with at least
#'   4 columns containing 'animal', 'timestamp', 'latitude', and 'longitude'.
#'   Column names are specified with \code{detColNames} by \code{type}.
#' 
#' @param type A character string that contains the type of data that is being 
#'   passed in, for example, "OTN", "GLATOS", or "sample".
#' 
#' @param id A character string that contains the animal identifier to examine
#' 
#' @param colourNames optional parameter, a list of Strings containing the name of the
#'   colours used for each animal id, must be same length as number of unique animal ids
#' 
#' @param meanLongitude optional parameter, holds the longitude of the middle of
#'   the base map
#' 
#' @param meanLatitude optional parameter, holds the latitude of the middle of
#'   the base map
#' 
#' @param zoom optional parameter, holds the zoom of the view of the base map
#' 
#' @details detColNames is defined as a list with names of required columns in 
#'   \code{detections}, defined by \code{type}: 
#' \itemize{
#'   \item \code{animalCol} is a character string with the name of the column 
#' 		 containing the individual animal identifier ('animal_id' for GLATOS data,
#' 		 'catalognumber' for OTN data, or 'animalId' for sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' for
#'     OTN data, or 'timestamp' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 'latitude'
#'     for OTN data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 'longitude'
#'     for OTN data, or 'longitude' for sample data).
#'
#' @details Uses \code{movePath} to get locations every second/minute
#'
#' @author Angela Dini
#' 
#' @export

showIdMap <- function(detections, type="sample", id, colourNames=list(), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  
  if(type == "GLATOS") { #Set column names for GLATOS data
    detColNames = list(animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
  } else if (type == "OTN") { #Set column names for OTN data
    detColNames = list(animalCol="catalognumber", timestampCol="datecollected",latCol="latitude", longCol="longitude")
  } else if (type == "sample") { #Set column names for sample data
    detColNames = list(animalCol="animalId", timestampCol="timestamp", latCol="latitude", longCol="longitude")
  } else { # Other type
    stop(paste0("The type '",type,"' is not defined."), call.=FALSE)
  }
  
  # Check that the specified columns appear in the detections dataframe
  missingCols <- setdiff(unlist(detColNames), names(detections))
  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }
  
  # Subset detections with only user-defined columns and change names
  # this makes code more easy to understand (esp. ddply)
  detections <- detections[,unlist(detColNames)] #subset
  names(detections) <- c("animalId","time","latitude","longitude")
  
  # Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(detections$time))){
    stop(paste0("Column '",detColNames$timestampCol,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  uniq <- unique(detections$animalId)
  nr <- length(uniq)
  len <- length(colourNames)
  # Set 'colourNames' if not already set
  if(len==0) {
    colourNames=palette(rainbow(nr))
  } else if (len != nr) { # Check that length of colourNames is same as number of unique animal ids
    stop(paste0("length of colourNames does not match number of unique '", detColNames$animalCol, "'"), call.=FALSE)
  }
  
  # Get location at every second
  mdSplit <- split(detections, detections$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    anId <- mdI$animalId[[1]]
    for (t in 1:(length(mdI$time)-1)) {
      if(length(mdI$time)>=2 && !is.na(mdI$time[[t]]) && !is.na(mdI$time[[t+1]]) && !is.na(mdI$latitude[[t]]) && !is.na(mdI$longitude[[t]]) && !is.na(mdI$latitude[[t+1]]) && !is.na(mdI$longitude[[t+1]])) {
        # Get sequence of time of seconds between the two times
        tS <- seq(mdI$time[[t]], mdI$time[[t+1]], by="sec")
        if(length(tS) > 1) {
          # Get dataframe of longitude and latitude at each time
          locs <- movePath(mdI$longitude[[t]], mdI$latitude[[t]], mdI$longitude[[t+1]], mdI$latitude[[t+1]], tS)
          
          # Clean up data to match detections dataframe format
          locs$latitude <- locs$lat
          locs$lat <- NULL
          locs$longitude <- locs$lon
          locs$lon <- NULL
          locs$animalId <- rep(x=anId, times=nrow(locs))
          
          # Merge the two dataframes
          detections <- rbind(detections, locs)
        }
      }
    }
  }
  
  #Filter data so that it only contains detections of the specified animal id
  dataS <- detections[detections$animalId==id,]
  
  # Order dataS by time
  dataS <- dataS[order(dataS$timestamp),]
  
  #Add colour column to dataS
  dataS$colour <- apply(dataS, 1, function(x) {
    listUniq <- unique(detections$animalId)
    n <- match(id, listUniq)
    return(colourNames[[n]])
  })
  
  # From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
  ui <- fluidPage(
    # Application page should have slider and leaflet map
    sliderInput("time", "date", min(dataS$time),
                max(dataS$timestamp),
                value=max(dataS$timestamp),
                step=1,
                animate=animationOptions(interval=400, loop=TRUE)),
    leafletOutput("mymap")
  )
  
  server <- function(input, output, session) {
    points <- reactive ({
      dataS %>%
        filter(dataS$timestamp==input$time)
    })
    # Define leaflet map as described
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>%
        addTiles() %>%
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=as.character(points()$a), color=points()$colour)
    })
  }
  # Run the application
  runApp(list(ui=ui, server=server))
}


#' showMapPathId: shows a leaflet map with the paths of animals with a 
#'   specific id defined by \code{id}, depicts these animals with circles 
#'   of different colours
#' 
#' @param detections A data frame containing detection data with at least
#'   4 columns containing 'animal', 'timestamp', 'latitude', and 'longitude'.
#'   Column names are specified with \code{detColNames} by \code{type}.
#' 
#' @param type A character string that contains the type of data that is being 
#'   passed in, for example, "OTN", "GLATOS", or "sample".
#' 
#' @param id A character string that contains the animal identifier to examine
#' 
#' @param colourNames optional parameter, a list of Strings containing the name of the
#'   colours used for each animal id, must be same length as number of unique animal ids
#' 
#' @param meanLongitude optional parameter, holds the longitude of the middle of
#'   the base map
#' 
#' @param meanLatitude optional parameter, holds the latitude of the middle of
#'   the base map
#' 
#' @param zoom optional parameter, holds the zoom of the view of the base map
#' 
#' @details detColNames is defined as a list with names of required columns in 
#'   \code{detections}, defined by \code{type}: 
#' \itemize{
#'   \item \code{animalCol} is a character string with the name of the column 
#' 		 containing the individual animal identifier ('animal_id' for GLATOS data,
#' 		 'catalognumber' for OTN data, or 'animalId' for sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' for
#'     OTN data, or 'timestamp' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 'latitude'
#'     for OTN data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 'longitude'
#'     for OTN data, or 'longitude' for sample data).
#' 
#' @details Uses \code{movePath} to get locations every second/minute
#'
#' @author Angela Dini
#' 
#' @export

showMapPathId <- function(detections, type="sample", id, colourNames=list(), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  
  if(type == "GLATOS") { #Set column names for GLATOS data
    detColNames = list(animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
  } else if (type == "OTN") { #Set column names for OTN data
    detColNames = list(animalCol="catalognumber", timestampCol="datecollected",latCol="latitude", longCol="longitude")
  } else if (type == "sample") { #Set column names for sample data
    detColNames = list(animalCol="animalId", timestampCol="timestamp", latCol="latitude", longCol="longitude")
  } else { # Other type
    stop(paste0("The type '",type,"' is not defined."), call.=FALSE)
  }
  
  # Check that the specified columns appear in the detections dataframe
  missingCols <- setdiff(unlist(detColNames), names(detections))
  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }
  
  # Subset detections with only user-defined columns and change names
  # this makes code more easy to understand (esp. ddply)
  detections <- detections[,unlist(detColNames)] #subset
  names(detections) <- c("animalId","time","latitude","longitude")
  
  # Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(detections$time))){
    stop(paste0("Column '",detColNames$timestampCol,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  uniq <- unique(detections$animalId)
  nr <- length(uniq)
  len <- length(colourNames)
  # Set 'colourNames' if not already set
  if(len==0) {
    colourNames=palette(rainbow(nr))
  } else if (len != nr) { # Check that length of colourNames is same as number of unique animal ids
    stop(paste0("length of colourNames does not match number of unique '", detColNames$animalCol, "'"), call.=FALSE)
  }
  
  # Get location at every second
  mdSplit <- split(detections, detections$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    anId <- mdI$animalId[[1]]
    for (t in 1:(length(mdI$time)-1)) {
      if(length(mdI$time)>=2 && !is.na(mdI$time[[t]]) && !is.na(mdI$time[[t+1]]) && !is.na(mdI$latitude[[t]]) && !is.na(mdI$longitude[[t]]) && !is.na(mdI$latitude[[t+1]]) && !is.na(mdI$longitude[[t+1]])) {
        # Get sequence of time of seconds between the two times
        tS <- seq(mdI$time[[t]], mdI$time[[t+1]], by="sec")
        if(length(tS) > 1) {
          # Get dataframe of longitude and latitude of each time
          locs <- movePath(mdI$longitude[[t]], mdI$latitude[[t]], mdI$longitude[[t+1]], mdI$latitude[[t+1]], tS)
          
          # Clean up data to match detections dataframe format
          locs$latitude <- locs$lat
          locs$lat <- NULL
          locs$longitude <- locs$lon
          locs$lon <- NULL
          locs$animalId <- rep(x=anId, times=nrow(locs))
          
          # Merge the two dataframes
          detections <- rbind(detecionts, locs)
        }
      }
    }
  }
  
  # Filter data so that it only contains detections of the specified animal id
  dataS <- detections[detections$animalId==id,]
  
  # Order data by time
  dataS <- dataS[order(dataS$timestamp),]
  
  
  # Add colour column to dataS
  dataS$colour <- apply(dataS, 1, function(x) {
    listUniq <- unique(detections$animalId)
    n <- match(id, listUniq)
    return(colourNames[[n]])
  })
  
  # From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
  ui <- fluidPage(
    # Application page should have leaflet map
    leafletOutput("mymap")
  )
  
  server <- function(input, output, session) {
    points <- reactive ({
      dataS
    })
    # Define leaflet map as described
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>%
        addTiles() %>%
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=as.character(points()$a), color=points()$colour)
    })
  }
  
  # Run the application
  runApp(list(ui=ui, server=server))
}


#' showMapPaths: shows a leaflet map with the paths of all of the animals
#'   
#' @param detections A data frame containing detection data with at least
#'   4 columns containing 'animal', 'timestamp', 'latitude', and 'longitude'.
#'   Column names are specified with \code{detColNames} by \code{type}.
#' 
#' @param type A character string that contains the type of data that is being 
#'   passed in, for example, "OTN", "GLATOS", or "sample".
#' 
#' @param colourNames optional parameter, a list of Strings containing the name of the
#'   colours used for each animal id, must be same length as number of unique animal ids
#' 
#' @param meanLongitude optional parameter, holds the longitude of the middle of
#'   the base map
#' 
#' @param meanLatitude optional parameter, holds the latitude of the middle of
#'   the base map
#' 
#' @param zoom optional parameter, holds the zoom of the view of the base map
#' 
#' @details detColNames is defined as a list with names of required columns in 
#'   \code{detections}, defined by \code{type}: 
#' \itemize{
#'   \item \code{animalCol} is a character string with the name of the column 
#' 		 containing the individual animal identifier ('animal_id' for GLATOS data,
#' 		 'catalognumber' for OTN data, or 'animalId' for sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' for
#'     OTN data, or 'timestamp' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 'latitude'
#'     for OTN data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 'longitude'
#'     for OTN data, or 'longitude' for sample data).
#'     
#' @details Uses \code{movePath} to get locations every second/minute
#'
#' @author Angela Dini
#' 
#' @export

showMapPaths <- function(detections, type="sample", colourNames=list(), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  
  if(type == "GLATOS") { #Set column names for GLATOS data
    detColNames = list(animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
  } else if (type == "OTN") { #Set column names for OTN data
    detColNames = list(animalCol="catalognumber", timestampCol="datecollected",latCol="latitude", longCol="longitude")
  } else if (type == "sample") { #Set column names for sample data
    detColNames = list(animalCol="animalId", timestampCol="timestamp", latCol="latitude", longCol="longitude")
  } else { # Other type
    stop(paste0("The type '",type,"' is not defined."), call.=FALSE)
  }
  
  # Check that the specified columns appear in the detections dataframe
  missingCols <- setdiff(unlist(detColNames), names(detections))
  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }
  
  # Subset detections with only user-defined columns and change names
  # this makes code more easy to understand (esp. ddply)
  detections <- detections[,unlist(detColNames)] #subset
  names(detections) <- c("animalId","time","latitude","longitude")
  
  # Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(detections$time))){
    stop(paste0("Column '",detColNames$timestampCol,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  uniq <- unique(detections$animalId)
  nr <- length(uniq)
  len <- length(colourNames)
  # Set 'colourNames' if not already set
  if(len == 0) {
    colourNames=palette(rainbow(nr))
  } else if (len != nr) { # Check that length of colourNames is same as number of unique animal ids
    stop(paste0("length of colourNames does not match number of unique '", detColNames$animalCol, "'"), call.=FALSE)
  }
  
  # Get location at every second
  mdSplit <- split(detections, detections$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    anId <- mdI$animalId[[1]]
    for (t in 1:(length(mdI$time)-1)) {
      if(length(mdI$time)>=2 && !is.na(mdI$time[[t]]) && !is.na(mdI$time[[t+1]]) && !is.na(mdI$latitude[[t]]) && !is.na(mdI$longitude[[t]]) && !is.na(mdI$latitude[[t+1]]) && !is.na(mdI$longitude[[t+1]])) {
        # Get sequence of time of seconds between the two times
        tS <- seq(mdI$time[[t]], mdI$time[[t+1]], by="sec")
        if(length(tS) > 1) {
          # Get dataframe of longitude and latitude at each time
          locs <- movePath(mdI$longitude[[t]], mdI$latitude[[t]], mdI$longitude[[t+1]], mdI$latitude[[t+1]], tS)
          
          # Clean up data to match detections dataframe format
          locs$latitude <- locs$lat
          locs$lat <- NULL
          locs$longitude <- locs$lon
          locs$lon <- NULL
          locs$animalId <- rep(x=anId, times=nrow(locs))
          
          # Merge the two dataframes
          detections <- rbind(detections, locs)
        }
      }
    }
  }
  
  # Do not filter by id, no slider
  
  # Order data by animal id and time
  sMapData <- detections[order(detections$animalId, detections$timestamp),]
  
  # Add colour column to sMapData
  sMapData$colour <- apply(sMapData, 1, function(x) {
    listUniq <- unique(sMapData$animalId)
    n <- match(x["animalId"], listUniq)
    return(colourNames[[n]])
  })
  
  # From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
  ui <- fluidPage(
    #Application page should have leaflet map
    leafletOutput("mymap")
  )
  
  server <- function(input, output, session) {
    points <- reactive ({
      sMapData
    })
    # Define leaflet map as described
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>%
        addTiles() %>%
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=as.character(points()$a), color=points()$colour)
    })
  }
  
  # Run the application
  runApp(list(ui=ui, server=server))
}



#' showMapPointsId: shows a leaflet map with the points (detections) of animals
#'  with a specific id defined by \code{id}, depicts these animals with circles 
#'  of different colours
#' 
#' @param detections A data frame containing detection data with at least
#'   4 columns containing 'animal', 'timestamp', 'latitude', and 'longitude'.
#'   Column names are specified with \code{detColNames} by \code{type}.
#' 
#' @param type A character string that contains the type of data that is being 
#'   passed in, for example, "OTN", "GLATOS", or "sample".
#' 
#' @param id A character string that contains the animal identifier to examine
#' 
#' @param colourNames optional parameter, a list of Strings containing the name of the
#'   colours used for each animal id, must be same length as number of unique animal ids
#' 
#' @param meanLongitude optional parameter, holds the longitude of the middle of
#'   the base map
#' 
#' @param meanLatitude optional parameter, holds the latitude of the middle of
#'   the base map
#' 
#' @param zoom optional parameter, holds the zoom of the view of the base map
#' 
#' @details detColNames is defined as a list with names of required columns in 
#'   \code{detections}, defined by \code{type}: 
#' \itemize{
#'   \item \code{animalCol} is a character string with the name of the column 
#' 		 containing the individual animal identifier ('animal_id' for GLATOS data,
#' 		 'catalognumber' for OTN data, or 'animalId' for sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' for
#'     OTN data, or 'timestamp' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 'latitude'
#'     for OTN data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 'longitude'
#'     for OTN data, or 'longitude' for sample data).
#' 
#' @details Uses \code{movePath} to get locations every second/minute
#'
#' @author Angela Dini
#' 
#' @export

showMapPointsId <- function(detections, type="sample", id, colourNames=list(), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  
  if(type == "GLATOS") { #Set column names for GLATOS data
    detColNames = list(animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
  } else if (type == "OTN") { #Set column names for OTN data
    detColNames = list(animalCol="catalognumber", timestampCol="datecollected",latCol="latitude", longCol="longitude")
  } else if (type == "sample") { #Set column names for sample data
    detColNames = list(animalCol="animalId", timestampCol="timestamp", latCol="latitude", longCol="longitude")
  } else { # Other type
    stop(paste0("The type '",type,"' is not defined."), call.=FALSE)
  }
  
  # Check that the specified columns appear in the detections dataframe
  missingCols <- setdiff(unlist(detColNames), names(detections))
  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }
  
  # Subset detections with only user-defined columns and change names
  # this makes code more easy to understand (esp. ddply)
  detections <- detections[,unlist(detColNames)] #subset
  names(detections) <- c("animalId","time","latitude","longitude")
  
  # Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(detections$time))){
    stop(paste0("Column '",detColNames$timestampCol,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  uniq <- unique(detections$animalId)
  nr <- length(uniq)
  len <- length(colourNames)
  # Set 'colourNames' if not already set
  if(len == 0) {
    colourNames=palette(rainbow(nr))
  } else if (len != nr) { # Check that length of colourNames is same as number of unique animal ids
    stop(paste0("length of colourNames does not match number of unique '", detColNames$animalCol, "'"), call.=FALSE)
  }
  
  # Do not get locations every second
  
  # Filter data so that it only contains detections of the specified animal id
  dataS <- detections[detections$animalId==id,]
  
  # Order data by time
  dataS <- dataS[order(dataS$timestamp),]
  
  # Add colour column to data
  dataS$colour <- apply(dataS, 1, function(x) {
    listUniq <- unique(sMapData$animalId)
    n <- match(id, listUniq)
    return(colourNames[[n]])
  })
  
  # From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
  ui <- fluidPage(
    # Application page should have leaflet map
    leafletOutput("mymap")
  )
  
  server <- function(input, output, session) {
    points <- reactive ({
      dataS
    })
    # Define leaflet map as described
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>%
        addTiles() %>%
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=paste0(as.character(points()$a), ", ", as.character(points()$timestamp)), color=points()$colour)
    })
  }
  
  # Run the application
  runApp(list(ui=ui, server=server))
}


#' showMapPoints: shows a leaflet map with the points (detections) of all of the animals
#'   
#' @param detections A data frame containing detection data with at least
#'   4 columns containing 'animal', 'timestamp', 'latitude', and 'longitude'.
#'   Column names are specified with \code{detColNames} by \code{type}.
#' 
#' @param type A character string that contains the type of data that is being 
#'   passed in, for example, "OTN", "GLATOS", or "sample".
#' 
#' @details detColNames is defined as a list with names of required columns in 
#'   \code{detections}, defined by \code{type}: 
#' \itemize{
#'   \item \code{animalCol} is a character string with the name of the column 
#' 		 containing the individual animal identifier ('animal_id' for GLATOS data,
#' 		 'catalognumber' for OTN data, or 'animalId' for sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' for
#'     OTN data, or 'timestamp' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 'latitude'
#'     for OTN data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 'longitude'
#'     for OTN data, or 'longitude' for sample data).
#'
#' @param colourNames optional parameter, a list of Strings containing the name of the
#'   colours used for each animal id, must be same length as number of unique animal ids
#' 
#' @param meanLongitude optional parameter, holds the longitude of the middle of
#'   the base map
#' 
#' @param meanLatitude optional parameter, holds the latitude of the middle of
#'   the base map
#' 
#' @param zoom optional parameter, holds the zoom of the view of the base map
#' 
#' @details Uses \code{movePath} to get locations every second/minute
#'
#' @author Angela Dini
#' 
#' @export

showMapPoints <- function(detections, type="sample", colourNames=list(), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  
  if(type == "GLATOS") { #Set column names for GLATOS data
    detColNames = list(animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
  } else if (type == "OTN") { #Set column names for OTN data
    detColNames = list(animalCol="catalognumber", timestampCol="datecollected",latCol="latitude", longCol="longitude")
  } else if (type == "sample") { #Set column names for sample data
    detColNames = list(animalCol="animalId", timestampCol="timestamp", latCol="latitude", longCol="longitude")
  } else { # Other type
    stop(paste0("The type '",type,"' is not defined."), call.=FALSE)
  }
  
  # Check that the specified columns appear in the detections dataframe
  missingCols <- setdiff(unlist(detColNames), names(detections))
  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }
  
  # Subset detections with only user-defined columns and change names
  # this makes code more easy to understand (esp. ddply)
  detections <- detections[,unlist(detColNames)] #subset
  names(detections) <- c("animalId","time","latitude","longitude")
  
  # Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(detections$time))){
    stop(paste0("Column '",detColNames$timestampCol,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  uniq <- unique(detections$animalId)
  nr <- length(uniq)
  len <- length(colourNames)
  # Set 'colourNames' if not already set
  if(len == 0) {
    colourNames=palette(rainbow(nr))
  } else if (len != nr) { # Check that length of colourNames is same as number of unique animal ids
    stop(paste0("length of colourNames does not match number of unique '", detColNames$animalCol, "'"), call.=FALSE)
  }
  
  # Do not get locations every second, do not filter by id
  
  # Order data by animal id and time
  detections <- detections[order(detections$animalId, detections$time),]
  
  #Add colour column to data
  detections$colour <- apply(detections, 1, function(x) {
    listUniq <- unique(detections$animalId)
    n <- match(x["animalId"], listUniq)
    return(colourNames[[n]])
  })
  
  # From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
  ui <- fluidPage(
    # Application page should have leaflet map
    leafletOutput("mymap")
  )
  
  server <- function(input, output, session) {
    points <- reactive ({
      detections
    })
    # Define leaflet map as described
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>%
        addTiles() %>%
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=paste0(as.character(points()$a), ", ", as.character(points()$timestamp)), color=points()$colour)
    })
  }
  
  # Run the application
  runApp(list(ui=ui, server=server))
}


#' showIdMapFollow: shows a leaflet map that follows animals
#'  with a specific id defined by \code{id}, depicts these animals with circles 
#'  of different colours
#' 
#' @param detections A data frame containing detection data with at least
#'   4 columns containing 'animal', 'timestamp', 'latitude', and 'longitude'.
#'   Column names are specified with \code{detColNames} by \code{type}.
#' 
#' @param type A character string that contains the type of data that is being 
#'   passed in, for example, "OTN", "GLATOS", or "sample".
#' 
#' @param id A character string that contains the animal identifier to examine
#' 
#' @param colourNames optional parameter, a list of Strings containing the name of the
#'   colours used for each animal id, must be same length as number of unique animal ids
#' 
#' @param meanLongitude optional parameter, holds the longitude of the middle of
#'   the base map
#' 
#' @param meanLatitude optional parameter, holds the latitude of the middle of
#'   the base map
#' 
#' @param zoom optional parameter, holds the zoom of the view of the base map
#' 
#' @details detColNames is defined as a list with names of required columns in 
#'   \code{detections}, defined by \code{type}: 
#' \itemize{
#'   \item \code{animalCol} is a character string with the name of the column 
#' 		 containing the individual animal identifier ('animal_id' for GLATOS data,
#' 		 'catalognumber' for OTN data, or 'animalId' for sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' for
#'     OTN data, or 'timestamp' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 'latitude'
#'     for OTN data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 'longitude'
#'     for OTN data, or 'longitude' for sample data).
#' 
#' @details Uses \code{movePath} to get locations every second/minute
#'
#' @author Angela Dini
#' 
#' @export

showIdMapFollow <- function(detections, type="sample", id, colourNames=list(), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  
  if(type == "GLATOS") { #Set column names for GLATOS data
    detColNames = list(animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
  } else if (type == "OTN") { #Set column names for OTN data
    detColNames = list(animalCol="catalognumber", timestampCol="datecollected",latCol="latitude", longCol="longitude")
  } else if (type == "sample") { #Set column names for sample data
    detColNames = list(animalCol="animalId", timestampCol="timestamp", latCol="latitude", longCol="longitude")
  } else { # Other type
    stop(paste0("The type '",type,"' is not defined."), call.=FALSE)
  }
  
  # Check that the specified columns appear in the detections dataframe
  missingCols <- setdiff(unlist(detColNames), names(detections))
  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '",missingCols,"'", collapse="\n")), 
         call.=FALSE)
  }
  
  # Subset detections with only user-defined columns and change names
  # this makes code more easy to understand (esp. ddply)
  detections <- detections[,unlist(detColNames)] #subset
  names(detections) <- c("animalId","time","latitude","longitude")
  
  # Check that timestamp is of class 'POSIXct'
  if(!('POSIXct' %in% class(detections$time))){
    stop(paste0("Column '",detColNames$timestampCol,
                "' in the detections dataframe must be of class 'POSIXct'."),
         call.=FALSE)
  }
  
  uniq <- unique(detections$animalId)
  nr <- length(uniq)
  len <- length(colourNames)
  # Set 'colourNames' if not already set
  if(len == 0) {
    colourNames=palette(rainbow(nr))
  } else if (len != nr) { # Check that length of colourNames is same as number of unique animal ids
    stop(paste0("length of colourNames does not match number of unique '", detColNames$animalCol, "'"), call.=FALSE)
  }
  
  # Get location at every second
  mdSplit <- split(detections, detections$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    anId <- mdI$animalId[[1]]
    for (t in 1:(length(mdI$time)-1)) {
      if(length(mdI$time)>=2 && !is.na(mdI$time[[t]]) && !is.na(mdI$time[[t+1]]) && !is.na(mdI$latitude[[t]]) && !is.na(mdI$longitude[[t]]) && !is.na(mdI$latitude[[t+1]]) && !is.na(mdI$longitude[[t+1]])) {
        # Get sequence of seconds of time between the two times
        tS <- seq(mdI$time[[t]], mdI$time[[t+1]], by="sec")
        if(length(tS) > 1) {
          # Get dataframe of longitude and latitude of each time
          locs <- movePath(mdI$longitude[[t]], mdI$latitude[[t]], mdI$longitude[[t+1]], mdI$latitude[[t+1]], tS)
          
          # Clean up data to match detections dataframe format
          locs$latitude <- locs$lat
          locs$lat <- NULL
          locs$longitude <- locs$lon
          locs$lon <- NULL
          locs$animalId <- rep(x=anId, times=nrow(locs))
          detections <- rbind(detections, locs)
        }
      }
    }
  }
  dataS <- detections[detections$animalId==id,]
  dataS <- dataS[order(dataS$timestamp),]
  
  
  #Adding colour column to dataS
  dataS$colour <- apply(dataS, 1, function(x) {
    listUniq <- unique(detections$animalId)
    n <- match(id, listUniq)
    return(colourNames[[n]])
  })
  
  # From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
  ui <- fluidPage(
    # Application page should have slider and leaflet map
    sliderInput("time", "date", min(dataS$time),
                max(dataS$timestamp),
                value=max(dataS$timestamp),
                step=1,
                animate=animationOptions(interval=400, loop=TRUE)),
    leafletOutput("mymap")
  )
  
  server <- function(input, output, session) {
    points <- reactive ({
      dataS %>%
        filter(dataS$timestamp==input$time)
    })
    # Define leaflet map as described
    # Does not have setView
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=as.character(points()$a), color=points()$colour)
    })
  }
  # Run the application
  runApp(list(ui=ui, server=server))
}



















# To run:

# Sample data:
# Data 1: All 3 fish (Alice, Bob, and Eve) present at all times
anId <- c("Alice", "Bob", "Eve", "Alice", "Bob", "Eve", "Alice", "Bob", "Eve")
timeA <- rep(x="2010/10/11 11:11:11", times=3)
timeB <- rep(x="2010/10/11 11:12:11", times=3)
timeC <- rep(x="2010/10/11 11:13:11", times=3)
times <- c(timeA, timeB, timeC)
times <- as.POSIXct(times, tz="America/Halifax")
long <- c(-63.575993, -63.580284, -63.626032, -63.604145, -63.587708, -63.575306, -63.612943, -63.618736, -63.568354)
lat <- c(44.652902, 44.631772, 44.665845, 44.655283, 44.645422, 44.642827, 44.642665, 44.649300, 44.640201)
smData <- data.frame(animalId=anId, timestamp=times,longitude=long, latitude=lat)
iF <- c("/Users/dinian/Desktop/glatos-git/R/visualization/Icons/redFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/blueFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/greenFish.png")
cF <- c("red", "blue", "green")

# #Data 2: Blue fish detected for one second but does not have path
# anId <- c(Alice, Bob, Alice)
# times <- c("2010/10/11 11:00:00", "2010/10/11 11:00:15", "2010/10/11 11:00:30")
# times <- as.POSIXct(times, tz="America/Halifax")
# long <- c(-63.626032, -63.618736, -63.580284)
# lat <- c(44.665845, 44.649300, 44.631772)
# smData <- data.frame(animalId=anId, timestamp=times,longitude=long, latitude=lat)
# iF <- c("/Users/dinian/Desktop/glatos-git/R/visualization/Icons/redFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/blueFish.png")
# cF <- c("red", "blue", "green")

# #Data 3: Bob(blue fish) detected after Alice(red fish) and leaves before Alice(red fish) but has path
# anId <- c(Alice, Alice, Bob, Alice, Bob, Alice, Alice)
# times <- c("2010/10/11 11:00:00", "2010/10/11 11:00:07", "2010/10/11 11:00:10", "2010/10/11 11:00:15", "2010/10/11 11:00:20", "2010/10/11 11:00:24", "2010/10/11 11:00:30")
# times <- as.POSIXct(times, tz="America/Halifax")
# long <- c(-63.62603, -63.61874, -63.58028, -63.5917, -63.60415, -63.58063, -63.56861)
# lat <- c(44.66584, 44.64930, 44.63177, 44.6366, 44.65528, 44.64747, 44.62328)
# smData <- data.frame(animalId=anId, timestamp=times,longitude=long, latitude=lat)
# iF <- c("/Users/dinian/Desktop/glatos-git/R/visualization/Icons/redFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/blueFish.png")
# cF <- c("red", "blue", "green")

# Show full animated map with icons:
showMapIcon(detections=smData, iconFiles = iF)
# OR (using default iconFiles)
showMapIcon(smData)

# Show full animated map with all species with coloured circles
showMapCircle(smData, cF)
# OR (using colour palette)
n <- length(unique(smData$animalId))
showMapCircle(smData, palette(rainbow(n)))


# Show animated map from a different position (viewed into Fairview)
showMapIcon(smData, iF, meanLatitude = 44.66584, meanLongitude = -63.62603, zoom=15)
showMapCircle(smData, iF, meanLatitude = 44.66584, meanLongitude = -63.62603, zoom=14)


# Show animated map with animals of specific animal id ("Eve"):
showIdMap(smData, "Eve", cF)

# Show map following animals with specific animal id ("Eve")
showIdMapFollow(smData, "Eve", cF)

# Show paths / points on map of animals of specific animal id ("Eve"):
showMapPathId(smData, "Eve", cF)

# Show paths of all animals
showMapPaths(smData, cF)

# Show all tags of animals
showMapPoints(smData, cF)

# Show all animals with id defined in 'id' ("Eve")
showMapPointsId(smData, "Eve", cF)


#Can do:
#For showMapIcon
showMapIcon(detections=smData, type="sample", iconFiles = iF, meanLongitude=-63.5904, meanLatitude=44.6474, zoom=14)
showMapIcon(detections=smData, iconFiles = iF, meanLongitude=-63.5904, meanLatitude=44.6474, zoom=14)
showMapIcon(detections=smData, iconFiles = iF)
showMapIcon(detections=smData)

#For showMapCircle
showMapCircle(detections=smData, type="sample", colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=14)
showMapCircle(detections=smData, colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=14)
showMapCircle(detections=smData, colourNames=cF)
showMapCircle(detections=smData)

#For showIdMap
showIdMap(detections=smData, type="sample", id="Eve", colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=14)
showIdMap(detections=smData, id="Eve", colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=14)
showIdMap(detections=smData, id="Eve", colourNames=palette(rainbow(3)))
showIdMap(detections=smData, id="Eve")

#For showIdMapFollow
showIdMapFollow(detections=smData, type="sample", id="Eve", colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=14)
showIdMapFollow(detections=smData, id="Eve", colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=14)
showIdMapFollow(detections=smData, id="Eve", colourNames=palette(rainbow(3)))
showIdMapFollow(detections=smData, id="Eve")

#For showMapPathId
showMapPathId(detections=smData, type="sample", id="Eve", colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=14)
showMapPathId(detections=smData, id="Eve", colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=14)
showMapPathId(detections=smData, id="Eve", colourNames=palette(rainbow(3)))
showMapPathId(detections=smData, id="Eve")


#For showMapPaths
showMapPaths(detections=smData, type="sample", colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=14)
showMapPaths(detections=smData, colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=14)
showMapPaths(detections=smData, colourNames=palette(rainbow(3)))
showMapPaths(detections=smData)

#For showMapPoints
showMapPoints(detections=smData, type="sample", colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=14)
showMapPoints(detections=smData, colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=14)
showMapPoints(detections=smData, colourNames=palette(rainbow(3)))
showMapPoints(detections=smData)

#For showMapPointsId
showMapPointsId(detections=smData, type="sample", id="Eve", colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=14)
showMapPointsId(detections=smData, id="Eve", colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=14)
showMapPointsId(detections=smData, id="Eve", colourNames=palette(rainbow(3)))
showMapPointsId(detections=smData, id="Eve")