# library(shiny)
# library(xts)
# library(dplyr)
# library(leaflet)

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
#'   passed in, for example, "OTNDet", "OTNQual", "GLATOS", or "sample".
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
#' 		 'catalognumber' for OTN detections or qualified data, or 'animalId' for 
#' 		 sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' for
#'     OTN detections or qualified data, or 'timestamp' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 'latitude'
#'     for OTN detections or qualified data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 'longitude'
#'     for OTN detections or qualified data, or 'longitude' for sample data).
#' 
#' @details Uses \code{movePath} to get locations every second/minute
#'
#' @author Angela Dini
#' 
#' @export

# Show map of all animals with icons
showMapIcon <- function(detections, type,
                        iconFiles=c("/Users/dinian/Desktop/glatos-git/R/visualization/Icons/redFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/blueFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/greenFish.png"), 
                        meanLongitude=0, meanLatitude=0, zoom=13) {
  
  if(type == "GLATOS") { #Set column names for GLATOS data
    detColNames = list(animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
  } else if (type == "OTNDet" || type=="OTNQual") { #Set column names for OTN data
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
  
  # Set mean longitude and mean latitude if not already set
  if (meanLongitude == 0 && meanLatitude == 0) {
    meanLongitude <- mean(detections$longitude)
    meanLatitude <- mean(detections$latitude)
  }
  
  # Get location at every second
  mdSplit <- split(detections, detections$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    #Order each split of data by time
    mdI <- mdI[order(mdI$time),]
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
        addMarkers(lat = points()$latitude, lng=points()$longitude, label=paste0("id: ", as.character(points()$a)), icon=makeIcon(points()$icon, iconWidth = 39, iconHeight=24))
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
#'   passed in, for example, "OTNDet", "OTNQual", "GLATOS", or "sample".
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
#' 		 'catalognumber' for OTN detections or qualified data, or 'animalId' for 
#' 		 sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' for
#'     OTN detections or qualified data, or 'timestamp' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 'latitude'
#'     for OTN detections or qualified data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 'longitude'
#'     for OTN detections or qualified data, or 'longitude' for sample data).
#'
#' @details Uses \code{movePath} to get locations every second/minute
#'
#' @author Angela Dini
#' 
#' @export

#Show map with coloured circle markers (Alice = red, Bob = blue, Eve = green)
showMapCircle <- function(detections, type, colourNames=list(), meanLongitude=0, meanLatitude=0, zoom=13) {
  
  if(type == "GLATOS") { #Set column names for GLATOS data
    detColNames = list(animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
  } else if (type == "OTNDet" || type == "OTNQual") { #Set column names for OTN data
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
  
  # Set mean longitude and mean latitude if not already set
  if (meanLongitude == 0 && meanLatitude == 0) {
    meanLongitude <- mean(detections$longitude)
    meanLatitude <- mean(detections$latitude)
  }
  
  # Get location at every second
  mdSplit <- split(detections, detections$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    #Order each split of data by time
    mdI <- mdI[order(mdI$time),]
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
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=paste0("id: ", as.character(points()$a)), color=points()$colour)
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
#'   passed in, for example, "OTNDet", "OTNQual", "GLATOS", or "sample".
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
#' 		 'catalognumber' for OTN detections or qualified data, or 'animalId' for 
#' 		 sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' for
#'     OTN detections or qualified data, or 'timestamp' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 'latitude'
#'     for OTN detections or qualified data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 'longitude'
#'     for OTN detections or qualified data, or 'longitude' for sample data).
#'
#' @details Uses \code{movePath} to get locations every second/minute
#'
#' @author Angela Dini
#' 
#' @export

showIdMap <- function(detections, type, id, colourNames=list(), meanLongitude=0, meanLatitude=0, zoom=13) {
  
  if(type == "GLATOS") { #Set column names for GLATOS data
    detColNames = list(animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
  } else if (type == "OTNDet" || type=="OTNQual") { #Set column names for OTN data
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
  
  # Set mean longitude and mean latitude if not already set
  if (meanLongitude == 0 && meanLatitude == 0) {
    meanLongitude <- mean(detections$longitude)
    meanLatitude <- mean(detections$latitude)
  }
  
  # Get location at every second
  mdSplit <- split(detections, detections$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    #Order each split of data by time
    mdI <- mdI[order(mdI$time),]
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
          
          # Merge the two dataframes
          detections <- rbind(detections, locs)
        }
      }
    }
  }
  
  #Filter data so that it only contains detections of the specified animal id
  dataS <- detections[detections$animalId==id,]
  
  # Order dataS by time
  dataS <- dataS[order(dataS$time),]
  
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
                max(dataS$time),
                value=max(dataS$time),
                step=1,
                animate=animationOptions(interval=400, loop=TRUE)),
    leafletOutput("mymap")
  )
  
  server <- function(input, output, session) {
    points <- reactive ({
      dataS %>%
        filter(dataS$time==input$time)
    })
    # Define leaflet map as described
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>%
        addTiles() %>%
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=paste0("id: ", as.character(points()$a)), color=points()$colour)
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
#'   passed in, for example, "OTNDet", "OTNQual", "GLATOS", or "sample".
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
#' 		 'catalognumber' for OTN detections or qualified data, or 'animalId' for 
#' 		 sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' for
#'     OTN detections or qualified data, or 'timestamp' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 'latitude'
#'     for OTN detections or qualified data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 'longitude'
#'     for OTN detections or qualified data, or 'longitude' for sample data).
#' 
#' @details Uses \code{movePath} to get locations every second/minute
#'
#' @author Angela Dini
#' 
#' @export

showMapPathId <- function(detections, type, id, colourNames=list(), meanLongitude=0, meanLatitude=0, zoom=13) {
  
  if(type == "GLATOS") { #Set column names for GLATOS data
    detColNames = list(animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
  } else if (type == "OTNDet" || type == "OTNQual") { #Set column names for OTN data
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
  
  # Set mean longitude and mean latitude if not already set
  if (meanLongitude == 0 && meanLatitude == 0) {
    meanLongitude <- mean(detections$longitude)
    meanLatitude <- mean(detections$latitude)
  }
  
  # Get location at every second
  mdSplit <- split(detections, detections$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    #Order each split of data by time
    mdI <- mdI[order(mdI$time),]
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
          locs$time <- locs$timestamp
          locs$timestamp <- NULL
          
          # Merge the two dataframes
          detections <- rbind(detections, locs)
        }
      }
    }
  }
  
  # Filter data so that it only contains detections of the specified animal id
  dataS <- detections[detections$animalId==id,]
  
  # Order data by time
  dataS <- dataS[order(dataS$time),]
  
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
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=paste0("id: ", as.character(points()$a)), color=points()$colour)
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
#'   passed in, for example, "OTNDet", "OTNQual", "GLATOS", or "sample".
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
#' 		 'catalognumber' for OTN detections or qualified data, or 'animalId' for 
#' 		 sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' for
#'     OTN detections or qualified data, or 'timestamp' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 'latitude'
#'     for OTN detections or qualified data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 'longitude'
#'     for OTN detections or qualified data, or 'longitude' for sample data).
#'     
#' @details Uses \code{movePath} to get locations every second/minute
#'
#' @author Angela Dini
#' 
#' @export

showMapPaths <- function(detections, type, colourNames=list(), meanLongitude=0, meanLatitude=0, zoom=13) {
  
  if(type == "GLATOS") { #Set column names for GLATOS data
    detColNames = list(animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
  } else if (type == "OTNDet" || type == "OTNQual") { #Set column names for OTN data
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
  
  # Set mean longitude and mean latitude if not already set
  if (meanLongitude == 0 && meanLatitude == 0) {
    meanLongitude <- mean(detections$longitude)
    meanLatitude <- mean(detections$latitude)
  }
  
  # Get location at every second
  mdSplit <- split(detections, detections$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    #Order each split of data by time
    mdI <- mdI[order(mdI$time),]
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
          
          # Merge the two dataframes
          detections <- rbind(detections, locs)
        }
      }
    }
  }
  
  # Do not filter by id, no slider
  
  # Order data by animal id and time
  sMapData <- detections[order(detections$animalId, detections$time),]
  
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
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=paste0("id: ",as.character(points()$a)), color=points()$colour)
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
#'   passed in, for example, "OTNDet", "OTNQual", "GLATOS", or "sample".
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
#' 		 'catalognumber' for OTN detections or qualified data, or 'animalId' for 
#' 		 sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' for
#'     OTN detections or qualified data, or 'timestamp' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 'latitude'
#'     for OTN detections or qualified data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 'longitude'
#'     for OTN detections or qualified data, or 'longitude' for sample data).
#' 
#' @details Uses \code{movePath} to get locations every second/minute
#'
#' @author Angela Dini
#' 
#' @export

showMapPointsId <- function(detections, type, id, colourNames=list(), meanLongitude=0, meanLatitude=0, zoom=13) {
  
  if(type == "GLATOS") { #Set column names for GLATOS data
    detColNames = list(animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
  } else if (type == "OTNDet" || type == "OTNQual") { #Set column names for OTN data
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
  
  # Set mean longitude and mean latitude if not already set
  if (meanLongitude == 0 && meanLatitude == 0) {
    meanLongitude <- mean(detections$longitude)
    meanLatitude <- mean(detections$latitude)
  }
  
  # Do not get locations every second
  
  # Filter data so that it only contains detections of the specified animal id
  dataS <- detections[detections$animalId==id,]
  
  # Order data by time
  dataS <- dataS[order(dataS$time),]
  
  # Add colour column to data
  dataS$colour <- apply(dataS, 1, function(x) {
    listUniq <- unique(detections$animalId)
    n <- match(id, listUniq)
    if(is.na(n)) {
      stop(paste0("The id '", id, "' does not exist in this data."))
    } else {
      return(colourNames[[n]])
    }
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
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=paste0("id: ",as.character(points()$a), ", time: ", as.character(points()$time)), color=points()$colour)
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
#'   passed in, for example, "OTNDet", "OTNQual", "GLATOS", or "sample".
#' 
#' @details detColNames is defined as a list with names of required columns in 
#'   \code{detections}, defined by \code{type}: 
#' \itemize{
#'   \item \code{animalCol} is a character string with the name of the column 
#' 		 containing the individual animal identifier ('animal_id' for GLATOS data,
#' 		 'catalognumber' for OTN detections or qualified data, or 'animalId' for 
#' 		 sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' for
#'     OTN detections or qualified data, or 'timestamp' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 'latitude'
#'     for OTN detections or qualified data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 'longitude'
#'     for OTN detections or qualified data, or 'longitude' for sample data).
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

showMapPoints <- function(detections, type, colourNames=list(), meanLongitude=0, meanLatitude=0, zoom=13) {
  
  if(type == "GLATOS") { #Set column names for GLATOS data
    detColNames = list(animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
  } else if (type == "OTNDet" || type == "OTNQual") { #Set column names for OTN data
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
  
  # Set mean longitude and mean latitude if not already set
  if (meanLongitude == 0 && meanLatitude == 0) {
    meanLongitude <- mean(detections$longitude)
    meanLatitude <- mean(detections$latitude)
  }
  
  # Do not get locations every second, do not filter by id
  
  # Order data by animal id and time
  detections <- detections[order(detections$animalId, detections$time),]
  
  #Add colour column to data
  detections$colour <- apply(detections, 1, function(x) {
    listUniq <- unique(detections$animalId)
    #print(listUniq)
    #Remove whitespace from animalId
    #anId <- gsub(" ","", x["animalId"], fixed=TRUE)
    #anId <- x["animalId"]
    #print(anId)
    n <- which(listUniq == x["animalId"]) # Find index of id in list of unique ids
    #print(paste0("'", anId, "' : ", n))
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
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=paste0("id: ", as.character(points()$a), ", time: ", as.character(points()$time)), color=points()$colour)
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
#'   passed in, for example, "OTNDet", "OTNQual", "GLATOS", or "sample".
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
#' 		 'catalognumber' for OTN detections or qualified data, or 'animalId' for 
#' 		 sample data).
#'	 \item \code{timestampCol} is a character string with the name of the column 
#' 		 containing datetime stamps for the detections (MUST be of class 
#'     'POSIXct') ('detection_timestamp_utc' for GLATOS data, 'datecollected' for
#'     OTN detections or qualified data, or 'timestamp' for sample data).
#'	 \item \code{latitudeCol} is a character string with the name of the column
#'     containing latitude of the receiver ('deploy_lat' for GLATOS data, 'latitude'
#'     for OTN detections or qualified data, or 'latitude' for sample data).
#'	 \item \code{longitudeCol} is a character string with the name of the column
#'     containing longitude of the receiver ('deploy_long' for GLATOS data, 'longitude'
#'     for OTN detections or qualified data, or 'longitude' for sample data).
#' 
#' @details Uses \code{movePath} to get locations every second/minute
#'
#' @author Angela Dini
#' 
#' @export

showIdMapFollow <- function(detections, type, id, colourNames=list(), meanLongitude=0, meanLatitude=0, zoom=13) {
  
  if(type == "GLATOS") { #Set column names for GLATOS data
    detColNames = list(animalCol="animal_id", timestampCol="detection_timestamp_utc",latCol="deploy_lat", longCol="deploy_long")
  } else if (type == "OTNDet" || type == "OTNQual") { #Set column names for OTN data
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
  
  # Set mean longitude and mean latitude if not already set
  if (meanLongitude == 0 && meanLatitude == 0) {
    meanLongitude <- mean(detections$longitude)
    meanLatitude <- mean(detections$latitude)
  }
  
  # Get location at every second
  mdSplit <- split(detections, detections$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    #Order each split of data by time
    mdI <- mdI[order(mdI$time),]
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
          locs$time <- locs$timestamp
          locs$timestamp <- NULL
          
          # Merge the two dataframes
          detections <- rbind(detections, locs)
        }
      }
    }
  }
  
  # Filter data to include only animals with the specified id
  dataS <- detections[detections$animalId==id,]
  
  # Order data by time
  dataS <- dataS[order(dataS$time),]
  
  # Add colour column to dataS
  dataS$colour <- apply(dataS, 1, function(x) {
    listUniq <- unique(detections$animalId)
    n <- match(id, listUniq)
    return(colourNames[[n]])
  })
  
  # From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
  ui <- fluidPage(
    # Application page should have slider and leaflet map
    sliderInput("time", "date", min(dataS$time),
                max(dataS$time),
                value=max(dataS$time),
                step=1,
                animate=animationOptions(interval=400, loop=TRUE)),
    leafletOutput("mymap")
  )
  
  server <- function(input, output, session) {
    points <- reactive ({
      dataS %>%
        filter(dataS$time==input$time)
    })
    # Define leaflet map as described
    # Does not have setView
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=paste0("id: ",as.character(points()$a)), color=points()$colour)
    })
  }
  # Run the application
  runApp(list(ui=ui, server=server))
}