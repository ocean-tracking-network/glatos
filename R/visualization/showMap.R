library(shiny)
library(xts)
library(dplyr)
library(leaflet)

#http://www.r-graph-gallery.com/2017/03/14/4-tricks-for-working-with-r-leaflet-and-shiny/

#' Shows leaflet maps as a shiny app with timelines of animal movements
#' Can also filter by id and show different types of maps





#' showMapIcon: shows an animated leaflet map with the movements of all the
#'   animals, depicts animals with icons on local disk
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
#' @param iconFiles optional parameter, holds locations of files that hold images
#'   of icons
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
  
  
  
  
  
  
  
  # Get location at every second
  mdSplit <- split(detections, detections$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    anId <- mdI$animalId[[1]]
    for (t in 1:(length(mdI$time)-1)) {
      if(length(mdI$time)<2) {
      }
      else if(!is.na(mdI$time[[t]]) && !is.na(mdI$time[[t+1]]) && !is.na(mdI$latitude[[t]]) && !is.na(mdI$longitude[[t]]) && !is.na(mdI$latitude[[t+1]]) && !is.na(mdI$longitude[[t+1]])) {
        tS <- seq(mdI$time[[t]], mdI$time[[t+1]], by="sec")
        if(length(tS) == 0 || length(tS) == 1) {
          #print("ts is too small")
        }
        else {
          locs <- movePath(mdI$longitude[[t]], mdI$latitude[[t]], mdI$longitude[[t+1]], mdI$latitude[[t+1]], tS)
          
          # Clean up data to match dataS
          locs$latitude <- locs$lat
          locs$lat <- NULL
          locs$longitude <- locs$lon
          locs$lon <- NULL
          locs$animalId <- rep(x=anId, times=nrow(locs))
          locs$time <- locs$timestamp
          locs$timestamp <- NULL
          print(locs)
          sMapData <- rbind(detections, locs)
        }
      }
    }
  }
  sMapData <- sMapData[order(sMapData$animalId, sMapData$time),]
  
  #Adding icons column to sMapData
  sMapData$icon <- apply(sMapData, 1, function(x) {
    listUniq <- unique(sMapData$animalId)
    n <- match(x["animalId"], listUniq)
    return(iconFiles[[n]])
    # n <- x["animalId"]
    # return(iconFiles[[as.integer(n)]])
  })
  
  #print(sMapData)
  
  # From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
  ui <- fluidPage(
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
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>%
        addTiles() %>%
        #Original: addMarkers(data = points(), label = as.character(points()$a), icon = makeIcon(points()$icon, iconWidth = 39, iconHeight = 24))
        #addMarkers(lat = points()$latitude, lng = points()$longitude, label = as.character(points()$anId), icon = makeIcon(points()$icon, iconWidth = 39, iconHeight = 24))
        addMarkers(lat = points()$latitude, lng=points()$longitude, label=as.character(points()$a), icon=makeIcon(points()$icon, iconWidth = 39, iconHeight=24))
    })
  }
  #shinyApp(ui, server)
  runApp(list(ui=ui, server=server))
}








# Get location at every second
mdSplit <- split(detections, detections$animal) #Split data by id
for(i in 1: length(mdSplit)) {
  mdI <- mdSplit[[i]]
  anId <- mdI$animal[[1]]
  for (t in 1:(length(mdI$timestamp)-1)) {
    if(length(mdI$timestamp)>=2 && !is.na(mdI$timestamp[[t]]) && !is.na(mdI$timestamp[[t+1]]) && !is.na(mdI$latitude[[t]]) && !is.na(mdI$longitude[[t]]) && !is.na(mdI$latitude[[t+1]]) && !is.na(mdI$longitude[[t+1]])) {
      # Gets sequence of every second between each pair of times
      tS <- seq(mdI$timestamp[[t]], mdI$timestamp[[t+1]], by="sec")
      if(length(tS) > 1){
        # Use 'movePath' method to get location at every second
        locs <- movePath(mdI$longitude[[t]], mdI$latitude[[t]], mdI$longitude[[t+1]], mdI$latitude[[t+1]], tS)
        
        # Clean up data to match dataS
        locs$latitude <- locs$lat
        locs$lat <- NULL
        locs$longitude <- locs$lon
        locs$lon <- NULL
        locs$animal <- rep(x=anId, times=nrow(locs))
        print(locs)
        mapData <- rbind(detections, locs)
      }
    }
  }
}
# Order by id and time
mapData <- mapData[order(mapData$animal, mapData$timestamp),]

#Adding icon column to mapData to associate each data instance with an icon
mapData$icon <- apply(mapData, 1, function(x) {
  listUniq <- unique(mapData$animal)
  n <- match(x["animal"], listUniq)
  return(iconFiles[[n]])
})

# From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
ui <- fluidPage(
  # Timeline slider
  sliderInput("time", "date", min(mapData$timestamp),
              max(mapData$timestamp),
              value=max(mapData$timestamp),
              step=1,
              animate=animationOptions(interval=400, loop=TRUE)), #Loop continuously through the time
  leafletOutput("mymap") #Leaflet map
)

server <- function(input, output, session) {
  points <- reactive ({
    mapData %>%
      filter(mapData$timestamp==input$time) #Points for the specific time
  })
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>% #Setting view with specified parameters
      addTiles() %>% #Adding base map
      addMarkers(lat = points()$latitude, lng=points()$longitude, label=paste(as.character(points()$a), ", ", points()$timestamp), icon=makeIcon(points()$icon, iconWidth = 39, iconHeight=24)) #Adding markers with their icons, labelled with the id and timestamp
  })
}
runApp(list(ui=ui, server=server)) #Show map
}
























#Show map with coloured circle markers (Alice = red, Bob = blue, Eve = green)
showMapCircle <- function(sMapData, colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  # Get location at every second
  mdSplit <- split(sMapData, sMapData$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    anId <- mdI$animalId[[1]]
    for (t in 1:(length(mdI$time)-1)) {
      if(length(mdI$time)>=2 && !is.na(mdI$time[[t]]) && !is.na(mdI$time[[t+1]]) && !is.na(mdI$latitude[[t]]) && !is.na(mdI$longitude[[t]]) && !is.na(mdI$latitude[[t+1]]) && !is.na(mdI$longitude[[t+1]])) {
        tS <- seq(mdI$time[[t]], mdI$time[[t+1]], by="sec")
        if(length(tS > 1)) {
          locs <- movePath(mdI$longitude[[t]], mdI$latitude[[t]], mdI$longitude[[t+1]], mdI$latitude[[t+1]], tS)
          
          # Clean up data to match dataS
          locs$latitude <- locs$lat
          locs$lat <- NULL
          locs$longitude <- locs$lon
          locs$lon <- NULL
          locs$animalId <- rep(x=anId, times=nrow(locs))
          sMapData <- rbind(sMapData, locs)
        }
      }
    }
  }
  sMapData <- sMapData[order(sMapData$animalId, sMapData$timestamp),]
  
  #Adding colour column to sMapData
  sMapData$colour <- apply(sMapData, 1, function(x) {
    listUniq <- unique(sMapData$animalId)
    n <- match(x["animalId"], listUniq)
    return(colourNames[[n]])
  })
  
  # From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
  ui <- fluidPage(
    sliderInput("time", "date", min(sMapData$time),
                max(sMapData$timestamp),
                value=max(sMapData$timestamp),
                step=1,
                animate=animationOptions(interval=400, loop=TRUE)),
    leafletOutput("mymap")
  )
  
  server <- function(input, output, session) {
    points <- reactive ({
      sMapData %>%
        filter(sMapData$timestamp==input$time)
    })
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>%
        addTiles() %>%
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=as.character(points()$a), color=points()$colour)
    })
  }
  #shinyApp(ui, server)
  runApp(list(ui=ui, server=server))
}

#Show only animals with a specific id
# Using coloured circle markers
showIdMap <- function(sMapData, id, colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  # Get location at every second
  mdSplit <- split(sMapData, sMapData$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    anId <- mdI$animalId[[1]]
    for (t in 1:(length(mdI$time)-1)) {
      if(length(mdI$time)>=2 && !is.na(mdI$time[[t]]) && !is.na(mdI$time[[t+1]]) && !is.na(mdI$latitude[[t]]) && !is.na(mdI$longitude[[t]]) && !is.na(mdI$latitude[[t+1]]) && !is.na(mdI$longitude[[t+1]])) {
        tS <- seq(mdI$time[[t]], mdI$time[[t+1]], by="sec")
        if(length(tS) > 1) {
          locs <- movePath(mdI$longitude[[t]], mdI$latitude[[t]], mdI$longitude[[t+1]], mdI$latitude[[t+1]], tS)
          
          # Clean up data to match dataS
          locs$latitude <- locs$lat
          locs$lat <- NULL
          locs$longitude <- locs$lon
          locs$lon <- NULL
          locs$animalId <- rep(x=anId, times=nrow(locs))
          sMapData <- rbind(sMapData, locs)
        }
      }
    }
  }
  dataS <- sMapData[sMapData$animalId==id,]
  dataS <- dataS[order(dataS$timestamp),]
  
  #Adding colour column to dataS
  dataS$colour <- apply(dataS, 1, function(x) {
    listUniq <- unique(sMapData$animalId)
    n <- match(id, listUniq)
    return(colourNames[[n]])
  })
  
  # From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
  ui <- fluidPage(
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
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>%
        addTiles() %>%
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=as.character(points()$a), color=points()$colour)
    })
  }
  runApp(list(ui=ui, server=server))
}


# Show path of animals with id defined in 'id'
showMapPathId <- function(sMapData, id, colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  # Get location at every second
  mdSplit <- split(sMapData, sMapData$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    anId <- mdI$animalId[[1]]
    for (t in 1:(length(mdI$time)-1)) {
      if(length(mdI$time)>=2 && !is.na(mdI$time[[t]]) && !is.na(mdI$time[[t+1]]) && !is.na(mdI$latitude[[t]]) && !is.na(mdI$longitude[[t]]) && !is.na(mdI$latitude[[t+1]]) && !is.na(mdI$longitude[[t+1]])) {
        tS <- seq(mdI$time[[t]], mdI$time[[t+1]], by="sec")
        if(length(tS) > 1) {
          locs <- movePath(mdI$longitude[[t]], mdI$latitude[[t]], mdI$longitude[[t+1]], mdI$latitude[[t+1]], tS)
          
          # Clean up data to match dataS
          locs$latitude <- locs$lat
          locs$lat <- NULL
          locs$longitude <- locs$lon
          locs$lon <- NULL
          locs$animalId <- rep(x=anId, times=nrow(locs))
          sMapData <- rbind(sMapData, locs)
        }
      }
    }
  }
  dataS <- sMapData[sMapData$animalId==id,]
  dataS <- dataS[order(dataS$timestamp),]
  
  
  #Adding colour column to dataS
  dataS$colour <- apply(dataS, 1, function(x) {
    listUniq <- unique(sMapData$animalId)
    n <- match(id, listUniq)
    return(colourNames[[n]])
  })
  
  # From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
  ui <- fluidPage(
    leafletOutput("mymap")
  )
  
  server <- function(input, output, session) {
    points <- reactive ({
      dataS
    })
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>%
        addTiles() %>%
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=as.character(points()$a), color=points()$colour)
    })
  }
  runApp(list(ui=ui, server=server))
}

# Show paths of all animals
showMapPaths <- function(sMapData, colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  # Get location at every second
  mdSplit <- split(sMapData, sMapData$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    anId <- mdI$animalId[[1]]
    for (t in 1:(length(mdI$time)-1)) {
      if(length(mdI$time)>=2 && !is.na(mdI$time[[t]]) && !is.na(mdI$time[[t+1]]) && !is.na(mdI$latitude[[t]]) && !is.na(mdI$longitude[[t]]) && !is.na(mdI$latitude[[t+1]]) && !is.na(mdI$longitude[[t+1]])) {
        tS <- seq(mdI$time[[t]], mdI$time[[t+1]], by="sec")
        if(length(tS) > 1) {
          locs <- movePath(mdI$longitude[[t]], mdI$latitude[[t]], mdI$longitude[[t+1]], mdI$latitude[[t+1]], tS)
          
          # Clean up data to match dataS
          locs$latitude <- locs$lat
          locs$lat <- NULL
          locs$longitude <- locs$lon
          locs$lon <- NULL
          locs$animalId <- rep(x=anId, times=nrow(locs))
          sMapData <- rbind(sMapData, locs)
        }
      }
    }
  }
  sMapData <- sMapData[order(sMapData$animalId, sMapData$timestamp),]
  
  #Adding colour column to sMapData
  sMapData$colour <- apply(sMapData, 1, function(x) {
    listUniq <- unique(sMapData$animalId)
    n <- match(x["animalId"], listUniq)
    return(colourNames[[n]])
  })
  
  # From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
  ui <- fluidPage(
    leafletOutput("mymap")
  )
  
  server <- function(input, output, session) {
    points <- reactive ({
      sMapData
    })
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>%
        addTiles() %>%
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=as.character(points()$a), color=points()$colour)
    })
  }
  runApp(list(ui=ui, server=server))
}

# Show all points (detections) of animals with id defined in 'id'
showMapPointsId <- function(sMapData, id, colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  dataS <- sMapData[sMapData$animalId==id,]
  dataS <- dataS[order(dataS$timestamp),]
  
  #Adding colour column to dataS
  dataS$colour <- apply(dataS, 1, function(x) {
    listUniq <- unique(sMapData$animalId)
    n <- match(id, listUniq)
    return(colourNames[[n]])
  })
  
  # From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
  ui <- fluidPage(
    leafletOutput("mymap")
  )
  
  server <- function(input, output, session) {
    points <- reactive ({
      dataS
    })
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>%
        addTiles() %>%
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=paste0(as.character(points()$a), ", ", as.character(points()$timestamp)), color=points()$colour)
    })
  }
  runApp(list(ui=ui, server=server))
}

# Show points (detections) of all animals
showMapPoints <- function(sMapData, colourNames=c("red", "blue", "green"), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  sMapData <- sMapData[order(sMapData$animalId, sMapData$timestamp),]
  
  #Adding colour column to sMapData
  sMapData$colour <- apply(sMapData, 1, function(x) {
    listUniq <- unique(sMapData$animalId)
    n <- match(x["animalId"], listUniq)
    return(colourNames[[n]])
  })
  
  # From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
  ui <- fluidPage(
    leafletOutput("mymap")
  )
  
  server <- function(input, output, session) {
    points <- reactive ({
      sMapData #%>%
      #filter(dataS$timestamp==input$time)
    })
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>%
        addTiles() %>%
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=paste0(as.character(points()$a), ", ", as.character(points()$timestamp)), color=points()$colour)
    })
  }
  runApp(list(ui=ui, server=server))
}

# Show map following animals with id defined in 'id'
showIdMapFollow <- function(sMapData, id, colourNames=palette(rainbow(3)), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  # Get location at every second
  mdSplit <- split(sMapData, sMapData$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    anId <- mdI$animalId[[1]]
    for (t in 1:(length(mdI$time)-1)) {
      if(length(mdI$time)>=2 && !is.na(mdI$time[[t]]) && !is.na(mdI$time[[t+1]]) && !is.na(mdI$latitude[[t]]) && !is.na(mdI$longitude[[t]]) && !is.na(mdI$latitude[[t+1]]) && !is.na(mdI$longitude[[t+1]])) {
        tS <- seq(mdI$time[[t]], mdI$time[[t+1]], by="sec")
        if(length(tS) > 1) {
          locs <- movePath(mdI$longitude[[t]], mdI$latitude[[t]], mdI$longitude[[t+1]], mdI$latitude[[t+1]], tS)
          
          # Clean up data to match dataS
          locs$latitude <- locs$lat
          locs$lat <- NULL
          locs$longitude <- locs$lon
          locs$lon <- NULL
          locs$animalId <- rep(x=anId, times=nrow(locs))
          sMapData <- rbind(sMapData, locs)
        }
      }
    }
  }
  dataS <- sMapData[sMapData$animalId==id,]
  dataS <- dataS[order(dataS$timestamp),]
  
  
  #Adding colour column to dataS
  dataS$colour <- apply(dataS, 1, function(x) {
    listUniq <- unique(sMapData$animalId)
    n <- match(id, listUniq)
    return(colourNames[[n]])
  })
  
  # From https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts/38878585
  ui <- fluidPage(
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
    output$mymap <- renderLeaflet({
      leaflet() %>%
        #setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>%
        addTiles() %>%
        addCircleMarkers(lat = points()$latitude, lng=points()$longitude, label=as.character(points()$a), color=points()$colour)
    })
  }
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