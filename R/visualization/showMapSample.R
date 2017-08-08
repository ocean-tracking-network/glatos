library(shiny)
library(xts)
library(dplyr)
library(leaflet)

#http://www.r-graph-gallery.com/2017/03/14/4-tricks-for-working-with-r-leaflet-and-shiny/

showMap <- function(sMapData, iconFiles, meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  # Get location at every second
  dataS <- sMapData
  mdSplit <- split(sMapData, sMapData$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    anId <- mdI$animalId[[1]]
    for (t in 1:(length(mdI$time)-1)) {
      if(length(mdI$time)<2) {
        # lon <- mdI$longitude[[t]]
        # lat <- mdI$latitude[[t]]
        # ti <- mdI$time[[t]]
        # sMapData <- apply(sMapData, 1, function(x) {
        #   if(x["time"] > ti) {
        #     x["longitude"] <- lon
        #     x["latitude"] <- lat
        #   }
        # })
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
          sMapData <- rbind(sMapData, locs)
        }
      }
    }
  }
  sMapData <- sMapData[order(sMapData$animalId, sMapData$timestamp),]
  sMapData$icon <- apply(sMapData, 1, function(x) {
    n <- x["animalId"]
    return(iconFiles[[as.integer(n)]])
  })
  
  #print(sMapData)
  
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
        addMarkers(data = points(), label=as.character(points()$a), icon=makeIcon(points()$icon, iconWidth = 39, iconHeight=24))
    })
  }
  #shinyApp(ui, server)
  runApp(list(ui=ui, server=server))
}

#Data 1 (blue fish, red fish, and green fish all going around halifax, at different points every hour)
anId <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
timeA <- rep(x="2010/10/11 11:11:11", times=3)
timeB <- rep(x="2010/10/11 11:12:11", times=3)
timeC <- rep(x="2010/10/11 11:13:11", times=3)
times <- c(timeA, timeB, timeC)
times <- as.POSIXct(times, tz="America/Halifax")
long <- c(-63.575993, -63.580284, -63.626032, -63.604145, -63.587708, -63.575306, -63.612943, -63.618736, -63.568354)
lat <- c(44.652902, 44.631772, 44.665845, 44.655283, 44.645422, 44.642827, 44.642665, 44.649300, 44.640201)
smData <- data.frame(animalId=anId, timestamp=times,longitude=long, latitude=lat)
iF <- c("/Users/dinian/Desktop/glatos-git/R/visualization/Icons/redFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/blueFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/greenFish.png")


# #Data 2 (blue fish comes in and out but does not have path)
# anId <- c(1, 2, 1)
# times <- c("2010/10/11 11:00:00", "2010/10/11 11:00:15", "2010/10/11 11:00:30")
# times <- as.POSIXct(times, tz="UCT")
# long <- c(-63.626032, -63.618736, -63.580284)
# lat <- c(44.665845, 44.649300, 44.631772)
# smData <- data.frame(animalId=anId, timestamp=times,longitude=long, latitude=lat)
# iF <- c("/Users/dinian/Desktop/glatos-git/R/visualization/Icons/redFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/blueFish.png")

# #Data 3 (blue fish comes in later and leaves later but has path)
# anId <- c(1, 1, 2, 1, 2, 1, 1)
# times <- c("2010/10/11 11:00:00", "2010/10/11 11:00:07", "2010/10/11 11:00:10", "2010/10/11 11:00:15", "2010/10/11 11:00:20", "2010/10/11 11:00:24", "2010/10/11 11:00:30")
# times <- as.POSIXct(times, tz="UCT")
# long <- c(-63.62603, -63.61874, -63.58028, -63.5917, -63.60415, -63.58063, -63.56861)
# lat <- c(44.66584, 44.64930, 44.63177, 44.6366, 44.65528, 44.64747, 44.62328)
# smData <- data.frame(animalId=anId, timestamp=times,longitude=long, latitude=lat)
# iF <- c("/Users/dinian/Desktop/glatos-git/R/visualization/Icons/redFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/blueFish.png")

showMap (smData, iF)

#Code to view zoomed in Fairview
showMap(smData, iF, meanLatitude = 44.66584, meanLongitude = -63.62603, zoom=15)



# Another type of map that could be useful, shows lines of where the fish are
showMap2 <- function(sMapData, iconFiles, meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  # Get location at every second
  dataS <- sMapData
  mdSplit <- split(sMapData, sMapData$animalId) #Split data by id
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
          sMapData <- rbind(sMapData, locs)
        }
      }
    }
  }
  sMapData <- sMapData[order(sMapData$animalId, sMapData$timestamp),]
  sMapData$icon <- apply(sMapData, 1, function(x) {
    n <- x["animalId"]
    return(iconFiles[[as.integer(n)]])
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
  
  # Split data by animal id
  mdSplitId <- split(sMapData, sMapData$animalId)
  redData <- mdSplitId[[1]]
  blueData <- mdSplitId[[2]]
  greenData <- mdSplitId[[3]]
  
  server <- function(input, output, session) {
    points <- reactive ({
      sMapData %>%
        filter(sMapData$timestamp==input$time)
    })
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng=meanLongitude, lat=meanLatitude, zoom=zoom) %>%
        addTiles() %>%
        addCircleMarkers(data=redData, lng=~redData$longitude, lat=~redData$latitude, label=as.character(points()$a), radius=8, color="black", fillColor="red", stroke=TRUE, fillOpacity = 0.8, group="Red")%>%
        addCircleMarkers(data=blueData, lng=~blueData$longitude, lat=~blueData$latitude, label=as.character(points()$a), radius=8, color="black", fillColor="blue", stroke=TRUE, fillOpacity = 0.8, group="Blue")%>%
        addCircleMarkers(data=greenData, lng=~greenData$longitude, lat=~greenData$latitude, label=as.character(points()$a), radius=8, color="black", fillColor="green", stroke=TRUE, fillOpacity = 0.8, group="Green")
      
      #addMarkers(data = points(), label=as.character(points()$a), icon=makeIcon(points()$icon, iconWidth = 39, iconHeight=24))
    })
  }
  #shinyApp(ui, server)
  runApp(list(ui=ui, server=server))
}

#Data 1 (blue fish, red fish, and green fish all going around halifax, at different points every hour)
anId <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
timeA <- rep(x="2010/10/11 11:11:11", times=3)
timeB <- rep(x="2010/10/11 11:12:11", times=3)
timeC <- rep(x="2010/10/11 11:13:11", times=3)
times <- c(timeA, timeB, timeC)
times <- as.POSIXct(times, tz="America/Halifax")
long <- c(-63.575993, -63.580284, -63.626032, -63.604145, -63.587708, -63.575306, -63.612943, -63.618736, -63.568354)
lat <- c(44.652902, 44.631772, 44.665845, 44.655283, 44.645422, 44.642827, 44.642665, 44.649300, 44.640201)
smData <- data.frame(animalId=anId, timestamp=times,longitude=long, latitude=lat)
iF <- c("/Users/dinian/Desktop/glatos-git/R/visualization/Icons/redFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/blueFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/greenFish.png")

showMap2 (smData, iF)

