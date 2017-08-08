library(shiny)
library(xts)
library(dplyr)
library(leaflet)

showMap <- function(sMapData, iconFiles) {
  # Get location at every second
  dataS <- sMapData
  mdSplit <- split(sMapData, sMapData$animalId) #Split data by id
  for(i in 1: length(mdSplit)) {
    mdI <- mdSplit[[i]]
    anId <- mdI$animalId[[1]]
    for (t in 1:(length(mdI$time)-1)) {
      if(!is.na(mdI$time[[t]]) && !is.na(mdI$time[[t+1]]) && !is.na(mdI$latitude[[t]]) && !is.na(mdI$longitude[[t]]) && !is.na(mdI$latitude[[t+1]]) && !is.na(mdI$longitude[[t+1]])) {
        tS <- seq(mdI$time[[t]], mdI$time[[t+1]], by="sec")
        if(length(tS) == 0 || length(tS) == 1) {
          print("ts is too small")
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
  
  server <- function(input, output, session) {
    points <- reactive ({
      sMapData %>%
        filter(sMapData$timestamp==input$time)
    })
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addMarkers(data = points(), label=as.character(points()$a), icon=makeIcon(points()$icon, iconWidth = 39, iconHeight=24))
      
    })
  }
  shinyApp(ui, server)
}

anId <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
timeA <- rep(x="2010/10/11 11:11:11", times=3)
timeB <- rep(x="2010/10/11 11:12:11", times=3)
timeC <- rep(x="2010/10/11 11:13:11", times=3)
times <- c(timeA, timeB, timeC)
times <- as.POSIXct(times, tz="UCT")
long <- c(-63.575993, -63.580284, -63.626032, -63.604145, -63.591700, -63.575306, -63.612943, -63.618736, -63.568354)
lat <- c(44.652902, 44.631772, 44.665845, 44.655283, 44.636597, 44.642827, 44.642665, 44.649300, 44.640201)
smData <- data.frame(animalId=anId, timestamp=times,longitude=long, latitude=lat)
iF <- c("/Users/dinian/Desktop/glatos-git/visualization/Icons/redFish.png", "/Users/dinian/Desktop/glatos-git/visualization/Icons/blueFish.png", "/Users/dinian/Desktop/glatos-git/visualization/Icons/greenFish.png")

showMap (smData, iF)
