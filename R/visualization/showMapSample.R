library(shiny)
library(xts)
library(dplyr)
library(leaflet)

#http://www.r-graph-gallery.com/2017/03/14/4-tricks-for-working-with-r-leaflet-and-shiny/

showMapIcon <- function(sMapData, 
                        iconFiles=c("/Users/dinian/Desktop/glatos-git/R/visualization/Icons/redFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/blueFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/greenFish.png"), 
                        meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
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
        #Original: addMarkers(data = points(), label = as.character(points()$a), icon = makeIcon(points()$icon, iconWidth = 39, iconHeight = 24))
        #addMarkers(lat = points()$latitude, lng = points()$longitude, label = as.character(points()$anId), icon = makeIcon(points()$icon, iconWidth = 39, iconHeight = 24))
        addMarkers(lat = points()$latitude, lng=points()$longitude, label=as.character(points()$a), icon=makeIcon(points()$icon, iconWidth = 39, iconHeight=24))
    })
  }
  #shinyApp(ui, server)
  runApp(list(ui=ui, server=server))
}

#Show map with coloured circle markers (Alice = red, Bob = blue, Eve = green)
showMapCircle <- function(sMapData, colourNames=c("red", "blue", "green"), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
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
showIdMap <- function(sMapData, id, colourNames=c("red", "blue", "green"), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  # Get location at every second
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
showMapPathId <- function(sMapData, id, colourNames=c("red", "blue", "green"), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  # Get location at every second
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
showMapPaths <- function(sMapData, colourNames=c("red", "blue", "green"), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  # Get location at every second
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
showMapPointsId <- function(sMapData, id, colourNames=c("red", "blue", "green"), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
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
showIdMapFollow <- function(sMapData, id, colourNames=c("red", "blue", "green"), meanLongitude=-63.5904, meanLatitude=44.6474, zoom=13) {
  # Get location at every second
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
showMapIcon(smData, iF)
# OR (using default iconFiles)
showMapIcon(smData)

# Show full animated map with all species with coloured circles
showMapCircle(smData, cF)

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