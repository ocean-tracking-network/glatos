install.packages("leaflet")
install.packages("geojsonio")
install.packages("colorRamps")
install.packages("shiny")
install.packages("xts")
#install.packages("mapview")
library(leaflet)
#library(mapview)
#webshot::install_phantomjs() #need for mapview
library(htmlwidgets)
library(htmltools)
library(geojsonio)

#For colour generation
library(colorRamps)

#For animating
library(shiny)
library(xts)
#library(leaflet)
#library(dplyr)

# from https://rstudio.github.io/leaflet/
# from http://bl.ocks.org/timelyportfolio/6119de4db34da4065832396c4bb9ea93

# Practice map
# m <- leaflet()
# m <- addTiles(m)
# m <- addMarkers(m, lng=-63.588545, lat=44.638017, popup="Rebecca Cohn")
# m <- addMarkers(m, lng=-63.591228, lat=44.637285, popup="Killam Library")
# m <- addMarkers(m, lng=-63.568311, lat=44.640170, popup="Westin Hotel")
# m <- addMarkers(m, lng=-63.580627, lat=44.647468, popup="Citadel Hill")


# Practice fish data:

# animalId  time  latitude    longitude

anId <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
timeA <- rep(x="2010/10/11 11:11:11", times=3)
timeB <- rep(x="2010/10/11 11:12:11", times=3)
timeC <- rep(x="2010/10/11 11:13:11", times=3)
times <- c(timeA, timeB, timeC)
times <- as.POSIXct(times, tz="UCT")
long <- c(-63.591785, -63.571830, -63.593094, -63.591228, -63.570306, -63.593255, -63.588545, -63.570178, -63.591743)
lat <- c(44.635467, 44.644277, 44.636216, 44.637284, 44.644078, 44.637034, 44.638017, 44.645788, 44.638239)
sMapData3 <- data.frame(animalId=anId, timestamp=times,longitude=long, latitude=lat)

anId <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
timeA <- rep(x="2010/10/11 11:11:11", times=3)
timeB <- rep(x="2010/10/11 11:12:11", times=3)
timeC <- rep(x="2010/10/11 11:13:11", times=3)
times <- c(timeA, timeB, timeC)
times <- as.POSIXct(times, tz="UCT")
long <- c(-63.575993, -63.580284, -63.626032, -63.604145, -63.591700, -63.575306, -63.612943, -63.618736, -63.568354)
lat <- c(44.652902, 44.631772, 44.665845, 44.655283, 44.636597, 44.642827, 44.642665, 44.649300, 44.640201)
sMapData <- data.frame(animalId=anId, timestamp=times,longitude=long, latitude=lat)


# Getting location at every second
dataS <- sMapData
mdSplit <- split(sMapData, sMapData$animalId) #Splits data by id
for(i in 1: length(mdSplit)) {
  mdI <- mdSplit[[i]]
  anId <- mdI$animalId[[1]]
  for (t in 1:(length(mdI$time)-1)) {
    #print(paste0("t:", t))
    #print(mdI$time[[t]])
    #print(mdI$time[[t+1]])
    #print(paste0(t,": ",mdI$time[[t]], " ", mdI$time[[t+1]], " ", mdI$latitude[[t]], " ", mdI$latitude[[t+1]], " ", mdI$longitude[[t]], " ", mdI$longitude[[t+1]]))
    if(!is.na(mdI$time[[t]]) && !is.na(mdI$time[[t+1]]) && !is.na(mdI$latitude[[t]]) && !is.na(mdI$longitude[[t]]) && !is.na(mdI$latitude[[t+1]]) && !is.na(mdI$longitude[[t+1]])) {
      tS <- seq(mdI$time[[t]], mdI$time[[t+1]], by="sec")
      #print(tS)
      if(length(tS) == 0 || length(tS) == 1) {
        print("ts is too small")
      }
      else {
        locs <- movePath(mdI$longitude[[t]], mdI$latitude[[t]], mdI$longitude[[t+1]], mdI$latitude[[t+1]], tS)
       
        # Cleaning up data to match dataS
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

# mgeo <- geojson_json(sMapData, lat="latitude", lon="longitude") #Converting data frame of data to GeoJSON
# 
# mapAll <- leaflet()
# mapAll <- addTiles(mapAll)
# for(i in 1:length(anId)) {
#   s <- paste0(sMapData$animalId[[i]], " ", sMapData$time[[i]])
#   mapAll <- addMarkers(mapAll, lng=sMapData$longitude[[i]], lat=sMapData$latitude[[i]], popup=s)
# }
# 
# 
# leaf <- leaflet() %>%
#   addTiles()
# 
# # # add leaflet-timeline as a dependency
# # #  to get the js and css
# # leaf$dependencies[[length(leaf$dependencies)+1]] <- htmlDependency(
# #   name = "leaflet-timeline",
# #   version = "1.0.0",
# #   src = c("href" = "http://skeate.github.io/Leaflet.timeline/"),
# #   script = "javascripts/leaflet.timeline.js",
# #   stylesheet = "stylesheets/leaflet.timeline.css"
# # )
# # 
# # # use the new onRender in htmlwidgets to run
# # #  this code once our leaflet map is rendered
# # #  I did not spend time perfecting the leaflet-timeline
# # #  options
# # leaf %>%
# #   setView(44.0665,23.74667,2) %>%
# #   onRender(sprintf(
# #     '
# #     function(el,x){
# #     var power_data = %s;
# #     
# #     var timeline = L.timeline(power_data, {
# #     pointToLayer: function(data, latlng){
# #     var hue_min = 120;
# #     var hue_max = 0;
# #     var hue = hue_min;
# #     return L.circleMarker(latlng, {
# #     radius: 10,
# #     color: "hsl("+hue+", 100%%, 50%%)",
# #     fillColor: "hsl("+hue+", 100%%, 50%%)"
# #     });
# #     },
# #     steps: 1000,
# #     duration: 10000,
# #     showTicks: true
# #     });
# #     timeline.addTo(this);
# #     }
# #     ',
# #     mgeo
# #   ))
# 
# 
# 
# 
# 
# #Maps by 3 times: mapTime1, mapTime2, mapTime3
# mdSplit <- split(sMapData, sMapData$time) #Splits data by time
# mdSplit1 <- mdSplit[[1]] #First time
# mapTime1 <- leaflet()
# mapTime1 <- addProviderTiles(mapTime1, "OpenStreetMap.Mapnik")
# colors3 <- c("red", "blue", "yellow")
# for(i in 1:length(mdSplit1$animalId)) {
#   s <- paste0(mdSplit1$animalId[[i]],"")
#   mapTime1 <- addMarkers(mapTime1, lng=mdSplit1$longitude[[i]], lat=mdSplit1$latitude[[i]], label=s, icon=fishIcons[[i]])
# }
# 
# mdSplit2 <- mdSplit[[2]] #Second time
# mapTime2 <- leaflet()
# mapTime2 <- addProviderTiles(mapTime2, "OpenStreetMap.Mapnik")
# for(i in 1:length(mdSplit2$animalId)) {
#   s <- paste0(mdSplit2$animalId[[i]],"")
#   mapTime2 <- addMarkers(mapTime2, lng=mdSplit2$longitude[[i]], lat=mdSplit2$latitude[[i]], label=s, icon=fishIcons[[i]])
# }
# mdSplit3 <- mdSplit[[3]] #Third time
# mapTime3 <- leaflet()
# mapTime3 <- addProviderTiles(mapTime3, "OpenStreetMap.Mapnik")
# 
# for(i in 1:length(mdSplit3$animalId)) {
#   s <- paste0(mdSplit3$animalId[[i]],"")
#   mapTime3 <- addMarkers(mapTime3, lng=mdSplit3$longitude[[i]], lat=mdSplit3$latitude[[i]], label=s, icon=fishIcons[[i]])
# }

iconFiles <- c("/Users/dinian/Desktop/glatos-git/visualization/Icons/redFish.png", "/Users/dinian/Desktop/glatos-git/visualization/Icons/blueFish.png", "/Users/dinian/Desktop/glatos-git/visualization/Icons/greenFish.png")
sMapData2 <- sMapData
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











saveWidget(mapTime1, file="/Users/dinian/Desktop/glatos/map1.html")
saveWidget(mapTime2, file="/Users/dinian/Desktop/glatos/map2.html")
saveWidget(mapTime3, file="/Users/dinian/Desktop/glatos/map3.html")