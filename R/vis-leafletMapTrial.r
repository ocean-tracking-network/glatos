leafletMapTrial <- function(){
install.packages("leaflet")
install.packages("geojsonio")
install.packages("colorRamps")
#install.packages("mapview")
library(leaflet)
#library(mapview)
#webshot::install_phantomjs() #need for mapview
library(htmlwidgets)
library(htmltools)
library(geojsonio)

#For colour generation
library(colorRamps)
# from https://rstudio.github.io/leaflet/
# from http://bl.ocks.org/timelyportfolio/6119de4db34da4065832396c4bb9ea93

# Practice map
m <- leaflet()
m <- addTiles(m)
m <- addMarkers(m, lng=-63.588545, lat=44.638017, popup="Rebecca Cohn")
m <- addMarkers(m, lng=-63.591228, lat=44.637285, popup="Killam Library")
m <- addMarkers(m, lng=-63.568311, lat=44.640170, popup="Westin Hotel")
m <- addMarkers(m, lng=-63.580627, lat=44.647468, popup="Citadel Hill")


# Practice fish data:

# animalId  time  latitude    longitude

anId <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
timeA <- rep(x="2010/10/11 11:11:11", times=3)
timeB <- rep(x="2010/10/12 11:11:11", times=3)
timeC <- rep(x="2010/10/13 11:11:11", times=3)
times <- c(timeA, timeB, timeC)
times <- as.POSIXct(times, tz="UCT")
long <- c(-63.591785, -63.571830, -63.593094, -63.591228, -63.570306, -63.593255, -63.588545, -63.570178, -63.591743)
lat <- c(44.635467, 44.644277, 44.636216, 44.637284, 44.644078, 44.637034, 44.638017, 44.645788, 44.638239)
sMapData <- data.frame(animalId=anId, time=times, longitude=long, latitude=lat)


mgeo <- geojson_json(sMapData, lat="latitude", lon="longitude") #Converting data frame of data to GeoJSON

mapAll <- leaflet()
mapAll <- addTiles(mapAll)
for(i in 1:length(anId)) {
  s <- paste0(sMapData$animalId[[i]], " ", sMapData$time[[i]])
  mapAll <- addMarkers(mapAll, lng=sMapData$longitude[[i]], lat=sMapData$latitude[[i]], popup=s)
}
uniqIds <- unique(sMapData$animalId)
colours <- rgb.tables(length(uniqIds)) #Color array


mapAll$dependencies[[length(mapAll$dependencies) + 1]] <- htmlDependency(
  name="map-timeline",
  version="1.0.0",
  src=c("href" = "http://skeate.github.io/Leaflet.timeline/"),
  script = "javascripts/leaflet.timeline.js",
  stylesheet = "stylesheets/leaflet.timeline.css"
)
mapAll <- onRender(sprintf(
  '
  function(el,x){
    var power_data = %s;

  var timeline = L.timeline(power_data, {
  pointToLayer: function(data, latlng){
  var hue_min = 120;
  var hue_max = 0;
  var hue = hue_min;
  return L.circleMarker(latlng, {
  radius: 10,
  color: "hsl("+hue+", 100%%, 50%%)",
  fillColor: "hsl("+hue+", 100%%, 50%%)"
  });
  },
  steps: 1000,
  duration: 10000,
  showTicks: true
  });
  timeline.addTo(this);
  }
  ',
  mgeo
))




#Maps by 3 times: mapTime1, mapTime2, mapTime3
mdSplit <- split(sMapData, sMapData$time) #Splits data by time
mdSplit1 <- mdSplit[[1]] #First time
mapTime1 <- leaflet()
mapTime1 <- addProviderTiles(mapTime1, "OpenStreetMap.Mapnik")
for(i in 1:length(mdSplit1$animalId)) {
  s <- paste0(mdSplit1$animalId[[i]],"")
  mapTime1 <- addMarkers(mapTime1, lng=mdSplit1$longitude[[i]], lat=mdSplit1$latitude[[i]], popup=s)
}
mdSplit2 <- mdSplit[[2]] #Second time
mapTime2 <- leaflet()
mapTime2 <- addProviderTiles(mapTime2, "OpenStreetMap.Mapnik")
for(i in 1:length(mdSplit2$animalId)) {
  s <- paste0(mdSplit2$animalId[[i]],"")
  mapTime2 <- addMarkers(mapTime2, lng=mdSplit2$longitude[[i]], lat=mdSplit2$latitude[[i]], popup=s)
}
mdSplit3 <- mdSplit[[3]] #Third time
mapTime3 <- leaflet()
mapTime3 <- addProviderTiles(mapTime3, "OpenStreetMap.Mapnik")

for(i in 1:length(mdSplit3$animalId)) {
  s <- paste0(mdSplit3$animalId[[i]],"")
  mapTime3 <- addMarkers(mapTime3, lng=mdSplit3$longitude[[i]], lat=mdSplit3$latitude[[i]], popup=s)
}

saveWidget(mapTime1, file="/Users/dinian/Desktop/glatos/map1.html")
saveWidget(mapTime2, file="/Users/dinian/Desktop/glatos/map2.html")
saveWidget(mapTime3, file="/Users/dinian/Desktop/glatos/map3.html")
}