## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(collapse = T,comment ="", fig.pos = 'H')
is_html_output = function() {
  knitr::opts_knit$get("rmarkdown.pandoc.to") =="html"
}

## ----warning=FALSE,message=FALSE-----------------------------------------
library(dplyr)
library(glatos)
library(plotly)

## ------------------------------------------------------------------------
detection_file <- system.file("extdata", "walleye_detections.csv", package = "glatos")
receiver_file <- system.file("extdata", "sample_receivers.csv", package = "glatos")

receivers <- read_glatos_receivers(receiver_file)
detections <- read_glatos_detections(detection_file)

## ------------------------------------------------------------------------
receivers <- receivers %>% 
  mutate( recover_date_time = replace(recover_date_time,
                                      is.na(recover_date_time), 
                                      Sys.time()))

## ------------------------------------------------------------------------
rei <- glatos::REI(detections,receivers)

## ----warning=FALSE,message=FALSE-----------------------------------------
geo <- list(
  scope = 'north america',
  showland = TRUE,
  landcolor = toRGB("white"),
  showocean = TRUE,
  oceancolor = toRGB("gray"),
  showcountreies = TRUE,
  showlakes = TRUE,
  lakecolor = plotly::toRGB("gray"),
  
  resolution = 50,
  center = list(lat = median(rei$latitude),
                lon = median(rei$longitude)),
  lonaxis = list(range=c(min(rei$longitude)-1, max(rei$longitude)+1)),
  lataxis = list(range=c(min(rei$latitude)-1, max(rei$latitude)+1))
)


map <- rei %>% 
  plot_geo(lat = ~latitude, lon = ~longitude, color = ~rei,width=900 )%>%
  add_markers(
    text = ~paste(station, ': ', rei),
    hoverinfo = "text",
    size = ~c(rei * 5 +5)
  )%>%
  layout(title = "REI", geo = geo)


## ---- eval=is_html_output(), warning=FALSE, message=FALSE----------------
#  map

## ----warning=FALSE,message=FALSE, echo=FALSE-----------------------------
Sys.setenv('MAPBOX_TOKEN' = 'Your Token Here')

## ---- eval=!is_html_output() ,echo=FALSE, message=FALSE,warning=FALSE, out.width = "400px"----
orca(map, file = 'rei_map.png')
knitr::include_graphics('rei_map.png')

