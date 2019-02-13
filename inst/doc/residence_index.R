## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(collapse = T,comment ="", fig.pos = 'H')
is_html_output = function() {
  knitr::opts_knit$get("rmarkdown.pandoc.to") =="html"
}

## ----warning=FALSE,message=FALSE-----------------------------------------
library(glatos)
library(plotly)

## ----warning=FALSE,message=FALSE, echo=FALSE-----------------------------
Sys.setenv('MAPBOX_TOKEN' = 'Your Token Here')

## ---- message=FALSE------------------------------------------------------
det_file <- system.file("extdata", "walleye_detections.csv",
                        package = "glatos")

detections <- read_glatos_detections(det_file)
detection_events <- glatos::detection_events(detections, location_col = 'station')

## ------------------------------------------------------------------------
ri <- glatos::residence_index(detection_events,calculation_method = 'kessel')

## ----warning=FALSE, message=FALSE,  fig.keep="FALSE"---------------------
geo <- list(
    scope = 'north america',
  showland = TRUE,
  landcolor = toRGB("white"),
  showocean = TRUE,
  oceancolor = toRGB("gray"),
  showcountries = TRUE,
  showlakes = TRUE,
  lakecolor = plotly::toRGB("gray"),
  
  resolution = 50,
  center = list(lat = median(ri$mean_latitude),
                lon = median(ri$mean_longitude)),
  lonaxis = list(range=c(min(ri$mean_longitude)-1, max(ri$mean_longitude)+1)),
  lataxis = list(range=c(min(ri$mean_latitude)-1, max(ri$mean_latitude)+1))
)


map <- ri %>% 
  plot_geo(lat = ~mean_latitude, lon = ~mean_longitude, color = ~residency_index )%>%
  add_markers(
            text = ~paste(location, ': ', residency_index),
            hoverinfo = "text",
    size = ~c(residency_index * 5)
  )%>%
  layout(title = "Kessel RI",geo = geo)

## ---- eval=is_html_output(), warning=FALSE, message=FALSE----------------
#  map

## ---- eval=!is_html_output() ,echo=FALSE, message=FALSE,warning=FALSE, out.width = "400px"----
orca(map, file = 'kessel.png')
knitr::include_graphics('kessel.png')

## ----warning=FALSE-------------------------------------------------------
ri <- glatos::residence_index(detection_events,calculation_method = 'timedelta')

geo <- list(
    scope = 'north america',
  showland = TRUE,
  landcolor = toRGB("white"),
  showocean = TRUE,
  oceancolor = toRGB("gray"),
  showcountries = TRUE,
  showlakes = TRUE,
  lakecolor = plotly::toRGB("gray"),
  
  resolution = 50,
  center = list(lat = median(ri$mean_latitude),
                lon = median(ri$mean_longitude)),
  lonaxis = list(range=c(min(ri$mean_longitude)-1, max(ri$mean_longitude)+1)),
  lataxis = list(range=c(min(ri$mean_latitude)-1, max(ri$mean_latitude)+1))
)


map <- ri %>% 
  plot_geo(lat = ~mean_latitude, lon = ~mean_longitude, color = ~residency_index )%>%
  add_markers(
            text = ~paste(location, ': ', residency_index),
            hoverinfo = "text",
    size = ~c(residency_index * 5)
  )%>%
  layout(title = "Timedelta RI",geo = geo)


## ---- eval=is_html_output(), warning=FALSE, message=FALSE----------------
#  map

## ---- eval=!is_html_output() ,echo=FALSE, message=FALSE,warning=FALSE, out.width = "400px"----
orca(map, file = 'timedelta.png')
knitr::include_graphics('timedelta.png')

## ----warning=FALSE-------------------------------------------------------
ri <- glatos::residence_index(detection_events,calculation_method = 'aggregate_with_overlap')

geo <- list(
    scope = 'north america',
  showland = TRUE,
  landcolor = toRGB("white"),
  showocean = TRUE,
  oceancolor = toRGB("gray"),
  showcountries = TRUE,
  showlakes = TRUE,
  lakecolor = plotly::toRGB("gray"),
  
  resolution = 50,
  center = list(lat = median(ri$mean_latitude),
                lon = median(ri$mean_longitude)),
  lonaxis = list(range=c(min(ri$mean_longitude)-1, max(ri$mean_longitude)+1)),
  lataxis = list(range=c(min(ri$mean_latitude)-1, max(ri$mean_latitude)+1))
)


map <- ri %>% 
  plot_geo(lat = ~mean_latitude, lon = ~mean_longitude, color = ~residency_index )%>%
  add_markers(
            text = ~paste(location, ': ', residency_index),
            hoverinfo = "text",
    size = ~c(residency_index * 5)
  )%>%
  layout(title = "Aggregate With Overlap RI",geo = geo)




## ---- eval=is_html_output(), warning=FALSE, message=FALSE----------------
#  map

## ---- eval=!is_html_output() ,echo=FALSE, message=FALSE,warning=FALSE, out.width = "400px"----
orca(map, file = 'awo.png')
knitr::include_graphics('awo.png')

## ----warning=FALSE-------------------------------------------------------
ri <- glatos::residence_index(detection_events,calculation_method = 'aggregate_no_overlap')

geo <- list(
  scope = 'north america',
  showland = TRUE,
  landcolor = toRGB("white"),
  showocean = TRUE,
  oceancolor = toRGB("gray"),
  showcountries = TRUE,
  showlakes = TRUE,
  lakecolor = plotly::toRGB("gray"),
  
  resolution = 50,
  center = list(lat = median(ri$mean_latitude),
                lon = median(ri$mean_longitude)),
  lonaxis = list(range=c(min(ri$mean_longitude)-1, max(ri$mean_longitude)+1)),
  lataxis = list(range=c(min(ri$mean_latitude)-1, max(ri$mean_latitude)+1))
)


map <- ri %>% 
  plot_geo(lat = ~mean_latitude, lon = ~mean_longitude, color = ~residency_index )%>%
  add_markers(
            text = ~paste(location, ': ', residency_index),
            hoverinfo = "text",
    size = ~c(residency_index * 5)
  )%>%
  layout(title = "Aggregate No Overlap RI",geo = geo)


## ---- eval=is_html_output(), warning=FALSE, message=FALSE----------------
#  map

## ---- eval=!is_html_output() ,echo=FALSE, message=FALSE,warning=FALSE, out.width = "400px"----
orca(map, file = 'ano.png')
knitr::include_graphics('ano.png')

