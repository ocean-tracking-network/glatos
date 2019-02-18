## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(collapse = T,comment ="", fig.pos = 'H')
is_html_output = function() {
  knitr::opts_knit$get("rmarkdown.pandoc.to") =="html"
}

## ----warning=FALSE,message=FALSE-----------------------------------------
library(glatos)
library(dplyr)
library(sp)

## ---- message=FALSE------------------------------------------------------
det_file <- system.file("extdata", "walleye_detections.csv",
                        package = "glatos")

detections <- read_glatos_detections(det_file)

## ------------------------------------------------------------------------
det_events <- glatos::detection_events(detections, time_sep = "3600")

## ------------------------------------------------------------------------
head(det_events)


## ------------------------------------------------------------------------
ri <- glatos::residence_index(det_events, calculation_method = 'kessel')

## ------------------------------------------------------------------------
head(ri)

## ------------------------------------------------------------------------
boxplot(residency_index~location, data = ri, horizontal = TRUE, las = 1, 
        xlab= 'Residence Index (method = "kessel")')

## ---- fig.width=5, fig.height=5------------------------------------------
#get example great lakes polygon
data(greatLakesPoly)

#plot; note indexing sp for plot.sp
sp::plot(greatLakesPoly, asp = 1, 
           xlim = range(ri$mean_longitude),
           ylim = range(ri$mean_latitude),  
           col = "white", bg = "grey80")

#sort by decreasing residency_index so that large bubbles don't hide smaller
ri <- ri[order(ri$residency_index, decreasing = TRUE),]

#make column for symbol color
ri$color <- "red"
ri$color[ri$residency_index == 0] <- "pink" #sites with no detects

points(mean_latitude ~ mean_longitude, data = ri[ri$animal_id == "153",], 
  pch = 21, cex = 1 + 30*residency_index, bg = color)


## ---- fig.width=5, fig.height=5------------------------------------------
#plot; note indexing sp for plot.sp
sp::plot(greatLakesPoly, asp = 1, 
           xlim = range(ri$mean_longitude),
           ylim = range(ri$mean_latitude),  
           col = "white", bg = "grey80")

points(mean_latitude ~ mean_longitude, data = ri[ri$animal_id == "22",], 
  pch = 21, cex = 1 + 30*residency_index, bg = color)

## ---- fig.width=5, fig.height=5------------------------------------------
#plot; note indexing sp for plot.sp
sp::plot(greatLakesPoly, asp = 1, 
           xlim = range(ri$mean_longitude),
           ylim = range(ri$mean_latitude),  
           col = "white", bg = "grey80")

points(mean_latitude ~ mean_longitude, data = ri[ri$animal_id == "23",], 
  pch = 21, cex = 1 + 30*residency_index, bg = color)


## ------------------------------------------------------------------------

#calculate mean and sd of RI among fish
rik_summary <- ri %>% 
               group_by(location, mean_latitude, mean_longitude) %>% 
               summarise(
                    ri_mean = mean(residency_index),
                    ri_sd = sd(residency_index))

head(rik_summary)


## ---- fig.width=5, fig.height=5------------------------------------------
#plot; note indexing sp for plot.sp
sp::plot(greatLakesPoly, asp = 1, 
           xlim = range(ri$mean_longitude),
           ylim = range(ri$mean_latitude),  
           col = "white", bg = "grey80")

#sort by decreasing residency_index so that large bubbles don't hide smaller
rik_summary <- rik_summary[order(rik_summary$ri_mean, decreasing = TRUE),]

#make column for symbol color
rik_summary$color <- "red"
rik_summary$color[rik_summary$ri_mean == 0] <- "pink" #sites with no detects

points(mean_latitude ~ mean_longitude, data = rik_summary, 
  pch = 21, cex = 1 + 30*ri_mean, bg = color)


## ------------------------------------------------------------------------
#get sample receiver data from pkg
loc_file <- system.file("extdata", "sample_receivers.csv", 
                        package = "glatos")
locs <- read_glatos_receivers(loc_file)
locs <- unique(locs[ , c("glatos_array", "deploy_lat", "deploy_long")])
names(locs) <- c("location", "mean_latitude", "mean_longitude")

rik2 <- residence_index(det_events, locations = locs,
                         calculation_method = 'kessel')


## ---- fig.width=5, fig.height=5------------------------------------------
#calculate mean and sd of RI among fish
rik2_summary <- rik2 %>% 
               group_by(location, mean_latitude, mean_longitude) %>% 
               summarise(
                    ri_mean = mean(residency_index),
                    ri_sd = sd(residency_index))

#sort by decreasing residency_index so that large bubbles don't hide smaller
rik2_summary <- rik2_summary[order(rik2_summary$ri_mean, decreasing = TRUE),]

#plot; note indexing sp for plot.sp
sp::plot(greatLakesPoly, asp = 1, 
           xlim = range(rik2$mean_longitude),
           ylim = range(rik2$mean_latitude),  
           col = "white", bg = "grey80")

#make column for symbol color
rik2_summary$color <- "red"
rik2_summary$color[rik2_summary$ri_mean == 0] <- "pink" #sites with no detects

points(mean_latitude ~ mean_longitude, data = rik2_summary, 
  pch = 21, cex = 1 + 30*ri_mean, bg = color)


## ------------------------------------------------------------------------
rik3 <- residence_index(det_events, locations = locs,
                        calculation_method = 'kessel',
                        groupwise_total = TRUE  )


## ---- fig.width=5, fig.height=5------------------------------------------
#calculate mean and sd of RI among fish
rik3_summary <- rik3 %>% 
               group_by(location, mean_latitude, mean_longitude) %>% 
               summarise(
                    ri_mean = mean(residency_index),
                    ri_sd = sd(residency_index))

#sort by decreasing residency_index so that large bubbles don't hide smaller
rik3_summary <- rik3_summary[order(rik3_summary$ri_mean, decreasing = TRUE),]

#plot; note indexing sp for plot.sp
sp::plot(greatLakesPoly, asp = 1, 
           xlim = range(rik3$mean_longitude),
           ylim = range(rik3$mean_latitude),  
           col = "white", bg = "grey80")

#make column for symbol color
rik3_summary$color <- "red"
rik3_summary$color[rik3_summary$ri_mean == 0] <- "pink" #sites with no detects

points(mean_latitude ~ mean_longitude, data = rik3_summary, 
  pch = 21, cex = 1 + 30*ri_mean, bg = color)


## ------------------------------------------------------------------------
riti <- glatos::residence_index(det_events, locations = locs, 
                                calculation_method = "time_interval")

## ------------------------------------------------------------------------
head(riti)

## ---- fig.width=5, fig.height=5------------------------------------------
#calculate mean and sd of RI among fish
riti_summary <- riti %>% 
               group_by(location, mean_latitude, mean_longitude) %>% 
               summarise(
                    ri_mean = mean(residency_index),
                    ri_sd = sd(residency_index))

#sort by decreasing residency_index so that large bubbles don't hide smaller
riti_summary <- riti_summary[order(riti_summary$ri_mean, decreasing = TRUE),]

#plot; note indexing sp for plot.sp
sp::plot(greatLakesPoly, asp = 1, 
           xlim = range(riti$mean_longitude),
           ylim = range(riti$mean_latitude),  
           col = "white", bg = "grey80")

#make column for symbol color
riti_summary$color <- "red"
riti_summary$color[riti_summary$ri_mean == 0] <- "pink" #sites with no detects

points(mean_latitude ~ mean_longitude, data = riti_summary, 
  pch = 21, cex = 1 + 30*ri_mean, bg = color)


## ---- fig.width=5, fig.height=5------------------------------------------
riti <- glatos::residence_index(det_events, locations = locs, 
                                calculation_method = "time_interval",
                                time_interval_size = "15 mins")

#calculate mean and sd of RI among fish
riti_summary <- riti %>% 
               group_by(location, mean_latitude, mean_longitude) %>% 
               summarise(
                    ri_mean = mean(residency_index),
                    ri_sd = sd(residency_index))

#sort by decreasing residency_index so that large bubbles don't hide smaller
riti_summary <- riti_summary[order(riti_summary$ri_mean, decreasing = TRUE),]

#plot; note indexing sp for plot.sp
sp::plot(greatLakesPoly, asp = 1, 
           xlim = range(riti$mean_longitude),
           ylim = range(riti$mean_latitude),  
           col = "white", bg = "grey80")

#make column for symbol color
riti_summary$color <- "red"
riti_summary$color[riti_summary$ri_mean == 0] <- "pink" #sites with no detects

points(mean_latitude ~ mean_longitude, data = riti_summary, 
  pch = 21, cex = 1 + 30*ri_mean, bg = color)


## ---- fig.width=5, fig.height=5------------------------------------------
riti <- glatos::residence_index(det_events, locations = locs, 
                                calculation_method = "time_interval",
                                time_interval_size = "1 month")

#calculate mean and sd of RI among fish
riti_summary <- riti %>% 
               group_by(location, mean_latitude, mean_longitude) %>% 
               summarise(
                    ri_mean = mean(residency_index),
                    ri_sd = sd(residency_index))

#sort by decreasing residency_index so that large bubbles don't hide smaller
riti_summary <- riti_summary[order(riti_summary$ri_mean, decreasing = TRUE),]

#plot; note indexing sp for plot.sp
sp::plot(greatLakesPoly, asp = 1, 
           xlim = range(riti$mean_longitude),
           ylim = range(riti$mean_latitude),  
           col = "white", bg = "grey80")

#make column for symbol color
riti_summary$color <- "red"
riti_summary$color[riti_summary$ri_mean == 0] <- "pink" #sites with no detects

points(mean_latitude ~ mean_longitude, data = riti_summary, 
  pch = 21, cex = 1 + 30*ri_mean, bg = color)


## ---- fig.width=5, fig.height=5------------------------------------------
ritd <- glatos::residence_index(det_events, locations = locs, 
                                calculation_method = "timedelta")

#calculate mean and sd of RI among fish
ritd_summary <- ritd %>% 
               group_by(location, mean_latitude, mean_longitude) %>% 
               summarise(
                    ri_mean = mean(residency_index),
                    ri_sd = sd(residency_index))

#sort by decreasing residency_index so that large bubbles don't hide smaller
ritd_summary <- ritd_summary[order(ritd_summary$ri_mean, decreasing = TRUE),]

#plot; note indexing sp for plot.sp
sp::plot(greatLakesPoly, asp = 1, 
           xlim = range(ritd$mean_longitude),
           ylim = range(ritd$mean_latitude),  
           col = "white", bg = "grey80")

#make column for symbol color
ritd_summary$color <- "red"
ritd_summary$color[ritd_summary$ri_mean == 0] <- "pink" #sites with no detects

points(mean_latitude ~ mean_longitude, data = ritd_summary, 
  pch = 21, cex = 1 + 30*ri_mean, bg = color)

## ---- fig.width=5, fig.height=5------------------------------------------
riawo <- glatos::residence_index(det_events, locations = locs, 
                                calculation_method = "aggregate_with_overlap",
                                group_col = NA)

#calculate mean and sd of RI among fish
riawo_summary <- riawo %>% 
               group_by(location, mean_latitude, mean_longitude) %>% 
               summarise(
                    ri_mean = mean(residency_index),
                    ri_sd = sd(residency_index))

#sort by decreasing residency_index so that large bubbles don't hide smaller
riawo_summary <- riawo_summary[order(riawo_summary$ri_mean, decreasing = TRUE),]

#plot; note indexing sp for plot.sp
sp::plot(greatLakesPoly, asp = 1, 
           xlim = range(riawo$mean_longitude),
           ylim = range(riawo$mean_latitude),  
           col = "white", bg = "grey80")

#make column for symbol color
riawo_summary$color <- "red"
riawo_summary$color[riawo_summary$ri_mean == 0] <- "pink" #sites with no detects

points(mean_latitude ~ mean_longitude, data = riawo_summary, 
  pch = 21, cex = 1 + 30*ri_mean, bg = color)

## ---- fig.width=5, fig.height=5------------------------------------------
riano <- glatos::residence_index(det_events, locations = locs, 
                                calculation_method = "aggregate_with_overlap",
                                group_col = NA)

#calculate mean and sd of RI among fish
riano_summary <- riano %>% 
               group_by(location, mean_latitude, mean_longitude) %>% 
               summarise(
                    ri_mean = mean(residency_index),
                    ri_sd = sd(residency_index))

#sort by decreasing residency_index so that large bubbles don't hide smaller
riano_summary <- riano_summary[order(riano_summary$ri_mean, decreasing = TRUE),]

#plot; note indexing sp for plot.sp
sp::plot(greatLakesPoly, asp = 1, 
           xlim = range(riano$mean_longitude),
           ylim = range(riano$mean_latitude),  
           col = "white", bg = "grey80")

#make column for symbol color
riano_summary$color <- "red"
riano_summary$color[riano_summary$ri_mean == 0] <- "pink" #sites with no detects

points(mean_latitude ~ mean_longitude, data = riano_summary, 
  pch = 21, cex = 1 + 30*ri_mean, bg = color)

## ----warning=FALSE,message=FALSE-----------------------------------------
library(plotly)

## ----warning=FALSE,message=FALSE, echo=FALSE-----------------------------
Sys.setenv('MAPBOX_TOKEN' = 'Your Token Here')

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

