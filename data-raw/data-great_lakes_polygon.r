#Make data object great_lakes_polygon

#get path to example detections file
data("greatLakesPoly")

#coerce to sf
great_lakes_polygon <- sf::st_as_sf(greatLakesPoly)

#set CRS
sf::st_crs(great_lakes_polygon) <- 4326

#----------------------------------------------------

#add to (exported) data
usethis::use_data(great_lakes_polygon, overwrite = TRUE)
