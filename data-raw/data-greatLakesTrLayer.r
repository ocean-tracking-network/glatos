# Make data object greatLakeTrLayer

# get path to example polygon
data("great_lakes_polygon")

# set resolution to match previous version of this object
greatLakes_tr <- make_transition(
  great_lakes_polygon,
  res = c(0.03261609, 0.01153027)
)

# Check results
x11(width = 12, height = 9)
# note: do not resize rendered window or raster will be distorted

# plot raster
raster::plot(greatLakes_tr$rast)

# compare to input polygon
plot(sf::st_geometry(great_lakes_polygon), add = TRUE)

# plot transition layer
raster::plot(raster::raster(greatLakes_tr$transition))

greatLakesTrLayer <- greatLakes_tr$transition

#----------------------------------------------------

# add to (exported) data
usethis::use_data(greatLakesTrLayer, overwrite = TRUE)
