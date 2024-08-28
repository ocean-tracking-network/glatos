# Make data object great_lakes_polygon

# Read polygon from shapefile
 
poly_file <- system.file("extdata", "shoreline.zip", package = "glatos")

poly <- sf::st_read(paste0("/vsizip/", poly_file))

# Combine into a single feature
#  this reduces processing time for some functions (e.g., st_distance)
great_lakes_polygon <- sf::st_union(poly)

# Note that this is invalid
sf::st_is_valid(great_lakes_polygon)

great_lakes_polygon <- sf::st_make_valid(great_lakes_polygon)

sf::st_is_valid(great_lakes_polygon)


# Reinforce crs; epsg 4326
#  (existing crs is slightly different)
sf::st_crs(great_lakes_polygon) <- 4326


# Make an sf object (for consistency with early version)
great_lakes_polygon <- sf::st_sf(id = 1, 
                                 geometry = great_lakes_polygon,
                                 agr = "constant")

#----------------------------------------------------

# add to (exported) data
usethis::use_data(great_lakes_polygon, overwrite = TRUE)
