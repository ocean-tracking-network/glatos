# Fix "CRS object has no comment" warning

# get path to example polygon file
data("greatLakesPoly")

# reset CRS
slot(greatLakesPoly, "proj4string") <- sp::CRS(SRS_string = "EPSG:4326")

#----------------------------------------------------

# add to (exported) data
usethis::use_data(greatLakesPoly, overwrite = TRUE)
