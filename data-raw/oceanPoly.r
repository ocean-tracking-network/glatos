#Make SpatialPolygonsDataFrame from Natural Earth Vector shapefile
#Source: http//www.naturalearthdata.com/download/110m/physical/ne_110m_ocean.zip
#Accessed 2017-09-26

#get path to ocean polygon (at 110 m) from 
zip_shpfile <- system.file("extdata", "ne_110m_ocean.zip", package = "glatos")

#unzip shapefile to temp directory
tmp <- paste0(tempdir(), "/ne_110m_ocean")
unzip(zip_shpfile, exdir = tmp)

ocean_shapefile <- tmp

layer <- rgdal::ogrListLayers(ocean_shapefile)

data.shape<-rgdal::readOGR(dsn = ocean_shapefile, layer =layer)

#SpatialPolygonDataFrame object
oceanPoly <- rgdal::readOGR(ocean_shapefile, layer="ne_110m_ocean") 

#add to package
#devtools::use_data(oceanPoly, pkg = "glatos")
