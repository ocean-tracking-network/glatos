#' KML To CSV Conversion
#'
#' Function for extracting features (points, lines, polygons) from kml files and
#' writing them to csv files.
#'
#' @param filePath The pathname for the kml file you wish to convert.
#' 
#' @param type Optional character string indicating the type(s) of feature(s)
#'   to read from the kml file. Valid values are `c("points", "lines", and
#'   "polygons")`.
#'
#' @details kmz files are not supported. Make sure exports from Google earth are
#'   saved as kml. Or extract (unzip) kml from kmz.
#'
#' @return A csv file (same name as input `filePath` but with `csv` 
#'   extension) is written to directory containing input `filePath` 
#'   with five columns \describe{
#'   \item{name}{Feature name}
#'   \item{feature_type}{Feature type}
#'   \item{seq}{Sequential position in feature}
#'   \item{longitude}{Longitude}
#'   \item{latitude}{Latitude}
#'   \item{altitude}{Altitude}
#'   }
#'   
#' @examples
#' 
#' #Get example kml with two polygons
#' kml_file <- system.file("inst/extdata", "example_polygons.kml",
#'                         package = "glatos")
#'                         
#' kml_to_csv(kml_file)
#'
#' @export

kml_to_csv <- function (filePath, 
                        type = c("points", "lines", "polygons")) { 
  
  # Change type to sf-style types
  type[type == "points"] <- "POINT"
  type[type == "lines"] <- "LINE"
  type[type == "polygons"] <- "POLYGON"

  # Unzip if kmz
  fileExt <- tools::file_ext(kml_file)
  
  if(tolower(fileExt) == "kmz") stop("kmz are not supported.")
  
  # Read kml
	kml <- sf::read_sf(filePath, as_tibble = FALSE)

	# Get types of features in the kml file.
	fTypes <- as.character(sf::st_geometry_type(kml))	
	 
	# Subset
	kml <- kml[fTypes %in% type]
	fTypes <- fTypes[fTypes %in% type]
	
	# Get names of features in the kml file.
	fNames <- kml$Name
	
	# Preallocate list object
	fList <- vector("list", length(fNames))
	names(fList) <- fNames
	
	# Write kml features to csv
	for (i in 1:length(fNames)) {

		coords_i <- sf::st_coordinates(kml$geometry[i])

		fList[[i]] <- data.frame(name = fNames[i],
		                       feature_type = fTypes[i],
		                       seq = 1:nrow(coords_i),
		                       longitude = coords_i[, "X"],
		                       latitude = coords_i[, "Y"],
		                       altitude = coords_i[, "Z"],
		                       stringsAsFactors = FALSE)
		
	}	# end i
	
	fDF <- do.call(rbind, fList)
	
	# Write
	out_file <- gsub("kml$", "csv", filePath)
	
	write.csv(fDF, out_file, row.names = FALSE)
    
  return(out_file)
}
