#' KML To CSV Conversion
#'
#' Function for extracting features (points, lines, polygons) from kml files and writing them to csv files.
#'
#' @param filePath The pathname for the kml file you wish to convert.
#' @param type A character string indicating the type of feature contained in the kml file. Valid arguments are c("points", "lines", and "polygons"). Cannot accomodate files with multiple feature types, so each kml file must contain only a single type.
#'
#' @details  The function utilizes the readOGR function in the rgdal package to parse the contents of the kml file. The readOGR function requires that the layer name be spefified, which is determined automatically by from the kml file by reading the <name> tag in the first line after the <Folder> tag.
#' @details The function cannot convert multiple feature types (i.e., points, lines, polygons) in the same kml file. When exporting the kml from Google Earth, make sure that only one feature type is present for each kml. The function can handle multiple features as long as they are the same type.
#' @details kmz files are not supported. Make sure exports from Google earth are saved as kml.
#'
#' @return For type = "points": a single csv file for all points
#' @return For type = "lines" or "polygons": a single csv for each feature in the kml file
#'
#' @examples
#' #TBD
#'
#' @export

kml_to_csv <- function (filePath, type) { #filePath = full path to the kml file, type = c('points', 'lines', 'polygons')
	
	# Function requires the package 'rgdal' for the readOGR function, which is used to parse the kml file.
	
	# Determine the layer name from the kml file.
	a <- readLines(filePath)
	layerLine <- which(grepl("<Folder>", a)) + 1
    layer <- strsplit(strsplit(a[layerLine], ">")[[1]][2], "<")[[1]][1]
	
	# Create a new directory to contain the results.
	dir.create(file.path(paste(dirname(filePath), gsub(".kml", "", basename(filePath)), sep="/")), showWarnings = FALSE) 
	
	resultsFolder <- file.path(paste(dirname(filePath), gsub(".kml", "", basename(filePath)), sep="/"))

    # Use rgdal package function to parse the kml into usable information (NOTE: readOGR cannot handle features with different geometries (e.g., lines and polygons), so each kml file processed with this script can contain only one class of feature.	
	kml <- rgdal::readOGR(filePath, layer = layer) 
	
	# Retrieves the names of features in the kml file.
	Labels <- kml@data$Name 
	
	# Write kml features to csv
	if (type == 'points') { # If type = 'points' all points are written to a single csv file.
		coordinates <- as.data.frame(kml@coords)
		coordinates$Labels <- Labels
		coordinates <- coordinates[, c(4, 1, 2, 3)]
		names(coordinates) <- c('Label', 'Longitude', 'Latitude', 'Altitude')
		write.csv(coordinates, file = file.path(paste0(resultsFolder, "/CoordinatesFromKML.csv")), row.names=FALSE)	
	} else if (type == 'polygons') { # Separate csv file written for each polygon			
		for (i in 1:length(Labels)) {
			label <- Labels[i]		
			coordinates <- as.data.frame(kml@polygons[[i]]@Polygons[[1]]@coords)
			names(coordinates) <- c('Longitude', 'Latitude')
			write.csv(coordinates, file = file.path(paste0(resultsFolder, "/", paste0("Polygon_", Labels[i], ".csv"))), row.names=FALSE)
		}	
	} else if (type == 'lines') {	
		for (i in 1:length(Labels)) { # Separate csv written for each line.
			label <- Labels[i]		
			coordinates <- as.data.frame(kml@lines[[i]]@Lines[[1]]@coords)
			names(coordinates) <- c('Longitude', 'Latitude')
			write.csv(coordinates, file = file.path(paste0(paste0(resultsFolder, "/", paste0("Line_", Labels[i], ".csv")))), row.names=FALSE)
        }
    }
    
    return(paste0("Output file(s) are located in a folder named '", gsub(".kml", "", basename(filePath)), "' located in the same directory as the original kml file"))
}