#' Make a KML (or KMZ) file for viewing receiver and animal release 
#'   locations in Google Earth.
#' 
#' Receiver data (deployment location, deployment timestamp, and 
#' recovery timestamp) and tagging data (release location, release timestamp) 
#' are imported from a zipped GLATOS Workbook archive and used to make a KML 
#' (and optionally, KMZ) for viewing receiver deployments  and release 
#' locations in Google Earth.
#'
#' @param wb_file A character vector with the full path and filename of zipped 
#'   GLATOS workbook (this is the \emph{ZIPPED} archive that gets uploaded to 
#'   GLATOSWeb).
#'   
#' @param browse A logical scalar. If TRUE, user is asked to select wb_file 
#'   using windows explorer. Default value is FALSE.
#'   
#' @param kmz A logical scalar; If TRUE, a KMZ file (zipped KML file) will also 
#'   be created. Default value is FALSE.
#'   
#' @param labelSize A numeric scalar with the size of placemark labels 
#'   (only shown when placemark is highlighted by user).
#'   
#' @param iconSize A numeric scalar with the size of placemark icons.
#' 
#' @param showOngoing A logicalscalar that indicates if ongoing stations 
#'   (missing recovery timestamp) should be included in result.
#'   
#' @param endDate End date (e.g. "YYYY-MM-DD") to be used for any ongoing 
#'   stations (if showOngoing == T)
#'
#' @details
#' Receiver locations will be visible between deployment and recovery 
#' timestamps at each location. Release locations will be displayed when the 
#' display window includes the date of release.
#' 
#' @return A KML (and optionally, KMZ) file, written to the directory that 
#'   contains the zipped GLATOS workbook. Nothing is returned to the R console.
#'
#' @author C. Holbrook (cholbrook@usgs.gov) 
#'
#' @examples
#' #TBD
#'
#' @export
kml_workbook <- function(wb_file,browse=F,kmz=F,labelSize=0.6,iconSize=0.6,
	showOngoing=T,endDate="2020-01-01") {

  tempDir <- gsub(".xlsm.zip|.zip","_tempR",wb_file)
  unzip(wb_file,exdir=tempDir)

  glatosFiles <- list.files(tempDir, full.names=T)

  #Receiver stations
	  #import deployment and recovery data
	  dpl <- read.csv(glatosFiles[grep("_GLATOS_Deployment.csv",glatosFiles)],
	    as.is=T)
	  rcv <- read.csv(glatosFiles[grep("_GLATOS_Recovery.csv",glatosFiles)],
	    as.is=T)
	  loc <- read.csv(glatosFiles[grep("_GLATOS_Locations.csv",glatosFiles)],
	    as.is=T)

	  recLoc <- merge(dpl,rcv,
		by.x=c("GLATOS_ARRAY","STATION_NO","CONSECUTIVE_DEPLOY_NO","INS_SERIAL_NO"),
		by.y=c("GLATOS_ARRAY","STATION_NO","CONSECUTIVE_DEPLOY_NO","INS_SERIAL_NUMBER"),
		all.x=T)
	  names(recLoc) <- gsub("\\.x",".dpl",names(recLoc)) #rename deployment fields if in both
	  names(recLoc) <- gsub("\\.y",".rcv",names(recLoc)) #rename recovery fields if in both
	  
	  recLoc <- merge(recLoc, loc, by="GLATOS_ARRAY")
 
	  #check for UTC times
	  if(all(is.na(recLoc$DEPLOY_DATE_TIME))) {
		recLoc$DEPLOY_DATE_TIME <- as.POSIXct(recLoc$GLATOS_DEPLOY_DATE_TIME, tz=paste0("US/",recLoc$GLATOS_TIMEZONE.dpl[1]))
		recLoc$RECOVER_DATE_TIME <- as.POSIXct(recLoc$GLATOS_RECOVER_DATE_TIME, tz=paste0("US/",recLoc$GLATOS_TIMEZONE.rcv[1]))
	  }
	  
	  #remove recovery timestamps if showOngoing=F
	  missingRecov <- is.na(recLoc$RECOVER_DATE_TIME)
	  if(showOngoing == F){
		recLoc <- recLoc[!missingRecov,] #omit receivers that have not been recovered
	  } else { recLoc$RECOVER_DATE_TIME[missingRecov] <- as.POSIXct(endDate,tz="UTC") }

	  recPos <- data.frame(
		Folder = "Receivers",
		Name = with(recLoc, paste0(GLATOS_ARRAY,"-",STATION_NO," (",WATER_BODY,")")),
		TimeSpan_start = paste(gsub(" ","T",recLoc$DEPLOY_DATE_TIME),"-00:00",sep=""),
		TimeSpan_end = paste(gsub(" ","T",recLoc$RECOVER_DATE_TIME),"-00:00",sep=""),
		Longitude = recLoc$DEPLOY_LONG,
		Latitude = recLoc$DEPLOY_LAT,
		stringsAsFactors=F)

	  recPos$Altitude <- 0
	  recPos$Description <- ""
 
 
  #Fish releases
	  #import tagging data
	  tgg <- read.csv(glatosFiles[grep("_GLATOS_Tagging.csv",glatosFiles)],as.is=T)
	  relLoc <- as.data.frame(table(tgg$RELEASE_GROUP))
	  names(relLoc)[1] <- "RELEASE_GROUP"
	  relLoc <- merge(relLoc,unique(tgg[c("RELEASE_GROUP","RELEASE_LOCATION","RELEASE_LATITUDE",
		"RELEASE_LONGITUDE", "GLATOS_RELEASE_DATE_TIME")]), by="RELEASE_GROUP")
 
 	  relPos <- data.frame(
		Folder = "Releases",
		Name = with(relLoc, paste0(RELEASE_LOCATION," (",RELEASE_GROUP,")")),
		TimeSpan_start = paste(gsub(" ","T",relLoc$GLATOS_RELEASE_DATE_TIME),"-00:00",sep=""),
		TimeSpan_end = paste(gsub(" ","T",relLoc$GLATOS_RELEASE_DATE_TIME),"-00:00",sep=""),
		Longitude = relLoc$RELEASE_LONGITUDE,
		Latitude = relLoc$RELEASE_LATITUDE,
		stringsAsFactors=F)

	  relPos$Altitude <- 0
	  relPos$Description <- ""
 
 
  #make KML

  #-kml-specific values
  kmlName <- gsub("\\.xlsm\\.zip|\\.zip", ".kml", basename(wb_file))
  


  kmlHead = c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">',
    ',<Document>',
    	paste('<name>',kmlName,'.kml</name>',sep=''),
    	'<StyleMap id="msn_placemark_circle">',
    		'<Pair>',
    			'<key>normal</key>',
    			'<styleUrl>#sn_placemark_circle</styleUrl>',
    		'</Pair>',
    		'<Pair>',
    			'<key>highlight</key>',
    			'<styleUrl>#sh_placemark_circle_highlight</styleUrl>',
    		'</Pair>',
    	'</StyleMap>',
    	',<Style id="sh_placemark_circle_highlight">',
    		'<IconStyle>',
    			'<color>ff0000ff</color>',
    			'<scale>0.709091</scale>',
    			'<Icon>',
    				'<href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle_highlight.png</href>',
    			'</Icon>',
    		'</IconStyle>',
    		'<LabelStyle>',
    			paste('<scale>',labelSize,'</scale>',sep=""),
    		'</LabelStyle>',
    		'<ListStyle>',
    		'</ListStyle>',
    	'</Style>',
    	'<Style id="sn_placemark_circle">',
    		'<IconStyle>',
    			'<color>ff0000ff</color>',
    			paste('<scale>',iconSize,'</scale>',sep=""),
    			'<Icon>',
    				'<href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png</href>',
    			'</Icon>',
    		'</IconStyle>',
    		'<LabelStyle>',
      			'<scale>0</scale>',
    		'</LabelStyle>',
    		'<ListStyle>',
    		'</ListStyle>',
    	'</Style>',
    	'<StyleMap id="msn_placemark_circle_rel">',
    		'<Pair>',
    			'<key>normal</key>',
    			'<styleUrl>#sn_placemark_circle_rel</styleUrl>',
    		'</Pair>',
    		'<Pair>',
    			'<key>highlight</key>',
    			'<styleUrl>#sh_placemark_circle_highlight_rel</styleUrl>',
    		'</Pair>',
    	'</StyleMap>',
    	',<Style id="sh_placemark_circle_highlight_rel">',
    		'<IconStyle>',
    			'<color>ff00ffff</color>',
    			'<scale>0.709091</scale>',
    			'<Icon>',
    				'<href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle_highlight.png</href>',
    			'</Icon>',
    		'</IconStyle>',
    		'<LabelStyle>',
    			paste('<scale>',labelSize,'</scale>',sep=""),
    		'</LabelStyle>',
    		'<ListStyle>',
    		'</ListStyle>',
    	'</Style>',
    	'<Style id="sn_placemark_circle_rel">',
    		'<IconStyle>',
    			'<color>ff00ffff</color>',
    			paste('<scale>',iconSize,'</scale>',sep=""),
    			'<Icon>',
    				'<href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png</href>',
    			'</Icon>',
    		'</IconStyle>',
    		'<LabelStyle>',
      			'<scale>0</scale>',
    		'</LabelStyle>',
    		'<ListStyle>',
    		'</ListStyle>',
    	'</Style>')

  #two style maps; one for receivers, second for releases
  stylemaps <- c("msn_placemark_circle","msn_placemark_circle_rel")
  
  makeKMLBody = function(myFolderName,stylemap,myPoints)
    {
      folderHead = c(
  	   '<Folder>',
        paste0('<name>',myFolderName,'</name>'))

      for (i in 1:nrow(myPoints)){
          if (i==1) pmBody = vector() #empty vector

          pmBody = c(pmBody,
              '<Placemark>',
          			paste0('<name>',myPoints$Name[i],'</name>'),
          			paste0('<styleUrl>#',stylemap,'</styleUrl>'),
          			'<Point>',
          				with(myPoints, paste('<coordinates>',Longitude[i],',',Latitude[i],',0</coordinates>',sep='')),
          			'</Point>',
          			'<TimeSpan>',
                  paste0("<begin>",myPoints$TimeSpan_start[i],"</begin>"),
                  paste0("<end>",myPoints$TimeSpan_end[i],"</end>"),
                '</TimeSpan>',
              '</Placemark>')
    		}

      folderBody = c(folderHead,pmBody,'</Folder>')
      return(folderBody)
    }

  #identify number of unique folders
  folders = sort(unique(recPos$Folder))

  for(i in 1:length(folders))
    {
      if(i == 1) folderBody = vector()

      folderBody = c(folderBody, makeKMLBody(folders[i],stylemaps[1],
		subset(recPos, Folder == folders[i])))
    }

  #add releases
  folderBody = c(folderBody, makeKMLBody(relPos$Folder[i],stylemaps[2],relPos))
	
  kmlFoot = c('</Document>','</kml>')

  kmlOut = c(kmlHead,folderBody,kmlFoot)

  kmlFullName <- paste0(dirname(wb_file),'/',kmlName)
  write.table(kmlOut,kmlFullName,col.names=FALSE,row.names=FALSE,quote=FALSE)
  if(kmz) zip(gsub(".kml",".kmz",kmlFullName),files=kmlFullName)
}

