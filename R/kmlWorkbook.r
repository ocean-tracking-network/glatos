#' @export
#input arguments
#-zipFile is the full path to the glatos workbook zip archive (submitted to GLATOSWeb)
kmlWorkbook <- function(zipFile,browse=F,kmz=F,labelSize=0.6,iconSize=0.6,
	showOngoing=T,endDate="2020-01-01") {

  tempDir <- gsub(".xlsm.zip|.zip","_tempR",zipFile)
  unzip(zipFile,exdir=tempDir)

  glatosFiles <- list.files(tempDir, full.names=T)

  #Receiver stations
	  #import deployment and recovery data
	  dpl <- read.csv(glatosFiles[grep("_GLATOS_Deployment.csv",glatosFiles)],as.is=T)
	  rcv <- read.csv(glatosFiles[grep("_GLATOS_Recovery.csv",glatosFiles)],as.is=T)
	  loc <- read.csv(glatosFiles[grep("_GLATOS_Locations.csv",glatosFiles)],as.is=T)

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
  kmlName <- gsub("\\.xlsm\\.zip|\\.zip", ".kml", basename(zipFile))
  


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

  kmlFullName <- paste0(dirname(zipFile),'/',kmlName)
  write.table(kmlOut,kmlFullName,col.names=FALSE,row.names=FALSE,quote=FALSE)
  if(kmz) zip(gsub(".kml",".kmz",kmlFullName),files=kmlFullName)
}

