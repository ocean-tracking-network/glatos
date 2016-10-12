#convert Vemco xml file to csv 
ulfx2csv <- function(ulfx){
	library('XML')

    #if ulfx is single directory, get list of ulfx file names
    if(all(file.info(ulfx)$isdir)) {
  	  if(!file.exists(ulfx)) stop(paste0("File or folder not found: ",ulfx))
	  if(length(ulfx) > 1) stop("input argument 'ulfx' cannot include more than one directory")
	  if(length(ulfx) == 0) stop(paste0("No ULFx files found at ",ulfx))
	  if(length(ulfx) == 1) ulfx <- list.files(ulfx,full.names=T, pattern=".ulfx$|.ULFX$|.ULFx$")
    } #end if

	pb <- txtProgressBar(min=1, max=length(ulfx),width=50)
	
	#loop through ulfx files and convert each to csv files
	for(i in 1:length(ulfx)){

		xmlFile <- ulfx[i]
		xml <- xmlTreeParse(xmlFile, useInternal=T)

		#preallocate detection fields
		detects <- data.frame(
			time=NA, #vemco name: Date and Time (UTC)
			receiver=NA, #vemco name: Receiver
			transmitter=NA, #vemco name: Transmitter
			transmitter_name=NA, #vemco name: Transmitter Name
			transmitter_serial=NA, #vemco name: Transmitter Serial
			sensor_value=NA, #vemco name: Sensor Value
			sensor_units=NA, #vemco name: Sensor Unit
			station_name=NA, #vemco name: Station Name
			latitude=NA, #vemco name: Latitude
			longitude=NA, #vemco name: Longitude
			stringsAsFactors=F, #do not convert character to character
			check.names=F)[0,] #allow special characters per default Vemco format)[0,]		

		#get device definitions
		dev.nodes <- getNodeSet(xml, "//definitions/device")

		devs <- data.frame(
			 id=xpathSApply(xml,"//definitions/device/@id"), 
			 short=xpathSApply(xml,"//definitions/device/display_id/text/@short"), 
			 transmitter=grepl("transmitter",xmlSApply(dev.nodes,names)),
			 receiver=grepl("receiver",xmlSApply(dev.nodes,names)),
			 sensor=grepl("sensor",xmlSApply(dev.nodes,names)),
			 stringsAsFactors=F)

		#Add required detection fields
		det.nodes <- getNodeSet(xml, "//log/records/detection")
		dets <- data.frame(
				src=xpathSApply(xml, "//log/records/detection/@src"),
				time=xpathSApply(xml,"//log/records/detection/@time"),
				lat=grepl("latitude",xmlSApply(det.nodes,names)),
				lon=grepl("longitude",xmlSApply(det.nodes,names))
				)
		dets <- merge(dets,devs,by.x="src",by.y="id",all.x=T) #add device data
		dets$receiver <- devs$short[devs$receiver] #add receiver
		
		detects[1:nrow(dets),] <- '' #preallocate
		
		#append to detects
		detects[,c("time","receiver","transmitter")] <- dets[,c("time","receiver","short")]
		

		#Add optional detection fields
		
		#add station name, latitude and longitude
		detects$latitude[dets$lat] <- as.character(xpathSApply(xml,"//log/records/detection/@latitude"))
		detects$longitude[dets$lon] <- as.character(xpathSApply(xml,"//log/records/detection/@longitude"))
		
		
		#sensor data
		
		if(sum(dets$sensor) > 0){	
			#get data about sensors
			sens <- data.frame(
				idx=xpathSApply(xml,"//definitions/device/sensor/@idx"),
				device=xpathSApply(xml,"//definitions/device/sensor/@device"),
				unit=xpathSApply(xml,"//definitions/device/sensor/@unit"),
				resolution=xpathSApply(xml,"//definitions/device/sensor/@resolution"),
				 stringsAsFactors=F)
			
			sensAttr <- data.frame(
				gid=xpathSApply(xml,"//definitions/unit[display_id/text/@lang]/@gid"),
				dimension=xpathSApply(xml,"//definitions/unit[display_id/text/@lang]/@dimension"),
				lang=xpathSApply(xml,"//definitions/unit/display_id/text/@lang"),
				short=xpathSApply(xml,"//definitions/unit/display_id/text/@short"),
				long=xpathSApply(xml,"//definitions/unit/display_id/text/@long"),
				 stringsAsFactors=F
				)
			
			sens <- merge(sens, sensAttr, by.x="unit",by.y="gid")
		 
			#get detections with sensor data
			sns <- data.frame(
				 device=xpathSApply(xml, "//log/records/detection/sample/@device"), 
				 sensor=xpathSApply(xml,"//log/records/detection/sample/@sensor"), 
				 sensor_value=xpathSApply(xml,"//log/records/detection/sample/@value"),
				 time=xpathSApply(xml, "//log/records/detection[sample/@sensor]/@time"),
				 stringsAsFactors=F)
			
			sns <- merge(sns,sens, by="device")
				
			#add to detects
				detects$sensor_value[match(sns$time,detects$time)] <- sns$sensor_value
				detects$sensor_units[match(sns$time,detects$time)] <- sns$short
		} #end if
		
		#drop T and Z from timestamps
		detects$time <- gsub("T"," ",detects$time)
		detects$time <- gsub("Z","",detects$time)	
		
		#sort by timestamp
		detects <- detects[order(as.POSIXct(detects$time,tz="GMT")),]
		
		#rename columns to be consistent with Vemco default format
		names(detects) <- c("Date and Time (UTC)","Receiver","Transmitter","Transmitter Name",
			"Transmitter Serial","Sensor Value","Sensor Unit","Station Name","Latitude","Longitude")
		
		#write to csv
		write.csv(detects,gsub(".ulfx$|.ULFX$|.ULFx$","_detections.csv",xmlFile),row.names=F,quote=F)
		
		

		
		#Receiver events

		#- memory stats
		   #intermediate values
		   mem.x <- data.frame(
				 # - memory used
				 #end_ptr=as.numeric(xpathSApply(xml,"//log/records/memory_stats/segment/@end_ptr")),
				 end_ptr=as.numeric(xpathSApply(xml,"//log/records/memory_stats[@trigger = 'scheduled' or @trigger = 'recording_pause' or @trigger = 'requested']/segment/@end_ptr")),
				 # - max memory
				 memory_size=as.numeric(xpathSApply(xml,"//log/records/memory_stats[@trigger = 'scheduled' or @trigger = 'recording_pause' or @trigger = 'requested']/segment/@memory_size")),
				 # - class
				 class=xpathSApply(xml,"//log/records/memory_stats[@trigger = 'scheduled' or @trigger = 'recording_pause' or @trigger = 'requested']/segment/@class"))
		   mem.x$percent_used <- with(mem.x, end_ptr/memory_size)
		   
		   mem <- data.frame(
				event_timestamp_utc=xpathSApply(xml,"//log/records/memory_stats[@trigger = 'scheduled' or @trigger = 'recording_pause' or @trigger = 'requested']/@time"),
				receiver=devs$short[devs$receiver],
				description="Memory Capacity",
				data=round(mem.x$percent_used[mem.x$class=="main_log"]*100,1),
				units="%",
				stringsAsFactors=F)
		   mem$data <- format(mem$data,digits=1) #so always exports one decimal point
				
				
		#- battery stats
		   bat <- data.frame(
				event_timestamp_utc=xpathSApply(xml,"//log/records/battery_stats/@time"),
				receiver=devs$short[devs$receiver],
				description="Battery",
				data=xpathSApply(xml,"//log/records/battery_stats/@volts"),
				units="V",
				stringsAsFactors=F)
		   bat$data <- round(as.numeric(bat$data),1)
			  

		#- receiver stats - daily
		   rec.nodes.daily <- getNodeSet(xml, 
			"//log/records/receiver_stats[@trigger='scheduled' or @trigger='recording_pause']")
		   		   
		   daily.pings <- data.frame(
				event_timestamp_utc=sapply(rec.nodes.daily,function(x) xmlGetAttr(x,"time")),
				receiver=devs$short[devs$receiver],
				description="Daily Pings",
				data=sapply(rec.nodes.daily,function(x) xmlGetAttr(x,"pulse_count")),
				units="",
				stringsAsFactors=F)
		   daily.syncs <- data.frame(
				event_timestamp_utc=sapply(rec.nodes.daily,function(x) xmlGetAttr(x,"time")),
				receiver=devs$short[devs$receiver],
				description="Daily Syncs",
				data=sapply(rec.nodes.daily,function(x) xmlGetAttr(x,"sync_count")),
				units="",
				stringsAsFactors=F)
		   daily.rejects <- data.frame(
				event_timestamp_utc=sapply(rec.nodes.daily,function(x) xmlGetAttr(x,"time")),
				receiver=devs$short[devs$receiver],
				description="Daily Rejects",
				data=sapply(rec.nodes.daily,function(x) xmlGetAttr(x,"reject_count")),
				units="",
				stringsAsFactors=F)
				
		#receiver stats - study
		   rec.nodes.study <- getNodeSet(xml, "//log/records/receiver_stats[@trigger='requested']")
			
		   study.pings <- data.frame(
				event_timestamp_utc=sapply(rec.nodes.study,function(x) xmlGetAttr(x,"time")),
				receiver=devs$short[devs$receiver],
				description="Study Pings",
				data=sapply(rec.nodes.study,function(x) xmlGetAttr(x,"pulse_count")),
				units="",
				stringsAsFactors=F)

		#offload
			offload <- data.frame(
				event_timestamp_utc=xpathSApply(xml,"//log/records/data_offload/@time"),
				receiver=devs$short[devs$receiver],
				description="Data Upload",
				data=xpathSApply(xml,"//log/records/data_offload/offload/@original_file"),
				units="",
				stringsAsFactors=F)  
				
		#pc time
			pctime <- data.frame(
				event_timestamp_utc=xpathSApply(xml,"//log/records/data_offload/offload/@time"),
				receiver=devs$short[devs$receiver],
				description="PC Time",
				data=xpathSApply(xml,"//log/records/data_offload/offload/initiator/@time"),
				units="",
				stringsAsFactors=F)
			#reformat to be consistent with VUE default
			pctime$data <- gsub("T"," ",pctime$data)
			pctime$data <- paste0(substr(pctime$data,1,19)," UTC",substr(pctime$data,20,28))
		 
		 
		#daily detections and last decode
			codespaces <- data.frame(
				id=xpathSApply(xml,"//definitions/code_space/@id"),
				short=xpathSApply(xml,"//definitions/code_space/display_id/text/@short"),
				stringsAsFactors=F)        
			codespaces <- codespaces[order(codespaces$short),]
			
			codespace.x <- xpathSApply(xml,"//log/records/decoder_stats/@code_space")
			codespace.x <- codespaces$short[match(as.character(codespace.x), codespaces$id)]
			
			dailydets <- data.frame(
				event_timestamp_utc=xpathSApply(xml,"//log/records/decoder_stats/@time"),
				receiver=devs$short[devs$receiver],
				description=paste0("Daily Detections on ",codespace.x),
				data=xpathSApply(xml,"//log/records/decoder_stats/@decode_count"),
				units="",
				stringsAsFactors=F)              
				
			lastdecode <- data.frame(
				event_timestamp_utc=xpathSApply(xml,"//log/records/decoder_stats/@time"),
				receiver=devs$short[devs$receiver],
				description=paste0("Last Detection on ",codespace.x),
				data=xpathSApply(xml,"//log/records/decoder_stats/@last_decode"),
				units="UTC",
				stringsAsFactors=F) 
			
			lastdecode$data <- gsub("T"," ",lastdecode$data) #replace T with space
			lastdecode$data <- gsub("Z","",lastdecode$data) #drop Z
			
			
		#initialization
			init <- data.frame(
				event_timestamp_utc=xpathSApply(xml,"//log/records/clock_change/@time"),
				receiver=devs$short[devs$receiver],
				description="Initialization",
				data="",
				units="",
				stringsAsFactors=F)  

			init.pctime <- data.frame(
				event_timestamp_utc=xpathSApply(xml,"//log/records/clock_change/@time"),
				receiver=devs$short[devs$receiver],
				description="PC Time",
				data=xpathSApply(xml,"//log/records/clock_change/initiator/@time"),
				units="",
				stringsAsFactors=F) 
			#reformat to be consistent with VUE default
			init.pctime$data <- gsub("T"," ",init.pctime$data)
			init.pctime$data <- paste0(substr(init.pctime$data,1,19)," UTC",substr(init.pctime$data,20,28))
			
			
		#receiver map
		   
			which.map <- xpathSApply(xml,"//log/device_config/receiver_config/@map")
			map.id <- xpathSApply(xml,paste0("//definitions/map[@id=",which.map,"]/display_id/text/@short")) 
			map.codespaceids <- xpathSApply(xml,paste0("//definitions/map[@id=",which.map,"]/decoder/@code_space"))
			map.codespaces <- paste(codespaces$short[match(map.codespaceids,codespaces$id)],collapse="; ") 
			map.codespaces <- paste0(map.id," (",map.codespaces,")")
			
			map <- data.frame(
				event_timestamp_utc=xpathSApply(xml,"//log/device_config/study_config/@init"),
				receiver=devs$short[devs$receiver],
				description="Map",
				data=map.codespaces,
				units="",
				stringsAsFactors=F)  

		#blanking
			map.blanking <- xpathSApply(xml,paste0("//definitions/map[@id=",which.map,"]/@blanking")) 
			
			blanking <- data.frame(
				event_timestamp_utc=xpathSApply(xml,"//log/device_config/study_config/@init"),
				receiver=devs$short[devs$receiver],
				description="Blanking",
				data=as.numeric(map.blanking)*0.001,
				units="ms",
				stringsAsFactors=F) 

		#reset
		resets.list <- xpathSApply(xml,"//log/records/event[@code='8']/display_desc/text/text()")
		resets.vec <- character(length(resets.list))
		for(j in 1:length(resets.list)) resets.vec[j] <- xmlValue(resets.list[[j]]) 
		
		reset <- data.frame(
				event_timestamp_utc=xpathSApply(xml,"//log/records/event[@code='8']/@time"),
				receiver=devs$short[devs$receiver],
				description="Reset",
				data=resets.vec,
				units="",
				stringsAsFactors=F)  
		
		
		#comment
		comments.list <- xpathSApply(xml,"//log/records/event[@code='4']/display_desc/text/text()")
		comments.vec <- character(length(comments.list))
		for(j in 1:length(comments.list)) comments.vec[j] <- xmlValue(comments.list[[j]]) 
		
		comments <- data.frame(
				event_timestamp_utc=xpathSApply(xml,"//log/records/event[@code='4']/@time"),
				receiver=devs$short[devs$receiver],
				description="Comment",
				data=comments.vec,
				units="",
				stringsAsFactors=F)  		

		#last detection
		
						
		
			
		#append all parts and export

		logOut <- rbind(
			bat,
			mem,
			daily.pings,
			daily.syncs,
			daily.rejects,
			offload,
			pctime,
			dailydets,
			lastdecode,
			init,
			init.pctime,
			map,
			blanking,
			study.pings,
			reset,
			comments)
			
		logOut <- as.data.frame(logOut, check.names=F, stringsAsFactors=F)
			
		logOut$event_timestamp_utc <- gsub("T"," ",logOut$event_timestamp_utc) #replace T with space
		logOut$event_timestamp_utc <- gsub("Z","",logOut$event_timestamp_utc) #drop Z
		 

		#enforce same order as VUE export
		event.order <- c(   "Initialization",
							"Data Upload",
							"Memory Capacity",
							"Battery",
							"Daily Pings",
							"Daily Syncs",
							"Daily Rejects",
							c(paste0("Daily Detections on ",codespaces$short),
							paste0("Last Detection on ",codespaces$short))[order(c(codespaces$short,codespaces$short))],
							"PC Time",
							"Map",
							"Blanking",
							"Study Pings",
							"Reset",
							"Comment")
		ord <- match(logOut$description, event.order)           
		logOut <- logOut[order(as.POSIXct(logOut$event_timestamp_utc,tz="GMT"),ord),]   

		#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		#Apply model-specific edits
		
			#VR2W180
			rows_vr2w180 <- grepl("^VR2W180",logOut$receiver) #identify 180 rows
			logOut <- within(logOut[rows_vr2w180,],{
				description <- gsub("^Map$","180 kHz decoding map", description)
				description <- gsub("^Blanking$","180 kHz blanking interval", description) 
				description <- gsub("^Study Pings$","Study Pings on 180 kHz", description)
				description <- gsub("^Daily Pings$","Daily Pings on 180 kHz", description)
				description <- gsub("^Daily Syncs$","Daily Syncs on 180 kHz", description)
				description <- gsub("^Daily Rejects$","Daily Rejects on 180 kHz", description)
			})			
				
			
			
		
		
		#-------------------------------------		
		
		#rename columns to Vemco default
		names(logOut) <- c("Date/Time","Receiver","Description","Data","Units")

		write.csv(logOut,gsub(".ulfx$|.ULFX$|.ULFx$","_receiverEvents.csv",xmlFile),row.names=F,quote=F)
		
		setTxtProgressBar(pb, i) #update progress bar
	} #end i
}

