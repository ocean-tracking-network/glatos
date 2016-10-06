#convert Vemco xml file to csv 
ulfx2csv <- function(ulfxFile){
	library('XML')
	xmlFile <- ulfxFile
	xml <- xmlTreeParse(xmlFile, useInternal=T)


	#get device definitions
	dev.nodes <- getNodeSet(xml, "//definitions/device")

	devs <- data.frame(
		 id=xpathSApply(xml,"//definitions/device/@id"), 
		 short=xpathSApply(xml,"//definitions/device/display_id/text/@short"), 
		 transmitter=grepl("transmitter",xmlSApply(dev.nodes,names)),
		 receiver=grepl("receiver",xmlSApply(dev.nodes,names)),
		 sensor=grepl("sensor",xmlSApply(dev.nodes,names)),
		 stringsAsFactors=F)

	#make detection dataframe
	detects <- data.frame(
			src=xpathSApply(xml, "//log/records/detection/@src"),
			time=xpathSApply(xml,"//log/records/detection/@time"),
			latitude=xpathSApply(xml,"//log/records/detection/@latitude"),
			longitude=xpathSApply(xml,"//log/records/detection/@longitude"),
			stringsAsFactors=F,
			check.names=F) #allow special characters per default Vemco format
	detects <- merge(detects,devs,by.x="src",by.y="id",all.x=T)
	detects$receiver <- devs$short[devs$receiver]
	detects <- detects[,c("time","receiver","short","latitude","longitude")]
	names(detects)[c(3)] <- c("transmitter")
	#drop T and Z from timestamps


	
	#get data about sensors
	sensors <- data.frame(
		idx=xpathSApply(xml,"//definitions/device/sensor/@idx"),
		device=xpathSApply(xml,"//definitions/device/sensor/@device"),
		unit=xpathSApply(xml,"//definitions/device/sensor/@unit"),
		resolution=xpathSApply(xml,"//definitions/device/sensor/@resolution"),
		 stringsAsFactors=F)
	
	sensorAttr <- data.frame(
		gid=xpathSApply(xml,"//definitions/unit[display_id/text/@lang]/@gid"),
		dimension=xpathSApply(xml,"//definitions/unit[display_id/text/@lang]/@dimension"),
		lang=xpathSApply(xml,"//definitions/unit/display_id/text/@lang"),
		short=xpathSApply(xml,"//definitions/unit/display_id/text/@short"),
		long=xpathSApply(xml,"//definitions/unit/display_id/text/@long"),
		 stringsAsFactors=F
		)
	
	sensors <- merge(sensors, sensorAttr, by.x="unit",by.y="gid")
 
	#get detections with sensor data
	sns <- data.frame(
		 device=xpathSApply(xml, "//log/records/detection/sample/@device"), 
		 sensor=xpathSApply(xml,"//log/records/detection/sample/@sensor"), 
		 sensor_value=xpathSApply(xml,"//log/records/detection/sample/@value"),
		 time=xpathSApply(xml, "//log/records/detection[sample/@sensor]/@time"),
		 stringsAsFactors=F)
	
	sns <- merge(sns,sensors, by="device")
		
	detects$transmitter_name <- ''
	detects$transmitter_serial <- ''
	detects$sensor_value <- ''
	detects$sensor_value[match(sns$time,detects$time)] <- sns$sensor_value
	detects$sensor_units <- ''
	detects$sensor_units[match(sns$time,detects$time)] <- sns$short
	detects$station_name <- ''
	detects$latitude[detects$latitude == 0] <- ''
	detects$longitude[detects$longitude == 0] <- ''
	
	#sort by timestamp
	detects$time <- gsub("T"," ",detects$time)
	detects$time <- gsub("Z","",detects$time)	
	detects <- detects[order(as.POSIXct(detects$time,tz="GMT")),]
	
	#order and rename columns to be consistent with Vemco default format
	detects <- detects[,c("time","receiver","transmitter","transmitter_name","transmitter_serial","sensor_value","sensor_units",
		"station_name","latitude","longitude")]	
	names(detects) <- c("Date and Time (UTC)","Receiver","Transmitter","Transmitter Name",
		"Transmitter Serial","Sensor Value","Sensor Unit","Station Name","Latitude","Longitude")
		
	#format latitude and longitude to be consistent with Vemco default format
	if(any(detects$Latitude != '')) detects$Latitude[as.numeric(detects$Latitude) > 0] <- 
		paste0("+",detects$Latitude[as.numeric(detects$Latitude) >= 0])
	if(any(detects$Longitude != '')) detects$Longitude[as.numeric(detects$Longitude) > 0] <- 
		paste0("+",detects$Longitude[as.numeric(detects$Longitude) >= 0])
	
	
	#write to csv
	write.csv(detects,gsub(".ulfx$|.ULFx$","_detections.csv",xmlFile),row.names=F,quote=F)
	
	

	#receiver events

	#- memory stats
	   #intermediate values
	   mem.x <- data.frame(
			 # - memory used
			 #end_ptr=as.numeric(xpathSApply(xml,"//log/records/memory_stats/segment/@end_ptr")),
			 end_ptr=as.numeric(xpathSApply(xml,"//log/records/memory_stats[@trigger = 'scheduled']/segment/@end_ptr")),
			 # - max memory
			 memory_size=as.numeric(xpathSApply(xml,"//log/records/memory_stats[@trigger = 'scheduled']/segment/@memory_size")),
			 # - class
			 class=xpathSApply(xml,"//log/records/memory_stats[@trigger = 'scheduled']/segment/@class"))
	   mem.x$percent_used <- with(mem.x, end_ptr/memory_size)
	   
	   mem <- data.frame(
			event_timestamp_utc=xpathSApply(xml,"//log/records/memory_stats[@trigger = 'scheduled']/@time"),
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
		  

	#- receiver stats
	   rec.nodes <- getNodeSet(xml, "//log/records/receiver_stats")
	   keep <- sapply(rec.nodes,function(x) all(c("device","receiver","pulse_count","sync_count","reject_count","time") %in% names(xmlAttrs(x))))
	   rec.nodes <- rec.nodes[keep] #subset
	   
	   rec.pings <- data.frame(
			event_timestamp_utc=sapply(rec.nodes,function(x) xmlGetAttr(x,"time")),
			receiver=devs$short[devs$receiver],
			description="Daily Pings",
			data=sapply(rec.nodes,function(x) xmlGetAttr(x,"pulse_count")),
			units="",
			stringsAsFactors=F)
	   rec.syncs <- data.frame(
			event_timestamp_utc=sapply(rec.nodes,function(x) xmlGetAttr(x,"time")),
			receiver=devs$short[devs$receiver],
			description="Daily Syncs",
			data=sapply(rec.nodes,function(x) xmlGetAttr(x,"sync_count")),
			units="",
			stringsAsFactors=F)
	   rec.rejects <- data.frame(
			event_timestamp_utc=sapply(rec.nodes,function(x) xmlGetAttr(x,"time")),
			receiver=devs$short[devs$receiver],
			description="Daily Rejects",
			data=sapply(rec.nodes,function(x) xmlGetAttr(x,"reject_count")),
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

		
	#append all parts and export

	logOut <- rbind(
		bat,
		mem,
		rec.pings,
		rec.syncs,
		rec.rejects,
		offload,
		pctime,
		dailydets,
		lastdecode,
		init,
		init.pctime,
		map,
		blanking)
		
	logOut <- as.data.frame(logOut, check.names=F, stringsAsFactors=F)
		
	logOut$event_timestamp_utc <- gsub("T"," ",logOut$event_timestamp_utc) #replace T with space
	logOut$event_timestamp_utc <- gsub("Z","",logOut$event_timestamp_utc) #drop Z
	 

	#enforce same order as VUE export
	event.order <- c(   "Initialization",
						"Memory Capacity",
						"Battery",
						"Daily Pings",
						"Daily Syncs",
						"Daily Rejects",
						c(paste0("Daily Detections on ",codespaces$short),
						paste0("Last Detection on ",codespaces$short))[order(c(codespaces$short,codespaces$short))],
						"Data Upload",
						"PC Time",
						"Map",
						"Blanking")
	ord <- match(logOut$description, event.order)                    

	logOut <- logOut[order(as.POSIXct(logOut$event_timestamp_utc,tz="GMT"),ord),]      
	
	names(logOut) <- c("Date/Time","Receiver","Description","Data","Units")

	write.csv(logOut,gsub(".ulfx$|.ULFx$","_receiverEvents.csv",xmlFile),row.names=F,quote=F)
}

