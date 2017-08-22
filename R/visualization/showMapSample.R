
# To run:

# Sample data:
# Data 1: All 3 fish (Alice, Bob, and Eve) present at all times
anId <- c("Alice", "Bob", "Eve", "Alice", "Bob", "Eve", "Alice", "Bob", "Eve")
timeA <- rep(x="2010/10/11 11:11:11", times=3)
timeB <- rep(x="2010/10/11 11:12:11", times=3)
timeC <- rep(x="2010/10/11 11:13:11", times=3)
times <- c(timeA, timeB, timeC)
times <- as.POSIXct(times, tz="America/Halifax")
long <- c(-63.575993, -63.580284, -63.626032, -63.604145, -63.587708, -63.575306, -63.612943, -63.618736, -63.568354)
lat <- c(44.652902, 44.631772, 44.665845, 44.655283, 44.645422, 44.642827, 44.642665, 44.649300, 44.640201)
smData <- data.frame(animalId=anId, timestamp=times,longitude=long, latitude=lat)
iF <- c("/Users/dinian/Desktop/glatos-git/R/visualization/Icons/redFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/blueFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/greenFish.png")
cF <- c("red", "blue", "green")

# #Data 2: Blue fish detected for one second but does not have path
# anId <- c(Alice, Bob, Alice)
# times <- c("2010/10/11 11:00:00", "2010/10/11 11:00:15", "2010/10/11 11:00:30")
# times <- as.POSIXct(times, tz="America/Halifax")
# long <- c(-63.626032, -63.618736, -63.580284)
# lat <- c(44.665845, 44.649300, 44.631772)
# smData <- data.frame(animalId=anId, timestamp=times,longitude=long, latitude=lat)
# iF <- c("/Users/dinian/Desktop/glatos-git/R/visualization/Icons/redFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/blueFish.png")
# cF <- c("red", "blue", "green")

# #Data 3: Bob(blue fish) detected after Alice(red fish) and leaves before Alice(red fish) but has path
# anId <- c(Alice, Alice, Bob, Alice, Bob, Alice, Alice)
# times <- c("2010/10/11 11:00:00", "2010/10/11 11:00:07", "2010/10/11 11:00:10", "2010/10/11 11:00:15", "2010/10/11 11:00:20", "2010/10/11 11:00:24", "2010/10/11 11:00:30")
# times <- as.POSIXct(times, tz="America/Halifax")
# long <- c(-63.62603, -63.61874, -63.58028, -63.5917, -63.60415, -63.58063, -63.56861)
# lat <- c(44.66584, 44.64930, 44.63177, 44.6366, 44.65528, 44.64747, 44.62328)
# smData <- data.frame(animalId=anId, timestamp=times,longitude=long, latitude=lat)
# iF <- c("/Users/dinian/Desktop/glatos-git/R/visualization/Icons/redFish.png", "/Users/dinian/Desktop/glatos-git/R/visualization/Icons/blueFish.png")
# cF <- c("red", "blue", "green")



# Show full animated map with icons:
showMapIcon(smData, iF)
# OR (using default iconFiles)
showMapIcon(smData)

# Show full animated map with all species with coloured circles
showMapCircle(smData, cF)
# OR (using colour palette)
n <- length(unique(smData$animalId))
showMapCircle(smData, palette(rainbow(n)))


# Show animated map from a different position (viewed into Fairview)
showMapIcon(smData, iF, meanLatitude = 44.66584, meanLongitude = -63.62603, zoom=15)
showMapCircle(smData, iF, meanLatitude = 44.66584, meanLongitude = -63.62603, zoom=14)


# Show animated map with animals of specific animal id ("Eve"):
showIdMap(smData, "Eve", cF)

# Show map following animals with specific animal id ("Eve")
showIdMapFollow(smData, "Eve", cF)

# Show paths / points on map of animals of specific animal id ("Eve"):
showMapPathId(smData, "Eve", cF)

# Show paths of all animals
showMapPaths(smData, cF)

# Show all tags of animals
showMapPoints(smData, cF)

# Show all animals with id defined in 'id' ("Eve")
showMapPointsId(smData, "Eve", cF)