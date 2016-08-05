#glatos

glatos is an R package with functions useful to members of the Great Lakes Acoustic Telemetry Observation System (www.data.glos.us/glatos); many more broadly relevant for simulation, processing, analysing, and visualizing acoustic telemetry data. 

Functions in the package can (and have) be used to:
1. Estimate probability of collision for PPM-type co-located telemetry transmitters [see `clacCollisionProb`]
2. Simulate detection of acoustic-tagged fish crossing a receiver line (or single receiver) [see `receiverLineDetSim`]
3. Make a KML (or KMZ) file (e.g., for viewing GLATOS receiver and release locations in Google Earth) from an existing GLATOS Data Workbook. [see `kmlWorkbook]
4. Simulate movement, tag signal transmissions, and detection of fish in a user-defined array [e.g., using `crwInPolygon`, `transmitAlongPath`, `and `detectionTransmissions`]

##Installation

1. install devtoools for R (if you haven't already
```R
 install.packages("devtools")
```

2. to install, replace USERNAME and PASSWORD in the R code below with your own 
```R
library(devtools)

#download the package and install
install_git("https://USERNAME:PASSWORD@gitlab.oceantrack.org/chrisholbrook/glatos.git")
```
