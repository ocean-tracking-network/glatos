# glatos

glatos is an R package with functions useful to members of the Great Lakes Acoustic Telemetry Observation System (www.data.glos.us/glatos). Functions may be generally useful for simulation, processing, analysing, and visualizing acoustic telemetry data, but are not strictly limited to acoustic telemetry application.

### Functions in the package can (and have) be used to:
1. Convert a Vemco VRL file to a comma separated values (CSV) file or ULFX (Vemco XML) file [see `vrl2csv` and `vrl2ulfx`]
2. Estimate probability of collision for PPM-type co-located telemetry transmitters [see `clacCollisionProb`]
3. Simulate detection of acoustic-tagged fish crossing a receiver line (or single receiver) [see `receiverLineDetSim`]
4. Make a KML (or KMZ) file (e.g., for viewing GLATOS receiver and release locations in Google Earth) from an existing GLATOS Data Workbook. [see `kmlWorkbook`]
5. Simulate movement, tag signal transmissions, and detection of fish in a user-defined array [e.g., using `crwInPolygon`, `transmitAlongPath`, and `detectTransmissions`]


## Installation

1. install devtoools for R (if you haven't already
```R
 install.packages("devtools")
```

2. to install, replace USERNAME and PASSWORD in the R code below with your own 
``` R
library(devtools)
# download the package and install
install_git("https://USERNAME:PASSWORD@gitlab.oceantrack.org/chrisholbrook/glatos.git")
```


## Help wanted!
This package is early in development. Suggestions or requests for new functions, enhancements, bug fixes etc. are needed. If you are interested in contributing (even just ideas), feel free to start a new issue or branch. Or email cholbrook@usgs.gov.
