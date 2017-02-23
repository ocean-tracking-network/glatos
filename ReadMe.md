## glatos: An R package for the Great Lakes Acoustic Telemetry Observation System

### Description  
glatos is an R package with functions useful to members of the Great Lakes Acoustic Telemetry Observation System ([http://glatos.glos.us](http://glatos.glos.us)). Functions may be generally useful for processing, analysing, simulating, and visualizing acoustic telemetry data, but are not strictly limited to acoustic telemetry applications.

### Contents  
#### Simulation functions for system design and evaluation

1. [`clacCollisionProb`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/calcCollisionProb.r) estimates the probability of collisions for PPM-type co-located telemetry transmitters. This is useful for determining the number of fish to release or tag specifications (e.g., delay). 

2. [`receiverLineDetSim`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/receiverLineDetSim.r) simulates detection of acoustic-tagged fish crossing a receiver line (or single receiver). This is useful 
for determining optimal spacing of receviers in a line and tag specifications (e.g., delay). 

3. [`crwInPolygon`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/crwInPolygon.r), [`transmitAlongPath`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/transmitAlongPath.r), and [`detectTransmissions`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/detectTransmissions.r) individually simulate random fish movement paths within a water body (`crwInPolygon`: a random walk in a polygon), tag signal transmissions along those paths (`transmitAlongPath`: time series and locations of transmissions based on tag specs), and detection of those transmittions by receivers in a user-defined receiver network (`detectTransmissions`: time series and locations of detections based on detection range curve). Collectively, these functions can be used to explore, compare, and contrast theoretical performance of a wide range of transmitter and receiver network designs.  


#### Data processing and summarization  

1. [`falseDetectionFilter`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/falseDetectionFilter.r) identifies potential false detections in the GLATOS standard data export package using "short interval" criteria (GLATOS min_lag column). 

2. [`detectionEventFilter`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/detectionEventFilter.r) distills detection data down to a much smaller number of discrete detection events, defined as a change in location (defined by user) or time gap that exceeds a threshold (defined by user). 

#### Visualization and data exploration

1. [`kmlWorkbook`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/kmlWorkbook.r) is useful for exploring receiver and animal release locations in Google Earth. 

2. [`abacusPlot`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/abacusPlot.r) and [`eventPlot`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/eventPlot.r) are useful for exploring movement patterns of individual tagged animals. 

3. [`detectionBubblePlot`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/detectionBubblePlot.r) is useful for exploring distribution of tagged individuals among receivers. 

4. [`movePath`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/movePath.r), [`interpolatePath`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/animate.r), and [`animatePath`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/animateFrames.r) can be used together to interpolate movement paths between detections and save animated movement paths to a video file (mp4).

#### Random utility functions

The following functions were needed by other functions in this package but might be useful for other things too.

1. [`vrl2csv`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/vrl2csv.r) converts a Vemco VRL file to a comma separated values (CSV) file using a system call to VEMCO VUE convert command.
2. [`rotatePoints`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/rotatePoints.r) will rotate a set of 2-d points about another point. 
3. [`crw`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/crw.r) will simulate an unconstrained correlated random walk.
4. [`vectorHeading`](https://gitlab.oceantrack.org/chrisholbrook/glatos/blob/master/R/vectorHeading.r) will calculate (in degrees) the heading of the vector between adjacent point-pairs in a set of positions (e.g., along a track).  


### Installation

The R package GLATOS is availabed from the Ocean Tracking Network's [gitlab](https://gitlab.oceantrack.org/chrisholbrook/glatos).  
  
To install:  

1. install devtoools for R (if you haven't already)
```R
 install.packages("devtools")
```

2. replace USERNAME and PASSWORD in the R code below with your own 
``` R
library(devtools)
# download the package and install
install_git("https://USERNAME:PASSWORD@gitlab.oceantrack.org/chrisholbrook/glatos.git")
```


### Package status
This package is in early development. If you encounter problems or have questions or suggestions, please post a new issue. If you have code to contribute, feel free to start a new branch. Any questions or comments can be sent to cholbrook@usgs.gov (maintainer).
