## glatos: An R package for the Great Lakes Acoustic Telemetry Observation System

### Description  
glatos is an R package with functions useful to members of the Great Lakes Acoustic Telemetry Observation System ([http://glatos.glos.us](http://glatos.glos.us)) and the Ocean Tracking Network (OTN). Functions may be generally useful for processing, analyzing, simulating, and visualizing acoustic telemetry data, but are not strictly limited to acoustic telemetry applications.

### Package status
This package is in early development. To access the package or contribute code, join the project at ([https://gitlab.oceantrack.org/GreatLakes/glatos](https://gitlab.oceantrack.org/GreatLakes/glatos)). If you encounter problems or have questions or suggestions, please post a new issue or email cholbrook@usgs.gov (maintainer: Chris Holbrook).

### Contents  
#### Simulation functions for system design and evaluation

1. [`calcCollisionProb`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/calcCollisionProb.r) estimates the probability of collisions for pulse-position-modulation type co-located telemetry transmitters. This is useful for determining the number of fish to release or tag specifications (e.g., delay). 

2. [`receiverLineDetSim`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/receiverLineDetSim.r) simulates detection of acoustic-tagged fish crossing a receiver line (or single receiver). This is useful 
for determining optimal spacing of receviers in a line and tag specifications (e.g., delay). 

3. [`crwInPolygon`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/crwInPolygon.r), [`transmitAlongPath`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/transmitAlongPath.r), and [`detectTransmissions`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/detectTransmissions.r) individually simulate random fish movement paths within a water body (`crwInPolygon`: a random walk in a polygon), tag signal transmissions along those paths (`transmitAlongPath`: time series and locations of transmissions based on tag specs), and detection of those transmittions by receivers in a user-defined receiver network (`detectTransmissions`: time series and locations of detections based on detection range curve). Collectively, these functions can be used to explore, compare, and contrast theoretical performance of a wide range of transmitter and receiver network designs.  


#### Data processing and summarization  

1. [`getMinLag`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/getMinLag.R) appends a 'min_lag' column to the detection data. This is minimum of the time lag before and after the current row for the same transmitter and receiver. This function can be used in the falseDetectionFilter if it does not already have a min_lag column.

2. [`falseDetectionFilter`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/falseDetectionFilter.r) identifies potential false detections in the GLATOS standard data export package using "short interval" criteria (GLATOS min_lag column or the column appended by the function 'getMinLag'). 

3. [`detectionEventFilter`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/detectionEventFilter.r) distills detection data down to a much smaller number of discrete detection events, defined as a change in location (defined by user) or time gap that exceeds a threshold (defined by user). 

4. [`numIntervalTest`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/numDetectionsAndIntervalTest.R) filters data by the number of detections and interval test (Steckenreuter et al., 2016).

5. [`distTest`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/distTestFunc.R) filters detection data by the distance test (Steckenreuter et al., 2016). 

6. [`velTest`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/velTestFunc.R) filters data by the velocity test (Steckenreuter et al., 2016).

7. [`efficiencyTest`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/efficiencyTestFunc.R) filters data by the number of detections and interval test, distance test, and velocity test (Steckenreuter et al., 2016). It then uses the definition from Steckenreuter and colleagues (2016) to identify if a row is valid (1), questionable (2), or has missing information (3). 

#### Visualization and data exploration

1. [`kmlWorkbook`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/kmlWorkbook.r) is useful for exploring receiver and animal release locations in Google Earth. 

2. [`abacusPlot`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/abacusPlot.R) and [`eventPlot`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/eventPlot.r) are useful for exploring movement patterns of individual tagged animals. 

3. [`detectionBubblePlot`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/detectionBubblePlot.r) is useful for exploring distribution of tagged individuals among receivers. 

4. [`movePath`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/movePath.r), [`interpolatePath`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/interpolatePath.r), and [`animatePath`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/animatePath.r) can be used together to interpolate movement paths between detections and save animated movement paths to a video file (mp4).

5. [`showMap`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/showMap.R) can be used to visualize the movements of the animals on a leaflet map using a shiny application. This uses 'movePath' to determine the location of the animals at every second to plot their movement better on the map.

#### Random utility functions

The following functions were needed by other functions in this package but might be useful for other things too.

1. [`vrl2csv`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/vrl2csv.r) converts a Vemco VRL file to a comma separated values (CSV) file using a system call to VEMCO VUE `convert` command.
2. [`rotatePoints`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/rotatePoints.r) will rotate a set of 2-d points about another point. 
3. [`crw`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/crw.r) will simulate an unconstrained correlated random walk.
4. [`vectorHeading`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/vectorHeading.r) will calculate (in degrees) the heading of the vector between adjacent point-pairs in a set of positions (e.g., along a track).  
5. [`residence_index`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/residence_index.R) will calculate the number of days inside a data frame and will plot this.


#### Testing functions

The following functions were used to test each of the functions that would return a data frame:
1. [`sampleData`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/sampleData.R) includes sample data that can be used for the functions
2. [`showMapSampleData`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/showMapSampleData.R) includes sample data for showMap functions
3. [`testDetectionBubblePlot`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/testDetectionBubblePlot') uses the 'testthat' package to test that the correct data frame is returned from sample data when using the 'detectionBubblePlot' function described above.
4. [`testDetectionEventFilter`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/testDetectionEventFilter.R) uses the 'testthat' package to test that the correct data frame is returned from sample data when using the 'detectionEventFilter' function described above.
5. [`testDistTest`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/testDistTest.R) uses the 'testthat' package to test that the correct data frame is returned from sample data when using the 'distTest' function described above.
6. [`testEfficiencyTest`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/testEfficiencyTest.R) uses the 'testthat' package to test that the correct data frame is returned from sample data when using the 'efficiencyTest' function described above.
7. [`testFalseDetectionFilter`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/testFalseDetectionFilter.R) uses the 'testthat' package to test that the correct data frame is returned from sample data when using the 'falseDetectionFilter' function described above.
8. [`testMinLag`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/testMinLag.R) uses the 'testthat' package to test that the correct data frame is returned from sample data when using the 'getMinLag' function described above.
9. [`testMovePath`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/testMovePath.R) uses the 'testthat' package to test that the correct data frame is returned from sample data when using the 'movePath' function described above.
10. [`testNumDetectionsAndIntervalTest`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/testNumDetectionsAndIntervalTest.R) uses the 'testthat' package to test that the correct data frame is returned from sample data when using the 'numDetectionsAndIntervalTest' function described above.
11. [`testResidenceIndex`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/testResidenceIndex.R) uses the 'testthat' package to test that the correct values are returned from sample data when using the 'residence_index' methods described above.
12. [`testVectorHeading`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/testVectorHeading.R) uses the 'testthat' package to test that the correct values are returned from sample data when using the 'vectorHeading' function described above.
13. [`testVelTest`](https://gitlab.oceantrack.org/GreatLakes/glatos/blob/master/R/testVelTest.R) uses the 'testthat' package to test that the correct data frame is returned from sample data when using the 'velTest' function described above.

### Installation

The R package GLATOS is available from the Ocean Tracking Network's [gitlab](https://gitlab.oceantrack.org/GreatLakes/glatos).  
  
To install:  

1. install devtoools for R (if you haven't already)
```R
 install.packages("devtools")
```

2. download the package from git repository and install
``` R
library(devtools)
# download the package and install
install_git("https://gitlab.oceantrack.org/GreatLakes/glatos.git")
```


### References
(in APA)
Steckenreuter, A., Hoenner, X., Huveneers, C., Simpfendorfer, C., Buscot, M.J., Tattersall, K., ... Harcourt, R. (2016). Optimising the design of large-scale acoustic telemetry curtains. Marine and Freshwater Research. doi: 10.1071/mf/16126

