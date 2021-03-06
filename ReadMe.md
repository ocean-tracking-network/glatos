glatos: An R package for the Great Lakes Acoustic Telemetry Observation System
------------------------------------------------------------------------------

glatos is an R package with functions useful to members of the Great Lakes Acoustic Telemetry Observation System (<http://glatos.glos.us>). Functions may be generally useful for processing, analyzing, simulating, and visualizing acoustic telemetry data, but are not strictly limited to acoustic telemetry applications.

### Package status

*This package is in early development and its contents are evolving.* For recent changes, see [NEWS](https://github.com/ocean-tracking-network/glatos/blob/main/NEWS.md) for recent changes. To access the package or contribute code, join the project at https://github.com/ocean-tracking-network/glatos. If you encounter problems or have questions or suggestions, please post a new issue or email <cholbrook@usgs.gov> (maintainer: Chris Holbrook).

### Installation

Installation instructions can be found at https://github.com/ocean-tracking-network/glatos/wiki/installation-instructions

### Contents

#### Data loading and processing

1.  [`read_glatos_detections`](https://github.com/ocean-tracking-network/glatos/blob/main/R/load-read_glatos_detections.r) and [`read_otn_detections`](https://github.com/ocean-tracking-network/glatos/blob/main/R/load-read_otn_detections.r) provide fast data loading from standard GLATOS and OTN data files to a single structure that is compatible with other glatos functions.

2.  [`read_glatos_receivers`](https://github.com/ocean-tracking-network/glatos/blob/main/R/load-read_glatos_receivers.r) and [`read_otn_deployments`](https://github.com/ocean-tracking-network/glatos/blob/main/R/load-read_otn_deployments.r) reads receiver location histories from standard GLATOS and OTN data files to a single structure that is compatible with other glatos functions.

3.  [`read_glatos_workbook`](https://github.com/ocean-tracking-network/glatos/blob/main/R/load-read_glatos_workbook.r) reads project-specific receiver history and fish taggging and release data from a standard glatos workbook file.

4.  [`read_vemco_tag_specs`](https://github.com/ocean-tracking-network/glatos/blob/main/R/load-read_vemco_tag_specs.r) reads transmitter (tag) specifications and operating schedule.

5.  [`real_sensor_values`](https://github.com/ocean-tracking-network/glatos/blob/main/R/proc-real_sensor_values.r) converts 'raw' transmitter sensor (e.g., depth, temperature) to 'real'-scale values (e.g., depth in meters) using transmitter specification data (e.g., from read\_vemco\_tag\_specs).

6. [`prepare_tag_sheet`](https://github.com/ocean-tracking-network/glatos/blob/main/R/load-prepare_tag_sheet.r) and [`prepare_deploy_sheet`](https://github.com/ocean-tracking-network/glatos/blob/main/R/load-prepare_deploy_sheet.r) load OTN metadata sheets for Tagging and Deployment of Receivers and formats them for converting to ATT Data.


#### Filtering and summarizing

1.  [`min_lag`](https://github.com/ocean-tracking-network/glatos/blob/main/R/proc-min_lag.r) facilitates identification and removal of false positive detections by calculating the minimum time interval (min\_lag) between successive detections.

2.  [`detection_filter`](https://github.com/ocean-tracking-network/glatos/blob/main/R/filt-false_detections.r) removes potential false positive detections using "short interval" criteria (see *min\_lag*).

3.  [`detection_events`](https://github.com/ocean-tracking-network/glatos/blob/main/R/summ-detection_events.r) distills detection data down to a much smaller number of discrete detection events, defined as a change in location or time gap that exceeds a threshold.

4.  [`summarize_detections`](https://github.com/ocean-tracking-network/glatos/blob/main/R/summ-summarize_detections.r) calculates number of fish detected, number of detections, first and last detection timestamps, and/or mean location of receivers or groups, depending on specific type of summary requested.

5.  [`residence_index`](https://github.com/ocean-tracking-network/glatos/blob/main/R/summ-residence_index.r) calculates the relative proportion of time spent at each location.

6.  [`REI`](https://github.com/ocean-tracking-network/glatos/blob/main/R/REI.r) calculates the relative activity at each receiver based on number of unique 
species and individual animals.

#### Simulation functions for system design and evaluation

1.  [`calc_collision_prob`](https://github.com/ocean-tracking-network/glatos/blob/main/R/sim-calc_collision_prob.r) estimates the probability of collisions for pulse-position-modulation type co-located telemetry transmitters. This is useful for determining the number of fish to release or tag specifications (e.g., delay).

2.  [`receiver_line_det_sim`](https://github.com/ocean-tracking-network/glatos/blob/main/R/sim-receiver_line_det_sim.r) simulates detection of acoustic-tagged fish crossing a receiver line (or single receiver). This is useful for determining optimal spacing of receviers in a line and tag specifications (e.g., delay).

3.  [`crw_in_polygon`](https://github.com/ocean-tracking-network/glatos/blob/main/R/simutil-crw_in_polygon.r), [`transmit_along_path`](https://github.com/ocean-tracking-network/glatos/blob/main/R/sim-transmit_along_path.r), and [`detect_transmissions`](https://github.com/ocean-tracking-network/glatos/blob/main/R/sim-detect_transmissions.r) individually simulate random fish movement paths within a water body (*crw\_in\_polygon*: a random walk in a polygon), tag signal transmissions along those paths (*transmit\_along\_path*: time series and locations of transmissions based on tag specs), and detection of those transmittions by receivers in a user-defined receiver network (*detect\_transmissions*: time series and locations of detections based on detection range curve). Collectively, these functions can be used to explore, compare, and contrast theoretical performance of a wide range of transmitter and receiver network designs.

#### Visualization and data exploration

1.  [`abacus_plot`](https://github.com/ocean-tracking-network/glatos/blob/main/R/vis-abacus_plot.r) is useful for exploring movement patterns of individual tagged animals through time.

2.  [`detection_bubble_plot`](https://github.com/ocean-tracking-network/glatos/blob/main/R/vis-detection_bubble_plot.r) is useful for exploring distribution of tagged individuals among receivers.

3.  [`interpolate_path`](https://github.com/ocean-tracking-network/glatos/blob/main/R/vis-interpolate_path.r), [`make_frames`](https://github.com/ocean-tracking-network/glatos/blob/main/R/vis-make_frames.r), and [`make_video`](https://github.com/ocean-tracking-network/glatos/blob/main/R/vis-make_video.r) Interpolate spatio-temporal movements, between detections, create video frames, and stitch frames together to create animated video file using *FFmpeg* software.

4.  [`adjust_playback_time`](https://github.com/ocean-tracking-network/glatos/blob/main/R/vis-adjust_playback_time.r) modify playback speed of videos and optionally convert between video file formats. Requires *FFmpeg*

#### Data Exporting

1. [`convert_glatos_to_att`](https://github.com/ocean-tracking-network/glatos/blob/main/R/util-convert_glatos_to_att.r) converts the glatos
detection and receiver objects to a format supported by [VTrack](https://github.com/RossDwyer/VTrack)/[ATT](https://github.com/vinayudyawer/ATT).

2. [`convert_otn_erddap_to_att`](https://github.com/ocean-tracking-network/glatos/blob/main/R/util-convert_otn_erddap_to_att.r) converts the OTN
detection and ERDDAP csvs of OTN animals, tags and stations to a format supported by [VTrack](https://github.com/RossDwyer/VTrack)/[ATT](https://github.com/vinayudyawer/ATT).

3. [`convert_otn_to_att`](https://github.com/ocean-tracking-network/glatos/blob/main/R/util-convert_otn_to_att.r) converts the OTN detections and metadata sheets to the ATT format. Also accepts deployment metadata from the [OTN website](https://members.oceantrack.org/) in CSV format.
