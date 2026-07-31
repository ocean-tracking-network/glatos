# Package index

## All functions

- [`REI()`](https://github.io/reference/REI.md) : Calculates a returns a
  list of each station and the REI (defined here)

- [`abacus_plot()`](https://github.io/reference/abacus_plot.md) : Plot
  detection locations of acoustic transmitters over time

- [`adjust_playback_time()`](https://github.io/reference/adjust_playback_time.md)
  : Modify playback time of video

- [`aggregate_total_no_overlap()`](https://github.io/reference/aggregate_total_no_overlap.md)
  : The function below aggregates timedelta of first_detection and
  last_detection, excluding overlap between detections. Any overlap
  between two detections is converted to a new detection using the
  earlier first_detection and the latest last_detection. If the
  first_detection and last_detection are the same, a timedelta of one
  second is assumed.

- [`aggregate_total_with_overlap()`](https://github.io/reference/aggregate_total_with_overlap.md)
  : The function below aggregates timedelta of first_detection and
  last_detection of each detection into a final timedelta then returns a
  float of the number of days. If the first_detection and last_detection
  are the same, a timedelta of one second is assumed.

- [`calc_collision_prob()`](https://github.io/reference/calc_collision_prob.md)
  : Estimate probability of collision for telemetry transmitters

- [`cast()`](https://github.io/reference/cast.md) : Cast a list of
  scalars to a new class

- [`check_vdat()`](https://github.io/reference/check_vdat.md) :

  Check path to Innovasea program `vdat.exe`

- [`check_vue()`](https://github.io/reference/check_vue.md) : Check path
  to Innovasea program VUE.exe

- [`convert_glatos_to_att()`](https://github.io/reference/convert_glatos_to_att.md)
  : Convert detections and receiver metadata to a format that ATT
  accepts.

- [`convert_otn_erddap_to_att()`](https://github.io/reference/convert_otn_erddap_to_att.md)
  : Convert detections, transmitter, receiver, and animal metadata to a
  format that ATT accepts.

- [`convert_otn_to_att()`](https://github.io/reference/convert_otn_to_att.md)
  : Convert detections, tagging metadata, and deployment metadata to a
  format that ATT accepts.

- [`crw()`](https://github.io/reference/crw.md) : Simulate a correlated
  random walk

- [`crw_in_polygon()`](https://github.io/reference/crw_in_polygon.md) :
  Simulate a correlated random walk inside a polygon

- [`detect_transmissions()`](https://github.io/reference/detect_transmissions.md)
  : Simulate detection of transmitter signals in a receiver network

- [`detection_bubble_plot()`](https://github.io/reference/detection_bubble_plot.md)
  : Make bubble plots showing the number of fish detected across a
  defined set of receiver locations.

- [`detection_events()`](https://github.io/reference/detection_events.md)
  : Classify discrete events in detection data

- [`detection_range_model()`](https://github.io/reference/detection_range_model.md)
  : Detection Range Probability Model

- [`false_detections()`](https://github.io/reference/false_detections.md)
  : False detection filter

- [`flynn_island_polygon`](https://github.io/reference/flynn_island_polygon.md)
  : An sf POLYGON object with coastline of Flynn Island

- [`flynn_island_transition`](https://github.io/reference/flynn_island_transition.md)
  : A transition object for Flynn Island for testing make_transition

- [`format_POSIXt()`](https://github.io/reference/format_POSIXt.md) :
  Round timestamp by fractional second and coerce to character

- [`get_days()`](https://github.io/reference/get_days.md) : Determines
  which calculation method to use for the residency index.

- [`get_local_vdat_template()`](https://github.io/reference/get_local_vdat_template.md)
  :

  Get schema from local installation of Innovasea program `vdat.exe`

- [`get_local_vdat_version()`](https://github.io/reference/get_local_vdat_version.md)
  : Get version of local installation of Innovasea program vdat.exe

- [`get_local_vue_version()`](https://github.io/reference/get_local_vue_version.md)
  : Get version of local installation of Innovasea program VUE.exe

- [`glatos-defunct`](https://github.io/reference/glatos-defunct.md)
  [`check_dependencies`](https://github.io/reference/glatos-defunct.md)
  [`make_video_ffmpeg`](https://github.io/reference/glatos-defunct.md)
  [`install_ffmpeg`](https://github.io/reference/glatos-defunct.md) :
  Defunct functions in glatos

- [`vrl2csv()`](https://github.io/reference/glatos-deprecated.md) :

  Deprecated functions in package glatos.

- [`glatos-package`](https://github.io/reference/glatos.md)
  [`glatos`](https://github.io/reference/glatos.md) : glatos: A package
  for the Great Lakes Acoustic Telemetry Observation System

- [`glatos_animals()`](https://github.io/reference/glatos_animals.md)
  [`as_glatos_animals()`](https://github.io/reference/glatos_animals.md)
  [`is_glatos_animals()`](https://github.io/reference/glatos_animals.md)
  [`validate_glatos_animals()`](https://github.io/reference/glatos_animals.md)
  : Construct, check, and validate a glatos_animals object

- [`glatos_check_col_names()`](https://github.io/reference/glatos_check_col_names.md)
  [`glatos_check_col_classes()`](https://github.io/reference/glatos_check_col_names.md)
  : Check column names and classes of a list or data.frame against
  requirements

- [`glatos_detections()`](https://github.io/reference/glatos_detections.md)
  [`as_glatos_detections()`](https://github.io/reference/glatos_detections.md)
  [`is_glatos_detections()`](https://github.io/reference/glatos_detections.md)
  [`validate_glatos_detections()`](https://github.io/reference/glatos_detections.md)
  : Construct, check, and validate a glatos_detections object

- [`glatos_receivers()`](https://github.io/reference/glatos_receivers.md)
  [`as_glatos_receivers()`](https://github.io/reference/glatos_receivers.md)
  [`is_glatos_receivers()`](https://github.io/reference/glatos_receivers.md)
  [`validate_glatos_receivers()`](https://github.io/reference/glatos_receivers.md)
  : Construct, check, and validate a glatos_receivers object

- [`greatLakesPoly`](https://github.io/reference/greatLakesPoly.md) :

  [Deprecated](https://rdrr.io/r/base/Deprecated.html) A
  SpatialPolygonDataFrame with Great Lakes coastline and some major
  tributaries.

- [`greatLakesTrLayer`](https://github.io/reference/greatLakesTrLayer.md)
  :

  A `TransitionLayer` of the Great Lakes that only prevents transition
  over land

- [`great_lakes_polygon`](https://github.io/reference/great_lakes_polygon.md)
  : An sf POLYGON object with Great Lakes coastline

- [`higgins_lake_polygon`](https://github.io/reference/higgins_lake_polygon.md)
  : An sf POLYGON object with coastline of Higgins Lake

- [`higgins_lake_transition`](https://github.io/reference/higgins_lake_transition.md)
  : A transition object for Higgins Lake for testing make_transition

- [`identify_workbook_version()`](https://github.io/reference/identify_workbook_version.md)
  : Identify and check GLATOS workbook file version

- [`interpolate_path()`](https://github.io/reference/interpolate_path.md)
  : Interpolate new positions within a spatiotemporal path data

- [`interval_count()`](https://github.io/reference/interval_count.md) :
  The function below takes a detection events data frame and determines
  the number of time bins in which detections were observed and returns
  the cumulative time covered by all bins, in days. Interval (bin) size
  is determined by the 'time_interval_size' argument.

- [`jarasterize()`](https://github.io/reference/jarasterize.md) : Just
  another rasterizer

- [`kml_to_csv()`](https://github.io/reference/kml_to_csv.md) : KML To
  CSV Conversion

- [`kml_workbook()`](https://github.io/reference/kml_workbook.md) : Make
  a KML or KMZ file of receiver and animal release locations

- [`lamprey_tracks`](https://github.io/reference/lamprey_tracks.md) :
  Sea Lamprey positions from Lake George, St. Marys River, 2012

- [`make_frames()`](https://github.io/reference/make_frames.md) : Create
  an animated video of spatiotemporal path data

- [`make_transition()`](https://github.io/reference/make_transition.md)
  : Create transition layer from spatial object.

- [`make_video()`](https://github.io/reference/make_video.md) : Create
  video from sequence of still images

- [`min_lag()`](https://github.io/reference/min_lag.md) : Calculate
  'min_lag' for identifying potential false positive detections

- [`otn_aat_animals`](https://github.io/reference/otn_aat_animals.md) :
  Example animal data from the OTN ERDDAP

- [`otn_aat_receivers`](https://github.io/reference/otn_aat_receivers.md)
  : Example station data from the OTN ERDDAP

- [`otn_aat_tag_releases`](https://github.io/reference/otn_aat_tag_releases.md)
  : Example tag release data from the OTN ERDDAP

- [`point_offset()`](https://github.io/reference/point_offset.md) :
  Identify new location based on distance and bearing from another

- [`position_heat_map()`](https://github.io/reference/position_heat_map.md)
  : Position Heat Maps

- [`prepare_deploy_sheet()`](https://github.io/reference/prepare_deploy_sheet.md)
  :

  Loads the OTN receiver deployment metadata sheet to prepare it for use
  in `convert_otn_to_att`

- [`prepare_tag_sheet()`](https://github.io/reference/prepare_tag_sheet.md)
  :

  Loads the OTN tagging metadata sheet to prepare it for use in
  `convert_otn_to_att`

- [`range_detection`](https://github.io/reference/range_detection.md) :
  Detection range data set

- [`raw_lamprey_workbook`](https://github.io/reference/raw_lamprey_workbook.md)
  : Raw GLATOS Workbook from St. Marys River Sea Lamprey project

- [`raw_walleye_detections`](https://github.io/reference/raw_walleye_detections.md)
  : Zipped GLATOS detection file from Huron Erie Corridor Walleye
  project

- [`read_glatos_detections()`](https://github.io/reference/read_glatos_detections.md)
  : Read data from a GLATOS detection file

- [`read_glatos_receivers()`](https://github.io/reference/read_glatos_receivers.md)
  : Read data from a GLATOS receiver location file

- [`read_glatos_workbook()`](https://github.io/reference/read_glatos_workbook.md)
  : Read data from a GLATOS project workbook

- [`read_otn_deployments()`](https://github.io/reference/read_otn_deployments.md)
  : Read data from a OTN deployment file

- [`read_otn_detections()`](https://github.io/reference/read_otn_detections.md)
  : Read data from a OTN detection file

- [`read_vdat_csv()`](https://github.io/reference/read_vdat_csv.md) :
  Read data from an Innovasea Fathom VDAT CSV file

- [`read_vemco_tag_specs()`](https://github.io/reference/read_vemco_tag_specs.md)
  : Read telemetry transmitter (tag) specification data from a Vemco
  file

- [`read_vue_detection_csv()`](https://github.io/reference/read_vue_detection_csv.md)
  : Read detection data exported from Innovasea VUE software

- [`read_vue_event_csv()`](https://github.io/reference/read_vue_event_csv.md)
  : Read receiver event data exported from Innovasea VUE software

- [`read_workbook_project()`](https://github.io/reference/read_workbook_project.md)
  : Read Project sheet from GLATOS workbook file

- [`real_sensor_values()`](https://github.io/reference/real_sensor_values.md)
  : Add 'real'-scale sensor values to glatos detetections

- [`receiver_line_det_sim()`](https://github.io/reference/receiver_line_det_sim.md)
  : Simulate detection of acoustic-tagged fish crossing a receiver line

- [`residence_index()`](https://github.io/reference/residence_index.md)
  : Generate the residence index from a set of detections

- [`rotate_points()`](https://github.io/reference/rotate_points.md) :
  Rotate points in a 2-d plane

- [`sample_detection_efficiency`](https://github.io/reference/sample_detection_efficiency.md)
  : Detection Efficiency data set

- [`scale_meters_to_degrees()`](https://github.io/reference/scale_meters_to_degrees.md)
  : Get degree-scale equivalent of meter-scale distance on a spatial
  object

- [`shoreline`](https://github.io/reference/shoreline.md) : zipped
  polygon shapefile of Great Lakes

- [`` `[`( ``*`<vdat_list>`*`)`](https://github.io/reference/sub-.vdat_list.md)
  : Subset method for vdat_list that retains attributes

- [`summarize_detections()`](https://github.io/reference/summarize_detections.md)
  : Summarize detections by animal, location, or both

- [`total_diff_days()`](https://github.io/reference/total_diff_days.md)
  : The function below determines the total days difference.

- [`transmit_along_path()`](https://github.io/reference/transmit_along_path.md)
  : Simulate telemetry transmitter signals along a path

- [`vdat_convert()`](https://github.io/reference/vdat_convert.md) :
  Convert an Innovasea VRL or VDAT file to a Fathom CSV file

- [`vdat_csv_schema`](https://github.io/reference/vdat_csv_schema.md) :
  A schema for Innovasea Fathom (VDAT) CSV files

- [`vector_heading()`](https://github.io/reference/vector_heading.md) :
  Calculate direction (heading) of a vector (in degrees)

- [`video-images`](https://github.io/reference/video-images.md) : Video
  frames of walleye movements in Lake Huron

- [`vue_convert()`](https://github.io/reference/vue_convert.md) :
  Convert an Innovasea Vemco VRL file to a VUE CSV file

- [`write_vdat_csv()`](https://github.io/reference/write_vdat_csv.md) :
  Write a vdat_list object to disk in Innovasea Fathom VDAT CSV format
