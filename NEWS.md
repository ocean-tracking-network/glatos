
----

# glatos 0.4.1

#### 2020-02-12
   
### Bug fixes

- abacus_plot 
    - fix issue where optional plot args passed as expressions 
		  (e.g., `panel.first` and `panel.last`) had to be wrapped in `quote`
    - fixes issue #87

- summarize_detections
    - fix error in documentation listing all three summ_type options as 
      default
    - fixes issue #94

- check_dependencies
    - ensure that glatos package directory is checked for ffmpeg.exe
    - fixes issue #95

----

# glatos 0.4.0

#### 2019-11-12
   
### New features

- functions
    - convert_glatos_to_att
    - convert_otn_erddap_to_att
		
- example data
    - walleye_att
    - blue_shart_att


### Minor changes

- make_frames
    - add argument show_progress (default = TRUE) to optionally supress progress bars
    - issue #74
		
- interpolate_path
    - add argument show_progress (default = TRUE) to optionally supress progres bars
    - issue #74
		
- calc_collision_probs
    - in help examples, make separate plots for detection and collision probs
    - issue #77
		
- make_video
    - issue #64
    - return output file name
    - add diagnostic_mode arg to optionally return ffmpeg output (hide by default)
    - add warning when output file exists and overwrite = FALSE
    
- adjust_playback_time
    - return output file name
    - add diagnostic_mode arg to optionally return ffmpeg output (hide by default)
    - add warning when output file exists and overwrite = FALSE

----

# glatos 0.3.2

#### 2019-10-17
   
### bug fixes and minor changes

- vrl2csv
    - fixed bug where output csv file names were not returned when input   
		  was multiple VRL file names. A vector of output CSV file names 
			is now returned.
    - make separate call to VUE --convert-files for each VRL file.
    - add progress bar
    - improve error messaging.

- detect_transmissions 
    - fixed bug where 'trns_x' and 'trns_y' in data slot of output was not 
		  converted to input coordinate reference system when *sp_out = TRUE*.

- make_frames
    - fixed bug where receivers were not displayed near the end of the time 
		  series in the images (and videos) when some receivers were missing 
			recovery data (issue #79).

- abacus_plot
    - added more flexible optional plotting arguments, including 'xlim' to 
      set custom x limits.
    - fixed bug where plot title (specified by *main* argument) was not 
		  included in plot (issue #70)
    - deprecated *show_receiver_status* argument. Receiver status will 
		  be shown on plot whenever 'receiver_history' is given.


----

# glatos 0.3.1

#### 2019-02-27
   
### bug fixes

- fix missing brackets in glatos.Rd


----

# glatos 0.3.0

#### 2019-02-26
   
### New features

- functions
    - receiver_efficiency
    - residence_index
    - read_otn_deployments
    - install_ffmpeg
    - check_dependencies

- vignettes and documentation
    - added data-loading, data-requirements, receiver_efficency, residence_index

### Minor changes

- detection_events
    - output data.table or tibble object if input is either of those and 
		  data.frame otherwise
    - faster (e.g., 11 sec vs 31 sec for 7.2M records on wimpy laptop)
    - coerce 'time_sep' argument to numeric if quoted and error if it cannot 
		  be coerced to numeric

- crw_in_polygon
    - changed sampling algorithm to mimimize stuck-at-boundary errors
    - added option to input SpatialPolygons object
    - added explicit coordinate reference system (EPSG 3175 - Great Lakes
      projected CRS) so that default units are in meters and simulations are 
      done in that CRS. Other CRS can be specified for other areas (via 
      EPSG argument).
    - added option to return spatial object or simple data frame.

- transmit_along_path
    - added option to input SpatialPolygons object
    - added explicit coordinate reference system (EPSG 3175 - Great Lakes
      projected CRS) so that default units are in meters and simulations are 
      done in that CRS. Other CRS can be specified for other areas (via 
      EPSG argument).
    - added option to return spatial object or simple data frame.	
	
- detect_transmissions
    - added option to input SpatialPolygons object
    - added explicit coordinate reference system (EPSG 3175 - Great Lakes
      projected CRS) so that default units are in meters and simulations are 
      done in that CRS. Other CRS can be specified for other areas (via 
      EPSG argument).
    - added option to return spatial object or simple data frame.	
			
- abacus_plot
    - added optional arguments *show_receiver_status* and *receiver_history*
		  to allow receiver history status to be added to plot (issue #36)
    - added optional arguments *x_res* and *x_format* to specify x-axis 
      tick mark spacing and format (issue #30)
    - removed input arguments that could also be passed via ellipses (issue #35)

- position_heat_map
    - corrected accuracy issues by converting lat-lon to UTM (issue #8)
    - added optional argument *abs_or_rel* to specify it output is absolute 
      numbers of fish are relative; simplified output so that only a single 
      output type occurs in a given call
    - Changed output to kmz instead of kml
    - new folder is only created when output is png or kmz
		
- dropped fasttime as a depenedency and uses lubridates fasttime implementation 
  instead

- Update sysdata detection schema for OTN

----


# glatos 0.2.7

#### 2018-08-29
   
### Minor changes 
- interpolate_path
    - added status bars during each of three major steps and improved 
      messaging (2018-08-29)

- vector_heading
    - allow input coordinates as longitude and latitude (decimal degrees) 
      (2018-08-03)

----


# glatos 0.2.6

#### 2018-06-11
   
### Minor changes and bug fixes
- read_otn_detections
    - fix bug that indexed columns by position, meaning columns had to be 
      orderd exactly as in spec (issue #52)
- read_glatos_detections
    - fix bug that indexed columns by position, meaning columns had to be 
      orderd exactly as in spec (issue #52)
- read_glatos_receivers
    - fix bug that indexed columns by position, meaning columns had to be 
      orderd exactly as in spec (issue #52)

----

# glatos 0.2.5 

#### 2018-03-14

### New features
- added check_dependencies() for checking if gdal and ffmpeg can be called (issue #40)
    
### Minor changes and bug fixes
- adjust_playback_time()
    - allow spaces in file names and paths (issue #45)
- interpolate_path()
    - trigger error when 'trans' is not a transition object (issue #42)
- make_frames()
    - fixed "cannot allocate vector" error when optional plot arg is vector (issue #47)
- make_transition()
    - allow polygon to be input as SpatialPolygonsDataFrame (issue #37)

----------------------------------------------------  

# glatos 0.2.4 

#### 2018-03-06

    
### Minor changes and bug fixes

- make_video()
    - added support for input and output directories containing spaces (issue #38)
		- check if input directory exists and error/message if not
		- create output directory if it does not exist


----------------------------------------------------  

# glatos 0.2.3  

#### 2018-02-25

    
### Major changes

- A complete overhaul since v0.1.3. This version is no longer compatible
with previous.
    - Adopt style notation suggested by Wickham (R Packages book) 
    - All previous camel-case function names are now snake-case
    
### New features

- Added data loading functions
    - read_glatos_detections
    - read_otn_detections
    - read_glatos_workbook
    - read_glatos_receivers
    - read_vemco_tag_specs
- Added data processing, filtering, and summarizing functions
    - min_lag
    - real_sensor_values
    - summarize_detections
- Added animation functions
    - adjust_playback_time
    - make_frames
    - make_video
    - make_transition


----------------------------------------------------  

# glatos 0.1.3 

#### 2017-03-01

### - initial release on OTN gitlab

