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

