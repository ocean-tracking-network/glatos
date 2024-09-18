----

# glatos 0.8.9011 (2024-09-18)


## Breaking changes

- Changed `make_transition()` to use `jarasterize()` (see New Features) and 
  added input arg `buffer`.
  - Removed dependence on gdalUtilities.
  - fixes [issue #234](https://github.com/ocean-tracking-network/glatos/issues/234)

- Removed `make_transition2()` (deprecated in 0.7) and `make_transition3()`.
  Also removed dependence on `fasterize`.
  - fixes [issue #67](https://github.com/ocean-tracking-network/glatos/issues/67)
  
- Updated example TransitionLayer object `greatLakesTrLayer` using new
`make_transition()` and `great_lakes_polygon` (an `sf` object) . The new version
has the same cell size (resolution) as the previous, but different extents
(matches great_lakes_polygon, so returned values (with same input) will differ
from earlier versions.
  - fixes [issue #219](https://github.com/ocean-tracking-network/glatos/issues/219)

- Removed 'gganimate_handout' (pdf and html) from 'vignettes'.


### New features

- Add new function `vue_convert()` to convert VRL file to CSV file (detection
  records only; receiver event log records are not supported). 
    - replaces `vrl2csv()` (deprecated).
    
- Add new function `vdat_convert()` to convert VRL or VDAT files to 
  Fathom/VDAT CSV. 
  
- Add new functions for reading data exported from VRL with VUE software.
    - `read_vue_detection_csv()` to read data from detection file exported 
    from VUE or created using `vue_convert()` or `vrl2csv()`.
    - `read_vue_event_csv()` to read data from receiver event log file exported 
    from VUE.
  
- Add new function `read_vdat_csv()` to read data exported from VRL or VDAT 
  using VDAT.exe (e.g., using `vdat_convert()`).

- Add new functions to create, check, and validate `glatos_animals` objects:
    - `glatos_animals()` to construct a `glatos_animals` object from
      individual vectors (one for each column) and optionally check for required
      column names and classes using `validate_glatos_animals()`. 
    - `as_glatos_animals()` to coerce a data.frame, or object that
      inherits from data.frame, to `glatos_animals` and optionally check for
      required column names and classes using `validate_glatos_animals()`.
    - `is_glatos_animals()` to check class attribute for `"glatos_animals"`.
    - `validate_glatos_animals()` to check for existence of required column 
      names and classes.
    - fixes [issue #126](https://github.com/ocean-tracking-network/glatos/issues/126)
    - fixes [issue #78](https://github.com/ocean-tracking-network/glatos/issues/78)
      
- Add new functions to create, check, and validate `glatos_detections` objects:
    - `glatos_detections()` to construct a `glatos_detections` object from
      individual vectors (one for each column) and optionally check for required
      column names and classes using `validate_glatos_detections()`. 
    - `as_glatos_detections()` to coerce a data.frame, or object that
      inherits from data.frame, to `glatos_detections` and optionally check for
      required column names and classes using `validate_glatos_detections()`.
    - `is_glatos_detections()` to check class attribute for `"glatos_detections"`.
    - `validate_glatos_detections()` to check for existence of required column 
      names and classes.
    - fixes [issue #126](https://github.com/ocean-tracking-network/glatos/issues/126)
    - fixes [issue #78](https://github.com/ocean-tracking-network/glatos/issues/78)
    
- Add new functions to create, check, and validate `glatos_recievers` objects:
    - `glatos_receivers()` to construct a `glatos_receivers` object from
      individual vectors (one for each column) and optionally check for required
      column names and classes using `validate_glatos_receivers()`. 
    - `as_glatos_receivers()` to coerce a data.frame, or object that
      inherits from data.frame, to `glatos_receivers` and optionally check for
      required column names and classes using `validate_glatos_receivers()`.
    - `is_glatos_receivers()` to check class attribute for `"glatos_receivers"`.
    - `validate_glatos_receivers()` to check for existence of required column 
      names and classes.
    - fixes [issue #126](https://github.com/ocean-tracking-network/glatos/issues/126)
    - fixes [issue #78](https://github.com/ocean-tracking-network/glatos/issues/78)

- New function `jarasterize()` to allow rasterization 
  (esp. with `all_touched = TRUE`) using only `sf` and `raster`. 
  Used by `make_transition()`.
  
- Add new function `scale_meters_to_degrees()` to inform selection of `res` 
  argument to `make_transition()`.

- In `make_frames()`, allow `terra::SpatVector` input for background map 
  (`bg_map` arg).
    - add test for `terra::SpatVector` input


### Bug fixes and minor changes

- Add `@srs` slot to `greatLakesTrLayer` data object and rename file 
  'data/greatLakesTrLayer.rda'.
    - fixes [issue #213](https://github.com/ocean-tracking-network/glatos/issues/213)

- Fix bug in `read_glatos_workbook()` where timestamps during daylight savings 
  were shifted one hour on linux operating system (not an issue on windows or 
  mac).
    - fixes [issue #208](https://github.com/ocean-tracking-network/glatos/issues/208)

- Omit data.table from class of object returned by `read_glatos_detections()`  
  and `read_glatos_receivers()`. 
    - fixes [issue #200](https://github.com/ocean-tracking-network/glatos/issues/200)

- Fix typo in Description to Suggest 'gifski' (not 'gifsky'). 
    - fixes [issue #185](https://github.com/ocean-tracking-network/glatos/issues/185)

- Various changes to resolve R CMD check errors, warning, and notes (generally
  not user-facing).


----


# glatos 0.7.3 (2024-04-09)


### Bug fixes

- Fixed bug in `summarize_detections()` where values in `num_locs` and 
  `locations` columns were incorrect. 
    - Likely a side effect of 
    [commit 3f0a2ee](https://github.com/ocean-tracking-network/glatos/commit/3f0a2ee366d8c9447bf4b0bff0a1f8dbfd3819a6)
which aimed to fix [issue #182](https://github.com/ocean-tracking-network/glatos/issues/182)


----


# glatos 0.7.2 (2024-02-25)


### Bug fixes


- Fixed bug in `summarize_detections()` where value in `locations` output 
  column was name of input column (e.g. "glatos_array"), rather than values 
  from that column (e.g., "AGR", "BBI").
    - fixed [issue #182](https://github.com/ocean-tracking-network/glatos/issues/182)
    - cherry-picked from [commit db9d69a]( https://github.com/ocean-tracking-network/glatos/commit/db9d69a3d08a97e7b8f86e0d4977aa0909776ddd)
      which was merged with dev but not main.
  

----

# glatos 0.7.1 (2024-01-19)


### Bug fixes and minor changes


- Fix bug introduced in glatos 0.7.0 where x and y limits were switched 
  when `bg_map` was supplied.
    - Add test for various inputs of `background_xlim`, `background_ylim`, and 
      `bg_map` to function `make_frames`.
  

----

# glatos 0.7.0 (2024-01-04)


### Bug fixes and minor changes

- Remove dependence on `rgeos` and `rgdal`.

- This package now requires R >= 3.5.0 because serialized objects in
     serialize/load version 3 cannot be read in older versions of R.
     File(s) containing such objects:
       'glatos/inst/testdata/flynn_island_transition.rds'
       'glatos/inst/testdata/higgins_lake_transition.rds'
       'glatos/inst/testdata/test-detect_transmissions-dtc_spout.rds'
       'glatos/inst/testdata/test-transmit_along_path-tr_dfin_spout.rds'
       
- Deprecate `make_transition` and `make_transition2`; suggest `make_transition3` 
  instead.

- Deprecate data object `greatLakesPoly`; suggest `great_lakes_polygon` 
  instead.

- `prepare_deploy_sheet`: 
    - Set 'skip = header_line - 1' and 'col_names = TRUE' to retain first 
      record and column names (read_excel ignores column names, unless set, 
      when skip is set).
    - Fix non-working example code.

- `convert_otn_to_att` and `convert_otn_erddap_to_att`:
    - Replaced `sp::CRS` with `sf::st_crs`
    - Changed link (URL) to relevant issue from GitLab to GitHub repo.

- Remove ffmpeg functions.
    - make defunct: 
        - `check_dependencies`
        - `install_ffmpeg`
        - `make_video_ffmpeg`

- Fix issues with several tests caused by changes to CRS/WKT and row.names 
  attributes.
    

----

# glatos 0.6.5 (2023-09-07)

### Bug fixes 

- Fix bug in summarize_detections() where setting 'location_col' triggers error.
    - fixes [issue #180](https://github.com/ocean-tracking-network/glatos/issues/180)


----

# glatos 0.6.4 (2023-09-06)

### Bug fixes and minor changes

- Add support for new column named 'record_status' in GLATOS detection export CSV.
    - Add new glatos detection schema version 1.4 (developer use)
    - fix [issue #179](https://github.com/ocean-tracking-network/glatos/issues/179)


----

# glatos 0.6.3 (2023-01-25)

### Bug fixes and minor changes

- Add support for new GLATOS receiver_locations file format; with code_map and 
code_map_comment columns.
    - fix [issue #177](https://github.com/ocean-tracking-network/glatos/issues/177)


----

# glatos 0.6.2 (2022-10-25)

### Bug fixes and minor changes

- Fix error in `position_heat_map()` function resulting in `Error in 
zip_internal... Some files do not exist`. Update documentation.


----

# glatos 0.6.1

#### 2022-10-11

### Minor changes

- Allow `sf` `MUTLIPOLYGON` geometry type for input `polyg` to `crw_in_polygon()`.
    
- Use new function `check_cross_boundary()` instead of `check_in_polygon` to prevent paths crossing land (e.g., over peninsulas) in `crw_in_polygon()`.

----

# glatos 0.6.0

#### 2022-06-24

### Breaking changes

- crw_in_polygon
    - replace input arg 'EPSG' with 'inputCRS' and 'cartesianCRS'
    - unlike previous versions, input polygon ('polyg') must now be in 
      Cartesian (projected) coordinate reference system or 'cartesianCRS' must 
      be specified (there is no default).
    - remove dependence on sp
	  - improve speed
	  - return sf object (default) or data.frame
	  
- transmit_along_path
    - remove dependence on sp
    - return sf object (default) or data.frame
    - calculate distances along path using geodist::geodist for geographic and 
      simple Euclidean for Cartesian coordinates. Cartesian input results in 
      fastest computations.
    - remove EPSG input (no longer requires transformation to Cartesian CRS for 
      calculations); but non-Cartesian input will be slower than Cartesian.
    - add pathCRS input arg for non-spatial inputs
    - change name of column 'et' in output to 'time'.
    - add input arg "colNames" for non-default coordinate column name 
      specification.
      
- detect_transmissions
    - remove dependence on sp
    - return sf object (default) or data.frame
    - calculate distances along path using geodist::geodist for geographic and 
      simple Euclidean for Cartesian coordinates. Cartesian input results in 
      fastest computations.
    - remove EPSG input (no longer requires transformation to Cartesian CRS for 
      calculations); but non-Cartesian input will be slower than Cartesian.
    - add inputCRS input arg for non-spatial inputs
    - change name of column 'et' in output to 'time'.
    - add input arg "colNames" for non-default coordinate column name 
      specification.    

### Bug fixes and minor changes

- position_heat_map
    - fix [issue #159](https://github.com/ocean-tracking-network/glatos/issues/159)
      where kmz output worked on mac but not windows
    - fix [issue #123](https://github.com/ocean-tracking-network/glatos/issues/123)
      when input positions object was data.table.

- remove dependence on PBSmapping
    - replace calls to PBSmapping::convUL with local functions 
      lonlat_to_utm and utm_to_lonlat

- remove dependence on gdalUtils
    - use gdalUtilities::gdal_rasterize instead of gdalUtils::gdal_rasterize
    - fix [issue #174](https://github.com/ocean-tracking-network/glatos/issues/174#)
    - removed gdalUtils dependency from DESCRIPTION and added gdalUtilities

- changes to kml_to_csv
    - read data from kml with sf::read_sf instead of rgdal::readOGR
    - allow multiple feature types to be read from kml
    - change export format from separate csv files to a single csv file
    - add feature name and type to output file

- fix 'greatLakesPoly' CRS to resolve warning: "CRS object has no comment"

- added example data 'great_lakes_polygon'; an sf POLYGON version of 
  'greatLakesPoly' (a SpatialPolygonsDataFrame)
	  

----

# glatos 0.5.2

#### 2021-12-10

### Bug fixes and minor changes

- read_glatos_workbook
    - use readr::readexcel instead of openxlsx to read xlsm file
    - fix [issue #138](https://github.com/ocean-tracking-network/glatos/issues/138#)

- removed openxlsx and cellranger dependencies from DESCRIPTION

- fix issue preventing vignettes from building when installed (missing space in data_loading_vignette.Rmd).



----

# glatos 0.5.1

#### 2021-11-09

### Bug fix

- position_heat_map
    - fix issue #121 (error when input does not contain columns X and Y)


----

# glatos 0.5.0

#### 2021-04-27

### New features

- functions
    - prepare_deploy_sheet
    - prepare_tag_sheet
    - convert_otn_to_att

### Minor changes

- convert_glatos_to_att, 
    - added argument `CRS` for ATT compatibility

- residence_index
    - replaced deprecated .dots with across in dplyr::group_by calls to 
      fix "Warning: The `.dots` argument of `group_by()` is deprecated as 
      of dplyr 1.0.0"

- convert_otn_erddap_to_att
    - renamed convert_otn_to_att to convert_otn_erddap_to_att to better describe what the function does


----

# glatos 0.4.2

#### 2020-04-06
   
### Minor changes

- make_transition
    - added new argument 'invert' for cases where input polygon represents land
	- fixes issue #101

### Bug fixes

- read_glatos_detections 
    - fix bug where animal_id was not created correctly if some but not all were missing
    - fixes issue #106

----

# glatos 0.4.1

#### 2020-02-18
   
### Minor changes

- make_video
    - overhauled to no longer require ffmpeg. now is a simple wrapper for
      av::av_encode_video
    - added input argument 'duration' to set total video duration in seconds
      previous version of make_video renamed 'make_video_ffmpeg'
    - calls to new 'make_video' with "old" arguments are caught and 
      redirected to 'make_video_ffmpeg' for backward compatibility

- make_frames
    - modified to call revised make_video (ffmpeg not required)
    - deprecated ffmpeg input argument
    
- interpolate_path
    - set `ties = "ordered"` for all calls to `stats::approx`
    - fixes issue #93
   
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

