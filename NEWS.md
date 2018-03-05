# glatos 0.2.4

    
### Minor changes and bug fixes

- make_video()
    - added support for input and output directories containing spaces (issue #38)
		- check in input directory exists and error/message if notation
		- create output directory if it does not exist


----------------------------------------------------
# glatos 0.2.3 

    
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

###- initial release on OTN gitlab - 2017-03-01

