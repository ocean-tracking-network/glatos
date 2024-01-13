# make_transition: Deprecation message and transition matrix for Higgins Lake water polygon as expected

    Code
      make_transition(higgins_lake_polygon, res = c(0.01, 0.01))$transition
    Condition
      Warning:
      This function is deprecated and will be removed in the next version
    Output
      class      : TransitionLayer 
      dimensions : 10, 12, 120  (nrow, ncol, ncell)
      resolution : 0.01, 0.01  (x, y)
      extent     : -84.78355, -84.66355, 44.41726, 44.51726  (xmin, xmax, ymin, ymax)
      crs        : +proj=longlat +datum=WGS84 +no_defs 
      values      : conductance 
      matrix class: dsCMatrix 

# make_transition: Deprecation message and raster values for Higgins Lake water polygon as expected

    Code
      make_transition(higgins_lake_polygon, res = c(0.01, 0.01))$rast
    Condition
      Warning:
      This function is deprecated and will be removed in the next version
    Output
      class      : RasterLayer 
      dimensions : 10, 12, 120  (nrow, ncol, ncell)
      resolution : 0.01, 0.01  (x, y)
      extent     : -84.78355, -84.66355, 44.41726, 44.51726  (xmin, xmax, ymin, ymax)
      crs        : +proj=longlat +datum=WGS84 +no_defs 
      source     : out.tif 
      names      : out 
      

# make_transition: Deprecation message and transition matrix for Flynn Island land polygon as expected

    Code
      make_transition(flynn_island_polygon, res = c(0.001, 0.001), all_touched = FALSE,
      invert = TRUE)$transition
    Condition
      Warning:
      This function is deprecated and will be removed in the next version
    Output
      class      : TransitionLayer 
      dimensions : 7, 9, 63  (nrow, ncol, ncell)
      resolution : 0.001, 0.001  (x, y)
      extent     : -84.73151, -84.72251, 44.47648, 44.48348  (xmin, xmax, ymin, ymax)
      crs        : +proj=longlat +datum=WGS84 +no_defs 
      values      : conductance 
      matrix class: dsCMatrix 

# make_transition: Deprecation message and raster values for Flynn Island polygon as expected

    Code
      make_transition(flynn_island_polygon, res = c(0.001, 0.001), all_touched = FALSE,
      invert = TRUE)$rast
    Condition
      Warning:
      This function is deprecated and will be removed in the next version
    Output
      class      : RasterLayer 
      dimensions : 7, 9, 63  (nrow, ncol, ncell)
      resolution : 0.001, 0.001  (x, y)
      extent     : -84.73151, -84.72251, 44.47648, 44.48348  (xmin, xmax, ymin, ymax)
      crs        : +proj=longlat +datum=WGS84 +no_defs 
      source     : out.tif 
      names      : out 
      

