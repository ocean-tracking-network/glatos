# make_transition: Transition layer for Higgins Lake water polygon as expected

    Code
      water
    Output
      class      : TransitionLayer 
      dimensions : 9, 11, 99  (nrow, ncol, ncell)
      resolution : 0.01, 0.01  (x, y)
      extent     : -84.77855, -84.66855, 44.42226, 44.51226  (xmin, xmax, ymin, ymax)
      crs        : +proj=longlat +datum=WGS84 +no_defs 
      values      : conductance 
      matrix class: dsCMatrix 

# make_transition: Raster values for Higgins Lake water polygon as expected

    Code
      water
    Output
      class      : RasterLayer 
      dimensions : 9, 11, 99  (nrow, ncol, ncell)
      resolution : 0.01, 0.01  (x, y)
      extent     : -84.77855, -84.66855, 44.42226, 44.51226  (xmin, xmax, ymin, ymax)
      crs        : +proj=longlat +datum=WGS84 +no_defs 
      source     : memory
      names      : layer 
      values     : 0, 1  (min, max)
      

# make_transition: Transition layer for Flynn Island land polygon as expected

    Code
      land
    Output
      class      : TransitionLayer 
      dimensions : 6, 8, 48  (nrow, ncol, ncell)
      resolution : 0.001, 0.001  (x, y)
      extent     : -84.73101, -84.72301, 44.47698, 44.48298  (xmin, xmax, ymin, ymax)
      crs        : +proj=longlat +datum=WGS84 +no_defs 
      values      : conductance 
      matrix class: dsCMatrix 

# make_transition: Raster values for Flynn Island polygon as expected

    Code
      land
    Output
      class      : RasterLayer 
      dimensions : 6, 8, 48  (nrow, ncol, ncell)
      resolution : 0.001, 0.001  (x, y)
      extent     : -84.73101, -84.72301, 44.47698, 44.48298  (xmin, xmax, ymin, ymax)
      crs        : +proj=longlat +datum=WGS84 +no_defs 
      source     : memory
      names      : layer 
      values     : 0, 1  (min, max)
      

