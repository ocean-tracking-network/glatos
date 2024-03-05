# data.frame input, sf output gives expected result

    Code
      dfin_sfout
    Output
      Simple feature collection with 6 features and 0 fields
      Geometry type: POINT
      Dimension:     XY
      Bounding box:  xmin: -19.63178 ymin: 0 xmax: 0 ymax: 43.70673
      CRS:           NA
                          geometry
      1                POINT (0 0)
      2 POINT (-4.347654 9.005438)
      3 POINT (-9.753604 17.41827)
      4 POINT (-16.59357 24.71312)
      5 POINT (-19.63178 34.24041)
      6  POINT (-16.4086 43.70673)

# data.frame input, data.frame output gives expected result

    Code
      dfin_dfout
    Output
                 x         y
      1   0.000000  0.000000
      2  -4.347654  9.005438
      3  -9.753604 17.418267
      4 -16.593567 24.713122
      5 -19.631781 34.240412
      6 -16.408604 43.706727

# SpatialPolygonsDataFrame input, data.frame output gives expected result

    Code
      spin_dfout
    Output
                x        y
      1 -87.49017 48.42314
      2 -87.56828 48.49653
      3 -87.66241 48.56116
      4 -87.77630 48.60994
      5 -87.83445 48.69117
      6 -87.95955 48.72632

# SpatialPolygonsDataFrame input, sf output gives expected result

    Code
      spin_sfout
    Output
      Simple feature collection with 6 features and 0 fields
      Geometry type: POINT
      Dimension:     XY
      Bounding box:  xmin: -87.95955 ymin: 48.42314 xmax: -87.49017 ymax: 48.72632
      Geodetic CRS:  WGS 84 (with axis order normalized for visualization)
                          geometry
      1 POINT (-87.49017 48.42314)
      2 POINT (-87.56828 48.49653)
      3 POINT (-87.66241 48.56116)
      4  POINT (-87.7763 48.60994)
      5 POINT (-87.83445 48.69117)
      6 POINT (-87.95955 48.72632)

# sf input, data.frame output gives expected result

    Code
      sfin_dfout
    Output
                x        y
      1 -87.49017 48.42314
      2 -87.56828 48.49653
      3 -87.66241 48.56116
      4 -87.77630 48.60994
      5 -87.83445 48.69117
      6 -87.95955 48.72632

# sf input, sf output gives expected result

    Code
      sfin_sfout
    Output
      Simple feature collection with 6 features and 0 fields
      Geometry type: POINT
      Dimension:     XY
      Bounding box:  xmin: -87.95955 ymin: 48.42314 xmax: -87.49017 ymax: 48.72632
      Geodetic CRS:  WGS 84
                          geometry
      1 POINT (-87.49017 48.42314)
      2 POINT (-87.56828 48.49653)
      3 POINT (-87.66241 48.56116)
      4  POINT (-87.7763 48.60994)
      5 POINT (-87.83445 48.69117)
      6 POINT (-87.95955 48.72632)

