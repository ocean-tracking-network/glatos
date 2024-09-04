# data.frame input, spatial output gives expected result

    Code
      tr_dfin_spout
    Output
      Simple feature collection with 8 features and 1 field
      Geometry type: POINT
      Dimension:     XY
      Bounding box:  xmin: -87.85012 ymin: 48.44519 xmax: -87.51364 ymax: 48.69558
      Geodetic CRS:  WGS 84 (with axis order normalized for visualization)
             time                   geometry
      1  600.8733 POINT (-87.51364 48.44519)
      2 1791.7514 POINT (-87.56015 48.48889)
      3 2833.5955 POINT (-87.60753 48.52348)
      4 3943.3385 POINT (-87.65978 48.55936)
      5 4909.4957  POINT (-87.7143 48.58339)
      6 5691.6579 POINT (-87.75888 48.60248)
      7 7374.9478 POINT (-87.81634 48.66588)
      8 8248.2156 POINT (-87.85012 48.69558)

# data.frame input, data.frame output gives expected result

    Code
      tr_dfin_dfout
    Output
                x        y      time
      1 -87.51364 48.44519  600.8733
      2 -87.56015 48.48889 1791.7514
      3 -87.60753 48.52348 2833.5955
      4 -87.65978 48.55936 3943.3385
      5 -87.71430 48.58339 4909.4957
      6 -87.75888 48.60248 5691.6579
      7 -87.81634 48.66588 7374.9478
      8 -87.85012 48.69558 8248.2156

# spatial input, data.frame output gives expected result

    Code
      tr_spin_dfout
    Output
                x        y      time
      1 -87.51364 48.44519  600.8733
      2 -87.56015 48.48889 1791.7514
      3 -87.60753 48.52348 2833.5955
      4 -87.65978 48.55936 3943.3385
      5 -87.71430 48.58339 4909.4957
      6 -87.75888 48.60248 5691.6579
      7 -87.81634 48.66588 7374.9478
      8 -87.85012 48.69558 8248.2156

# spatial input, spatial output gives expected result

    Code
      tr_spin_spout
    Output
      Simple feature collection with 8 features and 1 field
      Geometry type: POINT
      Dimension:     XY
      Bounding box:  xmin: -87.85012 ymin: 48.44519 xmax: -87.51364 ymax: 48.69558
      Geodetic CRS:  WGS 84 (with axis order normalized for visualization)
             time                   geometry
      1  600.8733 POINT (-87.51364 48.44519)
      2 1791.7514 POINT (-87.56015 48.48889)
      3 2833.5955 POINT (-87.60753 48.52348)
      4 3943.3385 POINT (-87.65978 48.55936)
      5 4909.4957  POINT (-87.7143 48.58339)
      6 5691.6579 POINT (-87.75888 48.60248)
      7 7374.9478 POINT (-87.81634 48.66588)
      8 8248.2156 POINT (-87.85012 48.69558)

