# data.frame input, spatial output gives expected result

    Code
      detect_transmissions(trnsLoc = tr_df, recLoc = recs_df, detRngFun = function(x)
        0.5, inputCRS = sf::st_crs(tr_sf), show_progress = FALSE)
    Output
      Simple feature collection with 8 features and 3 fields
      Active geometry column: rec_geometry
      Geometry type: POINT
      Dimension:     XY
      Bounding box:  xmin: -87.65 ymin: 48.6 xmax: -87.65 ymax: 48.6
      Geodetic CRS:  WGS 84 (with axis order normalized for visualization)
        trns_id rec_id      time        rec_geometry              trns_geometry
      1       1      4  600.8733 POINT (-87.65 48.6) POINT (-87.51364 48.44519)
      2       1      4 1791.7514 POINT (-87.65 48.6) POINT (-87.56015 48.48889)
      3       1      4 2833.5955 POINT (-87.65 48.6) POINT (-87.60753 48.52348)
      4       1      4 3943.3385 POINT (-87.65 48.6) POINT (-87.65978 48.55936)
      5       1      4 4909.4957 POINT (-87.65 48.6)  POINT (-87.7143 48.58339)
      6       1      4 5691.6579 POINT (-87.65 48.6) POINT (-87.75888 48.60248)
      7       1      4 7374.9478 POINT (-87.65 48.6) POINT (-87.81634 48.66588)
      8       1      4 8248.2156 POINT (-87.65 48.6) POINT (-87.85012 48.69558)

# data.frame input, data.frame output gives expected result

    Code
      detect_transmissions(trnsLoc = tr_df, recLoc = recs_df, detRngFun = function(x)
        0.5, inputCRS = 4326, sp_out = FALSE, show_progress = FALSE)
    Output
        trns_id rec_id  rec_x rec_y    trns_x   trns_y      time
      1       1      4 -87.65  48.6 -87.51364 48.44519  600.8733
      2       1      4 -87.65  48.6 -87.56015 48.48889 1791.7514
      3       1      4 -87.65  48.6 -87.60753 48.52348 2833.5955
      4       1      4 -87.65  48.6 -87.65978 48.55936 3943.3385
      5       1      4 -87.65  48.6 -87.71430 48.58339 4909.4957
      6       1      4 -87.65  48.6 -87.75888 48.60248 5691.6579
      7       1      4 -87.65  48.6 -87.81634 48.66588 7374.9478
      8       1      4 -87.65  48.6 -87.85012 48.69558 8248.2156

# spatial input, data.frame output gives expected result

    Code
      detect_transmissions(trnsLoc = tr_sf, recLoc = recs_sf, detRngFun = function(x)
        0.5, sp_out = FALSE, show_progress = FALSE)
    Output
        trns_id rec_id  rec_x rec_y    trns_x   trns_y      time
      1       1      4 -87.65  48.6 -87.51364 48.44519  600.8733
      2       1      4 -87.65  48.6 -87.56015 48.48889 1791.7514
      3       1      4 -87.65  48.6 -87.60753 48.52348 2833.5955
      4       1      4 -87.65  48.6 -87.65978 48.55936 3943.3385
      5       1      4 -87.65  48.6 -87.71430 48.58339 4909.4957
      6       1      4 -87.65  48.6 -87.75888 48.60248 5691.6579
      7       1      4 -87.65  48.6 -87.81634 48.66588 7374.9478
      8       1      4 -87.65  48.6 -87.85012 48.69558 8248.2156

# spatial input, spatial output gives expected result

    Code
      detect_transmissions(trnsLoc = tr_sf, recLoc = recs_sf, detRngFun = function(x)
        0.5, show_progress = FALSE)
    Output
      Simple feature collection with 8 features and 3 fields
      Active geometry column: rec_geometry
      Geometry type: POINT
      Dimension:     XY
      Bounding box:  xmin: -87.65 ymin: 48.6 xmax: -87.65 ymax: 48.6
      Geodetic CRS:  WGS 84 (with axis order normalized for visualization)
        trns_id rec_id      time        rec_geometry              trns_geometry
      1       1      4  600.8733 POINT (-87.65 48.6) POINT (-87.51364 48.44519)
      2       1      4 1791.7514 POINT (-87.65 48.6) POINT (-87.56015 48.48889)
      3       1      4 2833.5955 POINT (-87.65 48.6) POINT (-87.60753 48.52348)
      4       1      4 3943.3385 POINT (-87.65 48.6) POINT (-87.65978 48.55936)
      5       1      4 4909.4957 POINT (-87.65 48.6)  POINT (-87.7143 48.58339)
      6       1      4 5691.6579 POINT (-87.65 48.6) POINT (-87.75888 48.60248)
      7       1      4 7374.9478 POINT (-87.65 48.6) POINT (-87.81634 48.66588)
      8       1      4 8248.2156 POINT (-87.65 48.6) POINT (-87.85012 48.69558)

