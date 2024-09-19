# real_sensor_values works

    Code
      real_sensor_values(dtc, lamprey_tag_specs$specs)
    Output
        transmitter_codespace transmitter_id     animal_id detection_timestamp_utc
      1              A69-1601           1363 A69-1601-1363     2012-05-10 06:55:11
      2              A69-1601           1363 A69-1601-1363     2012-05-10 06:55:14
      3              A69-1601           1362 A69-1601-1362     2012-05-12 10:14:39
      4              A69-1601           1362 A69-1601-1362     2012-05-12 10:21:05
      5              A69-9002           7189 A69-9002-7189     2012-05-19 00:24:57
      6              A69-9002           7189 A69-9002-7189     2012-05-19 00:27:22
        glatos_array station_no sensor_value sensor_unit deploy_lat deploy_long
      1          DCK          3           NA        <NA>   46.36026   -84.12942
      2          DCK          2           NA        <NA>   46.35926   -84.13213
      3          DCK          2           NA        <NA>   46.35926   -84.13213
      4          DCK          2           NA        <NA>   46.35926   -84.13213
      5          DCK          4           66         ADC   46.36181   -84.13075
      6          DCK          4           62         ADC   46.36181   -84.13075
        receiver_sn tag_type tag_model tag_serial_number common_name_e
      1      109431 acoustic     V9-2H              <NA>   sea lamprey
      2      109471 acoustic     V9-2H              <NA>   sea lamprey
      3      109471 acoustic     V9-2H              <NA>   sea lamprey
      4      109471 acoustic     V9-2H              <NA>   sea lamprey
      5      109877 acoustic    V9P-2H              <NA>   sea lamprey
      6      109877 acoustic    V9P-2H              <NA>   sea lamprey
        capture_location length weight sex release_group release_location
      1  Cheboygan River   0.50  0.229   M        NCH-01    North Channel
      2  Cheboygan River   0.50  0.229   M        NCH-01    North Channel
      3  Cheboygan River   0.46  0.252   M        NCH-01    North Channel
      4  Cheboygan River   0.46  0.252   M        NCH-01    North Channel
      5 Manistique River   0.53  0.316   M        NCH-02    North Channel
      6 Manistique River   0.53  0.316   M        NCH-02    North Channel
        release_latitude release_longitude utc_release_date_time
      1         46.30641         -84.06129   2012-05-09 22:20:00
      2         46.30641         -84.06129   2012-05-09 22:20:00
      3         46.30641         -84.06129   2012-05-09 22:20:00
      4         46.30641         -84.06129   2012-05-09 22:20:00
      5         46.30628         -84.06122   2012-05-09 22:20:00
      6         46.30628         -84.06122   2012-05-09 22:20:00
        glatos_project_transmitter glatos_project_receiver glatos_tag_recovered
      1                      SMRSL                   SMRSL                   No
      2                      SMRSL                   SMRSL                   No
      3                      SMRSL                   SMRSL                   No
      4                      SMRSL                   SMRSL                   No
      5                      SMRSL                   SMRSL                  Yes
      6                      SMRSL                   SMRSL                  Yes
        glatos_caught_date station min_lag ord sensor_range sensor_units sensor_slope
      1               <NA> DCK-003     241   1         <NA>         <NA>           NA
      2               <NA> DCK-002     242   2         <NA>         <NA>           NA
      3               <NA> DCK-002     386   3         <NA>         <NA>           NA
      4               <NA> DCK-002      49   4         <NA>         <NA>           NA
      5         2012-07-04 DCK-004     145   5           50       Meters       0.2198
      6         2012-07-04 DCK-004     145   6           50       Meters       0.2198
        sensor_intercept accel_algorithm accel_sample_rate sensor_transmit_ratio
      1               NA            <NA>                NA                    NA
      2               NA            <NA>                NA                    NA
      3               NA            <NA>                NA                    NA
      4               NA            <NA>                NA                    NA
      5          -0.8794            <NA>                NA                    NA
      6          -0.8794            <NA>                NA                    NA
        sensor_value_real
      1                NA
      2                NA
      3                NA
      4                NA
      5           13.6274
      6           12.7482

