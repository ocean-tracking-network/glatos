# data.frame input gives expected result

    Code
      df_result
    Output
          animal_id detection_timestamp_utc glatos_array station_no
      129       153     2012-05-23 02:54:19          SBI          1
      130       153     2012-05-23 03:17:13          SBI          1
      131       153     2012-05-23 03:21:18          SBI          3
      132       153     2012-05-23 03:23:16          SBI          1
      133       153     2012-05-23 03:26:12          SBI          1
      134       153     2012-05-23 03:39:04          SBI          1
      135       153     2012-05-23 03:41:31          SBI          1
      136       153     2012-05-23 03:41:31          SBI          2
      137       153     2012-05-23 03:44:46          SBI          1
      138       153     2012-05-23 03:57:01          SBI          1
          transmitter_codespace transmitter_id sensor_value sensor_unit deploy_lat
      129              A69-9001          32054           NA        <NA>   44.17873
      130              A69-9001          32054           NA        <NA>   44.17873
      131              A69-9001          32054           NA        <NA>   44.17255
      132              A69-9001          32054           NA        <NA>   44.17873
      133              A69-9001          32054           NA        <NA>   44.17873
      134              A69-9001          32054           NA        <NA>   44.17873
      135              A69-9001          32054           NA        <NA>   44.17873
      136              A69-9001          32054           NA        <NA>   44.17714
      137              A69-9001          32054           NA        <NA>   44.17873
      138              A69-9001          32054           NA        <NA>   44.17873
          deploy_long receiver_sn tag_type tag_model tag_serial_number common_name_e
      129   -83.54767      109991     <NA>      <NA>              <NA>       walleye
      130   -83.54767      109991     <NA>      <NA>              <NA>       walleye
      131   -83.53090      109999     <NA>      <NA>              <NA>       walleye
      132   -83.54767      109991     <NA>      <NA>              <NA>       walleye
      133   -83.54767      109991     <NA>      <NA>              <NA>       walleye
      134   -83.54767      109991     <NA>      <NA>              <NA>       walleye
      135   -83.54767      109991     <NA>      <NA>              <NA>       walleye
      136   -83.54169      109956     <NA>      <NA>              <NA>       walleye
      137   -83.54767      109991     <NA>      <NA>              <NA>       walleye
      138   -83.54767      109991     <NA>      <NA>              <NA>       walleye
             capture_location length weight sex release_group release_location
      129 Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
      130 Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
      131 Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
      132 Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
      133 Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
      134 Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
      135 Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
      136 Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
      137 Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
      138 Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
          release_latitude release_longitude utc_release_date_time
      129               NA                NA   2012-03-20 20:00:00
      130               NA                NA   2012-03-20 20:00:00
      131               NA                NA   2012-03-20 20:00:00
      132               NA                NA   2012-03-20 20:00:00
      133               NA                NA   2012-03-20 20:00:00
      134               NA                NA   2012-03-20 20:00:00
      135               NA                NA   2012-03-20 20:00:00
      136               NA                NA   2012-03-20 20:00:00
      137               NA                NA   2012-03-20 20:00:00
      138               NA                NA   2012-03-20 20:00:00
          glatos_project_transmitter glatos_project_receiver glatos_tag_recovered
      129                      HECWL                   HECWL                   NO
      130                      HECWL                   HECWL                   NO
      131                      HECWL                   HECWL                   NO
      132                      HECWL                   HECWL                   NO
      133                      HECWL                   HECWL                   NO
      134                      HECWL                   HECWL                   NO
      135                      HECWL                   HECWL                   NO
      136                      HECWL                   HECWL                   NO
      137                      HECWL                   HECWL                   NO
      138                      HECWL                   HECWL                   NO
          glatos_caught_date station min_lag passed_filter
      129               <NA> SBI-001    1374             1
      130               <NA> SBI-001     363             1
      131               <NA> SBI-003    4180             0
      132               <NA> SBI-001     176             1
      133               <NA> SBI-001     176             1
      134               <NA> SBI-001     147             1
      135               <NA> SBI-001     147             1
      136               <NA> SBI-002    4478             0
      137               <NA> SBI-001     195             1
      138               <NA> SBI-001     735             1

# data.table input gives expected result

    Code
      dt_result
    Output
          animal_id detection_timestamp_utc glatos_array station_no
       1:       153     2012-05-23 02:54:19          SBI          1
       2:       153     2012-05-23 03:17:13          SBI          1
       3:       153     2012-05-23 03:21:18          SBI          3
       4:       153     2012-05-23 03:23:16          SBI          1
       5:       153     2012-05-23 03:26:12          SBI          1
       6:       153     2012-05-23 03:39:04          SBI          1
       7:       153     2012-05-23 03:41:31          SBI          1
       8:       153     2012-05-23 03:41:31          SBI          2
       9:       153     2012-05-23 03:44:46          SBI          1
      10:       153     2012-05-23 03:57:01          SBI          1
          transmitter_codespace transmitter_id sensor_value sensor_unit deploy_lat
       1:              A69-9001          32054           NA        <NA>   44.17873
       2:              A69-9001          32054           NA        <NA>   44.17873
       3:              A69-9001          32054           NA        <NA>   44.17255
       4:              A69-9001          32054           NA        <NA>   44.17873
       5:              A69-9001          32054           NA        <NA>   44.17873
       6:              A69-9001          32054           NA        <NA>   44.17873
       7:              A69-9001          32054           NA        <NA>   44.17873
       8:              A69-9001          32054           NA        <NA>   44.17714
       9:              A69-9001          32054           NA        <NA>   44.17873
      10:              A69-9001          32054           NA        <NA>   44.17873
          deploy_long receiver_sn tag_type tag_model tag_serial_number common_name_e
       1:   -83.54767      109991     <NA>      <NA>              <NA>       walleye
       2:   -83.54767      109991     <NA>      <NA>              <NA>       walleye
       3:   -83.53090      109999     <NA>      <NA>              <NA>       walleye
       4:   -83.54767      109991     <NA>      <NA>              <NA>       walleye
       5:   -83.54767      109991     <NA>      <NA>              <NA>       walleye
       6:   -83.54767      109991     <NA>      <NA>              <NA>       walleye
       7:   -83.54767      109991     <NA>      <NA>              <NA>       walleye
       8:   -83.54169      109956     <NA>      <NA>              <NA>       walleye
       9:   -83.54767      109991     <NA>      <NA>              <NA>       walleye
      10:   -83.54767      109991     <NA>      <NA>              <NA>       walleye
             capture_location length weight sex release_group release_location
       1: Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
       2: Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
       3: Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
       4: Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
       5: Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
       6: Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
       7: Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
       8: Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
       9: Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
      10: Tittabawassee River  0.565     NA   F          <NA>    Tittabawassee
          release_latitude release_longitude utc_release_date_time
       1:               NA                NA   2012-03-20 20:00:00
       2:               NA                NA   2012-03-20 20:00:00
       3:               NA                NA   2012-03-20 20:00:00
       4:               NA                NA   2012-03-20 20:00:00
       5:               NA                NA   2012-03-20 20:00:00
       6:               NA                NA   2012-03-20 20:00:00
       7:               NA                NA   2012-03-20 20:00:00
       8:               NA                NA   2012-03-20 20:00:00
       9:               NA                NA   2012-03-20 20:00:00
      10:               NA                NA   2012-03-20 20:00:00
          glatos_project_transmitter glatos_project_receiver glatos_tag_recovered
       1:                      HECWL                   HECWL                   NO
       2:                      HECWL                   HECWL                   NO
       3:                      HECWL                   HECWL                   NO
       4:                      HECWL                   HECWL                   NO
       5:                      HECWL                   HECWL                   NO
       6:                      HECWL                   HECWL                   NO
       7:                      HECWL                   HECWL                   NO
       8:                      HECWL                   HECWL                   NO
       9:                      HECWL                   HECWL                   NO
      10:                      HECWL                   HECWL                   NO
          glatos_caught_date station min_lag passed_filter
       1:               <NA> SBI-001    1374             1
       2:               <NA> SBI-001     363             1
       3:               <NA> SBI-003    4180             0
       4:               <NA> SBI-001     176             1
       5:               <NA> SBI-001     176             1
       6:               <NA> SBI-001     147             1
       7:               <NA> SBI-001     147             1
       8:               <NA> SBI-002    4478             0
       9:               <NA> SBI-001     195             1
      10:               <NA> SBI-001     735             1

# tibble input gives expected result

    Code
      tbl_result
    Output
      # A tibble: 10 x 31
         animal_id detection_timestamp_utc glatos_array station_no
         <chr>     <dttm>                  <chr>        <chr>     
       1 153       2012-05-23 02:54:19     SBI          1         
       2 153       2012-05-23 03:17:13     SBI          1         
       3 153       2012-05-23 03:21:18     SBI          3         
       4 153       2012-05-23 03:23:16     SBI          1         
       5 153       2012-05-23 03:26:12     SBI          1         
       6 153       2012-05-23 03:39:04     SBI          1         
       7 153       2012-05-23 03:41:31     SBI          1         
       8 153       2012-05-23 03:41:31     SBI          2         
       9 153       2012-05-23 03:44:46     SBI          1         
      10 153       2012-05-23 03:57:01     SBI          1         
      # i 27 more variables: transmitter_codespace <chr>, transmitter_id <chr>,
      #   sensor_value <dbl>, sensor_unit <chr>, deploy_lat <dbl>, deploy_long <dbl>,
      #   receiver_sn <chr>, tag_type <chr>, tag_model <chr>,
      #   tag_serial_number <chr>, common_name_e <chr>, capture_location <chr>,
      #   length <dbl>, weight <dbl>, sex <chr>, release_group <chr>,
      #   release_location <chr>, release_latitude <dbl>, release_longitude <dbl>,
      #   utc_release_date_time <dttm>, glatos_project_transmitter <chr>, ...

