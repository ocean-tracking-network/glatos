# linear interpolation works

    Code
      linear_interp
    Output
         animal_id bin_timestamp latitude longitude  record_type
      1          1    2000-01-01 44.00000 -87.00000    detection
      2          1    2000-01-02 44.01613 -86.85484 interpolated
      3          1    2000-01-03 44.03226 -86.70968 interpolated
      4          1    2000-01-04 44.04839 -86.56452 interpolated
      5          1    2000-01-05 44.06452 -86.41935 interpolated
      6          1    2000-01-06 44.08065 -86.27419 interpolated
      7          1    2000-01-07 44.09677 -86.12903 interpolated
      8          1    2000-01-08 44.11290 -85.98387 interpolated
      9          1    2000-01-09 44.12903 -85.83871 interpolated
      10         1    2000-01-10 44.14516 -85.69355 interpolated
      11         1    2000-01-11 44.16129 -85.54839 interpolated
      12         1    2000-01-12 44.17742 -85.40323 interpolated
      13         1    2000-01-13 44.19355 -85.25806 interpolated
      14         1    2000-01-14 44.20968 -85.11290 interpolated
      15         1    2000-01-15 44.22581 -84.96774 interpolated
      16         1    2000-01-16 44.24194 -84.82258 interpolated
      17         1    2000-01-17 44.25806 -84.67742 interpolated
      18         1    2000-01-18 44.27419 -84.53226 interpolated
      19         1    2000-01-19 44.29032 -84.38710 interpolated
      20         1    2000-01-20 44.30645 -84.24194 interpolated
      21         1    2000-01-21 44.32258 -84.09677 interpolated
      22         1    2000-01-22 44.33871 -83.95161 interpolated
      23         1    2000-01-23 44.35484 -83.80645 interpolated
      24         1    2000-01-24 44.37097 -83.66129 interpolated
      25         1    2000-01-25 44.38710 -83.51613 interpolated
      26         1    2000-01-26 44.40323 -83.37097 interpolated
      27         1    2000-01-27 44.41935 -83.22581 interpolated
      28         1    2000-01-28 44.43548 -83.08065 interpolated
      29         1    2000-01-29 44.45161 -82.93548 interpolated
      30         1    2000-01-30 44.46774 -82.79032 interpolated
      31         1    2000-01-31 44.48387 -82.64516 interpolated
      32         1    2000-02-01 44.50000 -82.50000    detection
      33         1    2000-02-02 44.46552 -82.34483 interpolated
      34         1    2000-02-03 44.43103 -82.18966 interpolated
      35         1    2000-02-04 44.39655 -82.03448 interpolated
      36         1    2000-02-05 44.36207 -81.87931 interpolated
      37         1    2000-02-06 44.32759 -81.72414 interpolated
      38         1    2000-02-07 44.29310 -81.56897 interpolated
      39         1    2000-02-08 44.25862 -81.41379 interpolated
      40         1    2000-02-09 44.22414 -81.25862 interpolated
      41         1    2000-02-10 44.18966 -81.10345 interpolated
      42         1    2000-02-11 44.15517 -80.94828 interpolated
      43         1    2000-02-12 44.12069 -80.79310 interpolated
      44         1    2000-02-13 44.08621 -80.63793 interpolated
      45         1    2000-02-14 44.05172 -80.48276 interpolated
      46         1    2000-02-15 44.01724 -80.32759 interpolated
      47         1    2000-02-16 43.98276 -80.17241 interpolated
      48         1    2000-02-17 43.94828 -80.01724 interpolated
      49         1    2000-02-18 43.91379 -79.86207 interpolated
      50         1    2000-02-19 43.87931 -79.70690 interpolated
      51         1    2000-02-20 43.84483 -79.55172 interpolated
      52         1    2000-02-21 43.81034 -79.39655 interpolated
      53         1    2000-02-22 43.77586 -79.24138 interpolated
      54         1    2000-02-23 43.74138 -79.08621 interpolated
      55         1    2000-02-24 43.70690 -78.93103 interpolated
      56         1    2000-02-25 43.67241 -78.77586 interpolated
      57         1    2000-02-26 43.63793 -78.62069 interpolated
      58         1    2000-02-27 43.60345 -78.46552 interpolated
      59         1    2000-02-28 43.56897 -78.31034 interpolated
      60         1    2000-02-29 43.53448 -78.15517 interpolated
      61         1    2000-03-01 43.50000 -78.00000    detection

# Non-linear interpolation works

    Code
      nonlinear_interp
    Output
         animal_id bin_timestamp latitude longitude  record_type
      1          1    2000-01-01 44.00000 -87.00000    detection
      2          1    2000-01-02 44.14456 -87.01280 interpolated
      3          1    2000-01-03 44.28736 -86.99353 interpolated
      4          1    2000-01-04 44.39165 -86.84604 interpolated
      5          1    2000-01-05 44.49603 -86.69840 interpolated
      6          1    2000-01-06 44.60050 -86.55064 interpolated
      7          1    2000-01-07 44.70507 -86.40275 interpolated
      8          1    2000-01-08 44.80973 -86.25472 interpolated
      9          1    2000-01-09 44.91448 -86.10656 interpolated
      10         1    2000-01-10 45.01933 -85.95826 interpolated
      11         1    2000-01-11 45.12427 -85.80983 interpolated
      12         1    2000-01-12 45.22932 -85.66127 interpolated
      13         1    2000-01-13 45.33445 -85.51257 interpolated
      14         1    2000-01-14 45.43968 -85.36373 interpolated
      15         1    2000-01-15 45.54501 -85.21476 interpolated
      16         1    2000-01-16 45.65044 -85.06565 interpolated
      17         1    2000-01-17 45.74961 -84.91262 interpolated
      18         1    2000-01-18 45.75648 -84.70329 interpolated
      19         1    2000-01-19 45.72012 -84.49758 interpolated
      20         1    2000-01-20 45.65750 -84.30603 interpolated
      21         1    2000-01-21 45.59065 -84.11694 interpolated
      22         1    2000-01-22 45.52387 -83.92803 interpolated
      23         1    2000-01-23 45.45715 -83.73930 interpolated
      24         1    2000-01-24 45.37617 -83.56547 interpolated
      25         1    2000-01-25 45.27099 -83.41672 interpolated
      26         1    2000-01-26 45.16591 -83.26810 interpolated
      27         1    2000-01-27 45.06093 -83.11961 interpolated
      28         1    2000-01-28 44.95605 -82.97127 interpolated
      29         1    2000-01-29 44.85125 -82.82305 interpolated
      30         1    2000-01-30 44.74656 -82.67497 interpolated
      31         1    2000-01-31 44.64195 -82.52702 interpolated
      32         1    2000-02-01 44.50000 -82.50000    detection
      33         1    2000-02-02 44.27598 -82.41394 interpolated
      34         1    2000-02-03 44.02721 -82.41394 interpolated
      35         1    2000-02-04 43.77843 -82.41394 interpolated
      36         1    2000-02-05 43.52964 -82.41394 interpolated
      37         1    2000-02-06 43.28084 -82.41394 interpolated
      38         1    2000-02-07 43.03202 -82.41394 interpolated
      39         1    2000-02-08 42.80359 -82.47917 interpolated
      40         1    2000-02-09 42.60977 -82.59843 interpolated
      41         1    2000-02-10 42.43783 -82.84162 interpolated
      42         1    2000-02-11 42.27309 -83.09093 interpolated
      43         1    2000-02-12 42.03710 -83.06659 interpolated
      44         1    2000-02-13 41.99075 -82.75061 interpolated
      45         1    2000-02-14 42.02156 -82.42462 interpolated
      46         1    2000-02-15 42.12825 -82.12284 interpolated
      47         1    2000-02-16 42.23508 -81.82064 interpolated
      48         1    2000-02-17 42.34205 -81.51804 interpolated
      49         1    2000-02-18 42.43982 -81.21055 interpolated
      50         1    2000-02-19 42.49761 -80.88362 interpolated
      51         1    2000-02-20 42.55545 -80.55641 interpolated
      52         1    2000-02-21 42.64289 -80.24305 interpolated
      53         1    2000-02-22 42.71383 -79.92129 interpolated
      54         1    2000-02-23 42.77186 -79.59301 interpolated
      55         1    2000-02-24 42.82994 -79.26443 interpolated
      56         1    2000-02-25 42.88807 -78.93556 interpolated
      57         1    2000-02-26 43.07781 -79.02641 interpolated
      58         1    2000-02-27 43.28679 -78.97607 interpolated
      59         1    2000-02-28 43.38565 -78.66456 interpolated
      60         1    2000-02-29 43.44428 -78.33286 interpolated
      61         1    2000-03-01 43.50000 -78.00000    detection

