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
      2          1    2000-01-02 44.14787 -86.99649 interpolated
      3          1    2000-01-03 44.28705 -86.96679 interpolated
      4          1    2000-01-04 44.39100 -86.81977 interpolated
      5          1    2000-01-05 44.49504 -86.67262 interpolated
      6          1    2000-01-06 44.59917 -86.52534 interpolated
      7          1    2000-01-07 44.70340 -86.37792 interpolated
      8          1    2000-01-08 44.80772 -86.23037 interpolated
      9          1    2000-01-09 44.91213 -86.08270 interpolated
      10         1    2000-01-10 45.01664 -85.93488 interpolated
      11         1    2000-01-11 45.12125 -85.78694 interpolated
      12         1    2000-01-12 45.22594 -85.63885 interpolated
      13         1    2000-01-13 45.33074 -85.49064 interpolated
      14         1    2000-01-14 45.43562 -85.34229 interpolated
      15         1    2000-01-15 45.54061 -85.19380 interpolated
      16         1    2000-01-16 45.64569 -85.04518 interpolated
      17         1    2000-01-17 45.74501 -84.89164 interpolated
      18         1    2000-01-18 45.75989 -84.68456 interpolated
      19         1    2000-01-19 45.71797 -84.48253 interpolated
      20         1    2000-01-20 45.65129 -84.29389 interpolated
      21         1    2000-01-21 45.58466 -84.10543 interpolated
      22         1    2000-01-22 45.51810 -83.91715 interpolated
      23         1    2000-01-23 45.45160 -83.72905 interpolated
      24         1    2000-01-24 45.37220 -83.55443 interpolated
      25         1    2000-01-25 45.26737 -83.40616 interpolated
      26         1    2000-01-26 45.16264 -83.25803 interpolated
      27         1    2000-01-27 45.05800 -83.11003 interpolated
      28         1    2000-01-28 44.95345 -82.96216 interpolated
      29         1    2000-01-29 44.84900 -82.81443 interpolated
      30         1    2000-01-30 44.74464 -82.66683 interpolated
      31         1    2000-01-31 44.64038 -82.51936 interpolated
      32         1    2000-02-01 44.50000 -82.50000    detection
      33         1    2000-02-02 44.27251 -82.43024 interpolated
      34         1    2000-02-03 44.02417 -82.43024 interpolated
      35         1    2000-02-04 43.77583 -82.43024 interpolated
      36         1    2000-02-05 43.52747 -82.43024 interpolated
      37         1    2000-02-06 43.27910 -82.43024 interpolated
      38         1    2000-02-07 43.03073 -82.43024 interpolated
      39         1    2000-02-08 42.79253 -82.46286 interpolated
      40         1    2000-02-09 42.59897 -82.61234 interpolated
      41         1    2000-02-10 42.43023 -82.85780 interpolated
      42         1    2000-02-11 42.26580 -83.10667 interpolated
      43         1    2000-02-12 42.03635 -83.07023 interpolated
      44         1    2000-02-13 42.00612 -82.75519 interpolated
      45         1    2000-02-14 42.04052 -82.43078 interpolated
      46         1    2000-02-15 42.14704 -82.12946 interpolated
      47         1    2000-02-16 42.25371 -81.82772 interpolated
      48         1    2000-02-17 42.36052 -81.52557 interpolated
      49         1    2000-02-18 42.44614 -81.21284 interpolated
      50         1    2000-02-19 42.50383 -80.88646 interpolated
      51         1    2000-02-20 42.56157 -80.55979 interpolated
      52         1    2000-02-21 42.65079 -80.24786 interpolated
      53         1    2000-02-22 42.70913 -79.92069 interpolated
      54         1    2000-02-23 42.76705 -79.59301 interpolated
      55         1    2000-02-24 42.82502 -79.26503 interpolated
      56         1    2000-02-25 42.88428 -78.93768 interpolated
      57         1    2000-02-26 43.08129 -79.04221 interpolated
      58         1    2000-02-27 43.28581 -78.97342 interpolated
      59         1    2000-02-28 43.38678 -78.66357 interpolated
      60         1    2000-02-29 43.44531 -78.33246 interpolated
      61         1    2000-03-01 43.50000 -78.00000    detection

