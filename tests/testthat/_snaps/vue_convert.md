# vue_convert works

    Code
      temp_csv_files <- vue_convert(temp_vrl_files)
    Message
      Converting 4 VRL file(s) to VUE CSV...
    Output
        |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
    Message
      
      
      Done. 4 of 4 file(s) written to disk.

---

    Code
      temp_csv_files <- vue_convert(temp_vrl_files)
    Message
      Converting 0 VRL file(s) to VUE CSV...
      
      
      Done. 0 of 4 file(s) written to disk.
      
      ! 4 file(s) skipped (already exists & 'overwrite' = FALSE):
         VR2W_109924_20110718_1.csv
         VR2W180_302187_20180629_1.csv
         VR2AR_546310_20190613_1.csv
         VR2Tx_480022_20190613_1.csv
      

---

    Code
      temp_csv_files2 <- vue_convert(temp_vrl_dir, overwrite = TRUE)
    Message
      Converting 4 VRL file(s) to VUE CSV...
    Output
        |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
    Message
      
      
      Done. 4 of 4 file(s) written to disk.

---

    Code
      temp_csv_files2 <- vdat_convert(temp_vrl_dir, overwrite = TRUE)
    Message
      Converting 4 VRL/VDAT file(s) to Fathom CSV...
    Output
        |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
    Message
      
      
      Done. 4 of 4 file(s) written to disk.

