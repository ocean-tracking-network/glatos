# one vrl gives expected result

    Code
      readLines(good_csv, n = 10)
    Output
       [1] "Date and Time (UTC),Receiver,Transmitter,Transmitter Name,Transmitter Serial,Sensor Value,Sensor Unit,Station Name,Latitude,Longitude,Transmitter Type,Sensor Precision"
       [2] "2011-04-11 20:17:49,VR2W-109924,A69-1303-63366,,,,,,+0,+0"                                                                                                              
       [3] "2011-05-08 05:38:32,VR2W-109924,A69-9002-4043,,,5,ADC,,+0,+0"                                                                                                           
       [4] "2011-05-08 05:41:09,VR2W-109924,A69-9002-4043,,,7,ADC,,+0,+0"                                                                                                           
       [5] "2011-05-08 05:43:14,VR2W-109924,A69-9002-4043,,,4,ADC,,+0,+0"                                                                                                           
       [6] "2011-05-08 05:44:15,VR2W-109924,A69-9002-4043,,,5,ADC,,+0,+0"                                                                                                           
       [7] "2011-05-08 05:45:59,VR2W-109924,A69-9002-4043,,,16,ADC,,+0,+0"                                                                                                          
       [8] "2011-05-08 05:46:36,VR2W-109924,A69-9002-4043,,,5,ADC,,+0,+0"                                                                                                           
       [9] "2011-05-08 05:48:07,VR2W-109924,A69-9002-4043,,,6,ADC,,+0,+0"                                                                                                           
      [10] "2011-05-08 05:48:31,VR2W-109924,A69-9002-4043,,,4,ADC,,+0,+0"                                                                                                           

# one vrl in dir with space in name gives expected result

    Code
      readLines(good_csv, n = 10)
    Output
       [1] "Date and Time (UTC),Receiver,Transmitter,Transmitter Name,Transmitter Serial,Sensor Value,Sensor Unit,Station Name,Latitude,Longitude,Transmitter Type,Sensor Precision"
       [2] "2011-04-11 20:17:49,VR2W-109924,A69-1303-63366,,,,,,+0,+0"                                                                                                              
       [3] "2011-05-08 05:38:32,VR2W-109924,A69-9002-4043,,,5,ADC,,+0,+0"                                                                                                           
       [4] "2011-05-08 05:41:09,VR2W-109924,A69-9002-4043,,,7,ADC,,+0,+0"                                                                                                           
       [5] "2011-05-08 05:43:14,VR2W-109924,A69-9002-4043,,,4,ADC,,+0,+0"                                                                                                           
       [6] "2011-05-08 05:44:15,VR2W-109924,A69-9002-4043,,,5,ADC,,+0,+0"                                                                                                           
       [7] "2011-05-08 05:45:59,VR2W-109924,A69-9002-4043,,,16,ADC,,+0,+0"                                                                                                          
       [8] "2011-05-08 05:46:36,VR2W-109924,A69-9002-4043,,,5,ADC,,+0,+0"                                                                                                           
       [9] "2011-05-08 05:48:07,VR2W-109924,A69-9002-4043,,,6,ADC,,+0,+0"                                                                                                           
      [10] "2011-05-08 05:48:31,VR2W-109924,A69-9002-4043,,,4,ADC,,+0,+0"                                                                                                           

# one good vrl in dir with corrupt vrl gives expected result

    Code
      readLines(out_csv, n = 10)
    Output
       [1] "Date and Time (UTC),Receiver,Transmitter,Transmitter Name,Transmitter Serial,Sensor Value,Sensor Unit,Station Name,Latitude,Longitude,Transmitter Type,Sensor Precision"
       [2] "2011-04-11 20:17:49,VR2W-109924,A69-1303-63366,,,,,,+0,+0"                                                                                                              
       [3] "2011-05-08 05:38:32,VR2W-109924,A69-9002-4043,,,5,ADC,,+0,+0"                                                                                                           
       [4] "2011-05-08 05:41:09,VR2W-109924,A69-9002-4043,,,7,ADC,,+0,+0"                                                                                                           
       [5] "2011-05-08 05:43:14,VR2W-109924,A69-9002-4043,,,4,ADC,,+0,+0"                                                                                                           
       [6] "2011-05-08 05:44:15,VR2W-109924,A69-9002-4043,,,5,ADC,,+0,+0"                                                                                                           
       [7] "2011-05-08 05:45:59,VR2W-109924,A69-9002-4043,,,16,ADC,,+0,+0"                                                                                                          
       [8] "2011-05-08 05:46:36,VR2W-109924,A69-9002-4043,,,5,ADC,,+0,+0"                                                                                                           
       [9] "2011-05-08 05:48:07,VR2W-109924,A69-9002-4043,,,6,ADC,,+0,+0"                                                                                                           
      [10] "2011-05-08 05:48:31,VR2W-109924,A69-9002-4043,,,4,ADC,,+0,+0"                                                                                                           

