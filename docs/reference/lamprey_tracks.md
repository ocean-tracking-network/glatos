# Sea Lamprey positions from Lake George, St. Marys River, 2012

Sea Lamprey positions from a positional acoustic telemetry array in Lake
George, North Channel of the St. Marys River during the 2012 spawning
year.

## Usage

``` r
lamprey_tracks
```

## Format

A data frame with 21043 rows and 14 variables:

- DETECTEDID:

  transmitter identifier (channel, frequency, code space, and ID code)

- DATETIME:

  position timestamp, in UTC

- X,Y:

  horizontal and vertical position on local grid, in meters

- D:

  assumed depth at time of detection, in meters (NOT from depth/pressure
  sensor)

- LAT,LON:

  position latitude and longitude, decimal degrees (west is negative);
  CRS: WGS84

- n:

  ?

- HPE:

  horizontal position error; calculated by VEMCO

- HPEm:

  horizontal position error, in meters; calculated by VEMCO

- TEMP:

  temperature at time of detection (from temperature sensor)

- DEPTH:

  depth at time of detection (from pressure sensor)

- ACCEL:

  acceleration at time of detection (from accelerometer)

- DRX:

  receivers that detected the associated transmission

## Source

Chris Holbrook, (cholbrook@glfc.org)

## Details

Data were collected as part of the GLATOS project SMRSL
<https://glatos.org/home/project/SMRSL>

Positions were calculated using the Vemco Positioning System.
