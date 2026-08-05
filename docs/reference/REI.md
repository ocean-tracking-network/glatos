# Calculates a returns a list of each station and the REI (defined here)

The receiver efficiency index is number between 0 and 1 indicating the
amount of relative activity at each receiver compared to the entire set
of receivers, regardless of positioning. The function takes a set
detections and a deployment history of the receivers to create a context
for the detections. Both the amount of unique tags and number of species
are taken into consideration in the calculation.

*(Ellis, R., Flaherty-Walia, K., Collins, A., Bickford, J., Walters
Burnsed, Lowerre-Barbieri S. 2018. Acoustic telemetry array evolution:
from species- and project-specific designs to large-scale, multispecies,
cooperative networks, <https://doi.org/10.1016/j.fishres.2018.09.015>)*

REI() takes two arguments. The first is a dataframe of detections the
detection timstamp, the station identifier, the species, and the tag
identifier. The next is a dataframe of deployments for each station. The
station name should match the stations in the detections. The
deployments need to include a deployment date and recovery date.

\$\$ REI = (Tr/Ta) x (Sr/Sa) x (DDr/DDa) x (Da/Dr) \$\$

- Tr = The number of tags detected on the receievr

- Ta = The number of tags detected across all receivers

- Sr = The number of species detected on the receiver

- Sa = The number of species detected across all receivers

- DDa = The number of unique days with detections across all receivers

- DDr = The number of unique days with detections on the receiver

- Da = The number of days the array was active

- Dr = The number of days the receiver was active

## Usage

``` r
REI(detections, deployments)
```

## Arguments

- detections:

  a glatos detections class data table

- deployments:

  a glatos receivers class data table

## Value

a list of receivers with lat and long and the receiver efficiency index

## Author

Alex Nunes <anunes@dal.ca>

## Examples

``` r
if (FALSE) { # \dontrun{
det_file <- system.file("extdata", "hfx_detections.csv",
  package = "glatos"
)

dep_file <- system.file("extdata", "hfx_deployments.csv",
  package = "glatos"
)

hfx_deployments <- glatos::read_otn_deployments(dep_file)
dets <- glatos::read_otn_detections(det_file)

hfx_receiver_efficiency_index <- glatos::REI(dets, hfx_deployments)
} # }
```
