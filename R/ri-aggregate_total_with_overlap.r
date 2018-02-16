# aggregate_total_with_overlap()
# ----------------------------------------
#
# The function below aggregates timedelta of first_detection and last_detection of each detection into
# a final timedelta then returns a float of the number of days. If the first_detection and last_detection
# are the same, a timedelta of one second is assumed.
#
# @var Detections -data frame pulled from the compressed detections CSV
require(lubridate)
require(dplyr)
aggregate_total_with_overlap <- function(detections) {
  detections <- mutate(detections, timedelta = as.double(difftime(as.Date(last_detection),as.Date(first_detection), units="secs")))
  detections <- mutate(detections, timedelta = dplyr::recode(detections$timedelta, `0` = 1))
  total <- as.double(sum(detections$timedelta))/86400.0
  return(total)
}
