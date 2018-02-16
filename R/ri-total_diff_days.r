# total_days_diff()
# -----------------
# The function below determines the total days difference.
#
# The difference is determined by the minimal first_detection of every detection and the maximum last_detection of every detection.
# Both are converted into a datetime then subtracted to get a timedelta. The timedelta
# is converted to seconds and divided by the number of seconds in a day (86400). The function
# returns a floating point number of days (i.e. 503.76834).
#
# @var Detections - data frame pulled from the compressed detections CSV
require(dplyr)
total_diff_days <- function(detections) {
  first <- detections$first_detection[which.min(detections$first_detection)]
  last <- detections$first_detection[which.max(detections$first_detection)]
  total <- as.double(difftime(last, first, units="secs"))/86400.0
  return(total)
}
