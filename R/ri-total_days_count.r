# total_days_count()
# ------------------
# The function below takes a Pandas DataFrame and determines the number of days any
# detections were seen on the array.
#
# The function converts both the first_detection and last_detection columns into a date with no hours, minutes,
# or seconds. Next it creates a list of the unique days where a detection was seen. The size of the
# list is returned as the total number of days as an integer.
#
# *** NOTE ****
# Possible rounding error may occur as a detection on 2016-01-01 23:59:59 and a detection on
# 2016-01-02 00:00:01 would be counted as days when it is really 2-3 seconds.
#
#
# @var Detections - data frame pulled from the compressed detections CSV
require(dplyr)
total_days_count <- function(detections) {
  startdays <- distinct(select(mutate(detections, days = as.Date(first_detection)), days))
  enddays <- distinct(select(mutate(detections, days = as.Date(last_detection)), days))
  days <- bind_rows(startdays,enddays)
  daycount <- as.double(dplyr::count(dplyr::distinct(select(days,days ))))
  return(daycount)
}
