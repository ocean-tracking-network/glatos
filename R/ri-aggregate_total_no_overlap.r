# aggregate_total_no_overlap()
# --------------------------------------
#
# The function below aggregates timedelta of first_detection and last_detection, excluding overlap between
# detections. Any overlap between two detections is converted to a new detection using the earlier
# first_detection and the latest last_detection. If the first_detection and last_detection are the same, a timedelta of one
# second is assumed.
#
# @var Detections - data frame pulled from the compressed detections CSV
require(lubridate)
require(dplyr)
aggregate_total_no_overlap <- function(detections) {
  total <- 0.0
  detections <- arrange(detections, first_detection)
  detcount <- as.integer(dplyr::count(detections))
  detections <- mutate(detections, interval = interval(first_detection,last_detection))
  detections <- mutate(detections, timedelta = as.double(difftime(as.Date(last_detection),as.Date(first_detection), units="secs")))
  detections <- mutate(detections, timedelta = dplyr::recode(detections$timedelta, `0` = 1))

  next_block <- 2
  start_block <- 1
  end_block <- 1
  while(next_block <= detcount) {
    # if it overlaps
    if(next_block < detcount && int_overlaps(dplyr::nth(detections$interval, end_block), dplyr::nth(detections$interval, next_block))) {
      if(dplyr::nth(detections$interval, next_block) >= dplyr::nth(detections$interval, end_block)) {
        end_block <- next_block
      }

      if(end_block == detcount) {
        tdiff <- as.double(difftime(dplyr::nth(detections$last_detection,end_block), dplyr::nth(detections$first_detection, start_block), units="secs"))

        if(tdiff == 0.0) {
          tdiff <- 1
        }
        start_block <- next_block
        end_block <- next_block + 1
        total <- total + as.double(tdiff)
      }
    } else {
      #if it doesn't overlap
      tdiff <- 0.0

      # Generate the time difference between the start of the start block and the end of the end block
      tdiff <- as.double(difftime(dplyr::nth(detections$last_detection,end_block), dplyr::nth(detections$first_detection, start_block), units="secs"))

      start_block <- next_block
      end_block <- next_block
      total <- total + as.double(tdiff)
    }
    next_block <- next_block + 1
  }
  total <- total/86400.0
  return(total)
}
