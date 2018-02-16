# get_days()
# ----------
# Determines which calculation method to use for the residency index.
#
# Wrapper method for the calulation methods above.
#
# @var dets - data frame pulled from the detection events
# @var calculation_method - determines which method above will be used to count total time and location time
get_days <- function(dets, calculation_method='kessel') {
    days <- 0
    if (calculation_method == 'aggregate_with_overlap') {
      days <- glatos:::aggregate_total_with_overlap(dets)
    } else if(calculation_method == 'aggregate_no_overlap') {
      days <- glatos:::aggregate_total_no_overlap(dets)
    } else if(calculation_method == 'timedelta') {
      days <- glatos:::total_diff_days(dets)
    } else {
      days <- glatos:::total_days_count(dets)
    }
    return(days)
}
