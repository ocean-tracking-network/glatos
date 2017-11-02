#' Calculate 'min_lag' for identifying potential false positive detections
#'
#' Calculate 'min_lag' column of a dataset, add this column into the dataset,
#' and return the dataset with the new column
#'
#' @param detections A data frame containing detection data with at least 3
#'   columns containing 'transmitter', 'receiver', and 'timestamp'. Column names
#'   are specified by \code{type}.
#'
#' @param type A character string that contains the type of data that is being
#'   passed in, for example, "OTN", "GLATOS", or "sample". Default is "GLATOS".
#'
#' @param detColNames An optional list that contains the user-defined column
#'   names
#'
#' @details detColNames is defined as a list with the names of the required
#'   columns in \code{detections}, defined by \code{type}: \itemize{ \item
#'   \code{transmitters} is a character string with the name of the column
#'   containing the ids of the transmitters ('transmission_id' for GLATOS data,
#'   'tagname' for OTN data, or 'transmitter' for sample data). \item
#'   \code{receivers} is a character string with the name of the column
#'   containing the ids of the receivers ('receiver_sn' for GLATOS data,
#'   'receiver_group' for OTN data, or 'receiver' for sample data). \item
#'   \code{timestampCol} is a character string with the name of the column
#'   containing datetime stamps for the detections (MUST be of class 'POSIXct')
#'   ('detection_timestamp_utc' for GLATOS data, 'datecollected' for OTN data,
#'   or 'time' for sample data). }
#'
#' @details Each value in the min_lag column defines the minimum number of
#'   seconds between either a time with the same transmitter and receiver if
#'   before or after the current time. This is calculated with the dplyr package
#'   in R. This min_lag column will be used in falseDetectionFilter.R to filter
#'   false data.
#'
#' @details A new column (\code{min_lag}), indicating the minimum lag of each
#' entry (according to the definition above), is added to the input data frame.
#'
#' @return A data frame containing the data with a min_lag column appended
#'
#' @author A. Dini
#'
#' @examples
#' library(glatos)
#'
#' #Example with GLATOS data
#' data(walleye_detections)
#'
#' dtc2 <- getMinLag(walleye_detections)
#'
#' @export
require(lazyeval)
getMinLag <- function(detections, type = "GLATOS", detColNames = list()) {
  #Check if user has not set column names
  if(length(detColNames) == 0) {
    if(type == "GLATOS") { #Set column names for GLATOS data
      detColNames <- list(transmittersCol = "transmitter_id",
                          receiversCol = "receiver_sn",
                          timestampCol = "detection_timestamp_utc")
    } else if (type == "OTN"){ #Set column names for OTN data
      detColNames <- list(transmittersCol = "catalognumber",
                          receiversCol = "station",
                          timestampCol = "datecollected")
    } else if (type == "sample") { #Set column names for sample data
      detColNames <- list(transmittersCol = "transmitter",
                          receiversCol = "receiver",
                          timestampCol = "time")
    } else { #Other type
      stop(paste0("The type '", type, "' is not defined."), call. = FALSE)
    }
  }

  # Check that the specified columns above appear in the detections dataframe
  missingCols <- setdiff(unlist(detColNames), names(detections))

  if (length(missingCols) > 0){
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '", missingCols, "'",
                collapse="\n")),
         call. = FALSE)
  }


  detections <- detections %>% mutate_(timestamp = detColNames$timestampCol)
  detections <- mutate(detections, timestamp = as.POSIXct(detections$timestamp))
  detections <- mutate(detections, this.date = as.character(detections$timestamp))

  out <- detections %>%
  group_by_(c(detColNames$transmittersCol, detColNames$receiversCol)) %>%

 # Arrange these groups in order of animal and timestamp
 arrange_(detColNames$transmittersCol, detColNames$timestampCol) %>%

 mutate_( # lag() and lead() are a snappy way to peek ahead and behind each row,
     # ifelse so we only look at the other rows on station/animal match. Others get NA
   last.date = interp(~ifelse(tc == lag(tc) & rc == lag(rc), lag(this.date), NA), rc=as.name(detColNames$receiversCol), tc=as.name(detColNames$transmittersCol)),
   next.date = interp(~ifelse(tc == lead(tc) & rc == lead(rc), lead(this.date), NA), rc=as.name(detColNames$receiversCol), tc=as.name(detColNames$transmittersCol))
 ) %>%
 mutate( # Less-snappy way to run a bunch of conditional logic on what's ahead and behind.
       last.diff = ifelse(is.na(last.date), NA, difftime(this.date, last.date, units="secs")),
       next.diff = ifelse(is.na(next.date), NA, difftime(next.date, this.date, units="secs"))
     ) %>% # Now can run the filter step to evaluate
   mutate(
    min_lag = pmin(last.diff, next.diff, na.rm=TRUE),
    )
     return(out)
}
