#' False detection filter
#'
#' Identify possible false detections based on "short interval" criteria
#'
#' @param det A \code{glatos_detections} object (e.g., produced by
#'   \link{read_glatos_detections}) \emph{or} a data frame containing detection
#'   data and \code{min_lag}. If \code{min_lag} is missing, then it will be 
#'   added using \link{min_lag}.
#'   
#' @param tf A number indicating the time threshold (in seconds; e.g., 
#'   Pincock's (2012) "short interval") for identifying possible false 
#'   detections.
#'   
#' @param min_lag_col A character string containing the name of the column 
#'   in \code{det} that contains 'min_lag'.
#'   
#' @param show_plot Indicates if a plot should be displayed showing the 
#'   proportion of detections that exceed min_lag from min_lag = 1 to 
#'   min_lag = 5 * tf. 
#'
#' @details Detections are identified as potentially false when 
#'   \code{min_lag > tf}.
#'   
#' @details 
#' A new column (\code{passed_filter}), indicating if each record (row) passed 
#' the filter, is added to the input data frame. This function was written
#' specifically with GLATOS standard detection export in mind, so it requires
#' \code{min_lag} or equivalent. 
#' 
#' @details  
#' A common rule of thumb for choosing tf for VEMCO PPM encoded transmitters 
#'   is 30 times the nominal delay (e.g., 3600 s for a transmitter with a 
#'   120 s nominal delay) - see Pincock (2012).
#'   
#' @details  
#' When \code{show_plot = TRUE} then the plot can be used to assess sensistivity 
#'   of the proportion of detections removed to the choice of tf.
#'
#' @return A data frame consisting of \code{det} with an additional 
#'   column 'passed_filter' indicating if each detection did (1) or did not (0) 
#'   pass the criteria.
#'
#' @author T. R. Binder, edited by A. Dini
#' 
#' @aliases falseDetectionFilter
#'
#' @references
#'   Pincock, D.G., 2012. False detections: what they are and how to remove them 
#'     from detection data. Vemco Division, Amirix Systems Inc., Halifax, 
#'     Nova Scotia.
#'     \cr \url{http://www.vemco.com/pdf/false_detections.pdf}
#' @references
#'   Simpfendorfer, C.A., Huveneers, C., Steckenreuter, A., Tattersall, K., 
#'     Hoenner, X., Harcourt, R. and Heupel, M.R., 2015. Ghosts in the data: 
#'     false detections in VEMCO pulse position modulation acoustic telemetry 
#'     monitoring equipment. Animal Biotelemetry, 3(1), p.55.
#'     \cr \url{https://animalbiotelemetry.biomedcentral.com/articles/10.1186/s40317-015-0094-z}
#'
#' @examples
#' #get path to example detection file
#' det_file <- system.file("extdata", "walleye_detections.csv",
#'                          package = "glatos")
#' det <- read_glatos_detections(det_file)
#'
#' det <- false_detections(det, 3600)
#' head(det)
#' 
#' #plot sensitivity to tf
#' det <- false_detections(det, 3600, show_plot = TRUE)
#' 
#' @export

false_detections <- function(det, tf, min_lag_col = "min_lag", 
  show_plot = FALSE){
 
  # Check that the min_lag column is in the detections dataframe
  # If not, add it in using min_lag function
  if (!(min_lag_col %in% names(det))){
    det <- min_lag(det) #Get min_lag column
  }
  
  # Identify possible false detections by comparing "min_lag" column to 
	#  threshold defined in object "tf".
  det$passed_filter <- ifelse(!is.na(det[, min_lag_col]) & 
		                                       det[, min_lag_col] <= tf, 1, 0)
  
  nr <- nrow(det)
  
  message(paste0("The filter identified ",
    nr - sum(det$passed_filter), " (",
    round((nr - sum(det$passed_filter))/
        nr*100, 2), "%) of ", nr,
    " detections as potentially false."))
  
  if(show_plot){
    cdist <- ecdf(det$min_lag) #empirical cumulative distribution function
    x <- 1:(5*tf)
    y <- 1 - cdist(x)
    plot(x, y, type = "l", las = 2, xlab = "min_lag threshold (tf)", 
      ylab = "Proportion of min_lag > tf")
    abline(v = tf, col = "red", lty = 2)
  }  
  
  return(det)
}