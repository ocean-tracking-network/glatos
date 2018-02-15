#' False detection filter
#'
#' Identify possible false detections based on "short interval" criteria
#'
#' @param detections A data frame containing detection data (e.g., from the 
#'   standard GLATOS detection export file '*_detectionsWithLocs.csv'). 
#'   Must contain the column \code{min_lag} or equivalent (specified by 
#'   \code{minLagCol}). \code{min_lag} is loosely based on the the 
#'   "short interval" method described by Pincock (2012). In this case 
#'   (GLATOS), it is defined for each detection as the shortest interval 
#'   (in seconds) between either the previous or next detection (whichever 
#'   is closest) of the same transmitter on the same receiver.
#'   
#' @param tf A numeric scalar indicating the time threshold (in seconds; e.g., 
#'   Pincock's (2012) "short interval") for identifying possible false 
#'   detections.
#'   
#' @param minLagCol A character string containing the name of the column 
#'   in \code{detections} that contains 'min_lag'.
#'
#' @details Detections are identified as potentially false when 
#'   \code{min_lag > tf}.
#' @details 
#' A new column (\code{passedFilter}), indicating if each record (row) passed 
#' the filter, is added to the input data frame. This function was written
#' specifically with GLATOS standard detection export in mind, so it requires
#' \code{min_lag} or equivalent. 
#' @details  
#' A common rule of thumb for choosing tf for VEMCO PPM encoded transmitters 
#'   is 30 times the nominal delay (e.g., 3600 s for a transmitter with a 
#'   120 s nominal delay) - see Pincock (2012).
#'
#' @return A data frame consisting of \code{detections} with an additional 
#'   column 'passedFilter' indicating if each detection did (1) or did not (0) 
#'   pass the criteria.
#'
#' @author T. R. Binder
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
#' data("walleye_detections") #example data
#' 
#' head(walleye_detections)
#'
#' dtx <- falseDetectionFilter(walleye_detections, 3600)
#' head(dtx)
#' 
#' @export

falseDetectionFilter <- function(detections, tf, minLagCol = "min_lag"){
    # Check that the minLag column is in the detections dataframe
    if (!(minLagCol %in% names(detections))){
        stop(paste0("The column '",minLagCol,
					"' must appear in the detections data frame."), call.=FALSE)
    }

    # Identify possible false detections by comparing "min_lag" column to 
		#  threshold defined in object "tf".
    detections$passedFilter <- ifelse(!is.na(detections[,minLagCol]) & 
			detections[,minLagCol] <= tf, 1, 0)
		with(detections,
			message(paste0("The filter identified ", 
				nrow(detections) - sum(passedFilter), " (", 
				round((nrow(detections) - sum(passedFilter))/
				nrow(detections)*100, 2), "%) of ", nrow(detections), 
				" detections as potentially false.")))
    return(detections)
}