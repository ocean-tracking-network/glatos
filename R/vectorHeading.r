#' @export
#function to calculate ordinal direction for vector
vectorHeading <- function (x, y) {
    theta.rad <- atan2(diff(x), diff(y))
    theta.deg <- theta.rad * 180/pi
	#theta.deg <- theta.deg %% 360
	theta.deg <- theta.deg - 360*(theta.deg > 360)
    return(theta.deg)
}