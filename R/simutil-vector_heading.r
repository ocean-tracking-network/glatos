#' @title Calculate direction (heading) of a vector (in degrees)
#' 
#' @description
#' Calculate direction (heading) of a vector (in degrees)
#'
#' @param x A numeric vector of x coordinates; minimum of 2.
#' @param y A numeric vector of y coordinates; minimum of 2.
#'
#' @details
#' Calculates direction (in degrees) for each of k-1 vectors, where 
#' k = length(x) - 1 = length(y). Lenghts of \code{x} and \code{y} must be 
#' equal.
#' 
#' @return A numeric scalar with heading in degrees or a numeric vector of 
#' headings if \code{length(x) > 2}.
#'
#' @note
#' This function is called from within \code{\link{crwInPolygon}}
#' 
#' @author C. Holbrook (cholbrook@usgs.gov) 
#'
#' @examples
#' x=c(2,4)
#' y=c(2,4)
#' vector_heading(x, y)
#' 
#' x2=c(2,4,2)
#' y2=c(2,4,2)
#' vector_heading(x2, y2)
#'
#' @export
vector_heading <- function (x, y) {
    theta.rad <- atan2(diff(x), diff(y))
    theta.deg <- theta.rad * 180/pi
		theta.deg <- theta.deg - 360*(theta.deg > 360)
    return(theta.deg)
}