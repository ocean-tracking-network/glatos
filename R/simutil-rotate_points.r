#' @title Rotate points in a 2-d plane
#'
#' @description
#' Rotate points around a point in a 2-d plane
#'
#' @param x A numeric vector of x coordinates; minimum of 2.
#' @param y A numeric vector of y coordinates; minimum of 2.
#' @param theta A numeric scalar with the angle of rotation in degrees;
#'   positive is clockwise.
#' @param focus A numeric vector of x (first element) and y (second element)
#' coordinates for the point around which `x` and `y` will rotate.
#'
#' @details
#' Points are shifted to be centered at the focus, then rotated using a
#' rotation matrix, then shifted back to original focus.
#'
#' @return A two-column data frame containing:
#' \item{x}{x coordinates}
#' \item{y}{y coordinates}
#'
#' @note
#' This function is called from [crw_in_polygon()]
#'
#' @author C. Holbrook (cholbrook@usgs.gov)
#'
#' @examples
#' x <- runif(10, 0, 10)
#' y <- runif(10, 0, 10)
#' plot(x, y, type = "b", pch = 20)
#' foo <- rotate_points(x, y, 20, c(5, 5))
#' points(foo$x, foo$y, type = "b", pch = 20, col = "red")
#'
#' @export
rotate_points <- function(x, y, theta, focus) {
  pos <- cbind(x, y) # original points
  theta.rad <- theta * (pi / (180)) # convert to radians
  focus <- as.numeric(focus) # coerce to vector if not
  # make rotation matrix - for clockwise rotation
  rot <- matrix(c(cos(theta.rad), -sin(theta.rad), sin(theta.rad), cos(theta.rad)), ncol = 2)
  pos[, 1] <- pos[, 1] - focus[1] # shift points to center at focus
  pos[, 2] <- pos[, 2] - focus[2] # shift points to center at focus
  pos <- t(rot %*% t(pos)) # rotate
  pos[, 1] <- pos[, 1] + focus[1] # shift back
  pos[, 2] <- pos[, 2] + focus[2] # shift back
  colnames(pos) <- c("x", "y")
  return(as.data.frame(pos))
}
