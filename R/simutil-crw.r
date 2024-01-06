#' Simulate a correlated random walk
#'
#' Simulate a random walk as series of equal-length steps with turning angles
#'   drawn from a normal distribution.
#'
#' @param theta A 2-element numeric vector with turn angle parameters
#'   (theta[1] = mean; theta[2] = sd) from normal distribution.
#'
#' @param stepLen A numeric scalar with total distance moved in each step.
#'
#' @param initPos A 2-element numeric vector with nital position (initPos[1]=x,
#'   initPos[2]=y).
#'
#' @param initHeading A numeric scalar with initial heading in degrees.
#'
#' @param nsteps A numeric scalar with number of steps to simulate.
#'
#' @details
#' First, nsteps turn angles are drawn from a normal distribution. Second, the
#' cumulative sum of the vector of turn angles defines the heading within each
#' step. The x and y component vectors in each are then calculated and summed
#' to obtain the simualted path.
#'
#' @return A two-column data frame containing:
#' \item{x}{x coordinates}
#' \item{y}{y coordinates}
#'
#' @author C. Holbrook (cholbrook@usgs.gov)
#'
#' @note Adapted from code provided by Tom Binder.
#'
#' @examples
#' foo <- crw(
#'   theta = c(0, 5), stepLen = 10, initPos = c(0, 0), initHeading = 0,
#'   nsteps = 10
#' )
#' plot(foo, type = "o", pch = 20, asp = c(1, 1))
#'
#' @export
crw <- function(
    theta = c(0, 5), stepLen = 10, initPos = c(0, 0),
    initHeading = 0, nsteps = 10000) {
  # generate turn angles
  heading <- rnorm(nsteps, mean = theta[1], sd = theta[2])
  heading <- initHeading + cumsum(heading)
  heading <- heading %% 360
  heading_rad <- heading * pi / 180 # convert to radians

  # create steps
  xlen <- sin(heading_rad) * stepLen # x-component vector
  ylen <- cos(heading_rad) * stepLen # y-component vector
  path <- data.frame(
    x = as.numeric(initPos)[1] + cumsum(xlen),
    y = as.numeric(initPos)[2] + cumsum(ylen)
  )

  return(path)
} # end crw def
