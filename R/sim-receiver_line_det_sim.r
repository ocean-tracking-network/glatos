#' @title Simulate detection of acoustic-tagged fish crossing a receiver line
#'
#' @description
#' Estimate, by simulation, the probability of detecting an acoustic-tagged
#' fish on a receiver line, given constant fish velocity (ground speed),
#' receiver spacing, number of receivers, and detection range curve.
#'
#' @param vel A numeric scalar with fish velocity in meters per second.
#'
#' @param delayRng A 2-element numeric vector with minimum and maximum delay
#'   (time in seconds from end of one coded burst to beginning of next)
#'
#' @param burstDur A numeric scalar with duration (in seconds) of each coded
#'   burst (i.e., pulse train).
#'
#' @param recSpc A numeric vector with distances (in meters) between receivers.
#'   The length of vector is N-1, where N is number of receivers. One receiver
#'   is simulated when `recSpc = NA` (default).
#'
#' @param maxDist A numeric scalar with maximum distance between tagged fish
#'   and any receiver during simulation (i.e., sets spatial boundaries)
#'
#' @param rngFun A function that defines detection range curve; must accept a
#'   numeric vector of distances and return a numeric vector of detection
#'   probabilities at each distance.
#'
#' @param outerLim A two-element numeric vector with space (in meters) in which
#'   simulated fish are allowed to pass to left (first element) and right
#'   (second element) of the receiver line.
#'
#' @param nsim Integer scalar with the number of crossings (fish) to simulate
#'
#' @param showPlot A logical scalar. Should a plot be drawn showing receivers
#'   and fish paths?
#'
#' @details
#' Virtual tagged fish (N=`nsim`) are "swum" through a virtual receiver
#' line. The first element of `recSpc` determines spacing between first
#' two receivers in the line, and each subsequent element of `recSpc`
#' determine spacing of subsequent receivers along the line, such that the
#' number of receivers is equal to `length(recSpc) + 1`. Each fish moves
#' at constant velocity (`vel`) along a line perpendicular to the
#' receiver line. The location of each fish path along the receiver line is
#' random (drawn from uniform distribution), and fish can pass outside the
#' receiver line (to the left of the first receiver or right of last receiver)
#' if `outerLim[1]` or `outerLim[2]` are greater than 0 meters.
#' Each fish starts and ends about `maxDist` meters from the receiver
#' line.
#'
#' @details
#' A simulated tag signal is transmitted every `delayRng[1]` to
#' `delayRng[2]` seconds. At time of each transmission, the distance is
#' calculated between the tag and each receiver, and rngFun is used to
#' calculate the probability (p) that the signal was detected on each receiver.
#' Detection or non-detection on each receiver is determined by a draw from a
#' Bernoulli distribution with probability p.
#'
#' @return A data frame with one column:
#'   \item{detProb}{The proportion of simulated fish that were detected more
#'   than once on any single receiver.}
#'
#' @author C. Holbrook \email{cholbrook@usgs.gov}
#'
#' @references
#' For application example, see:  \cr\cr
#'   Hayden, T.A., Holbrook, C.M., Binder, T.R., Dettmers, J.M., Cooke, S.J.,
#'   Vandergoot, C.S. and Krueger, C.C., 2016. Probability of acoustic
#'   transmitter detections by receiver lines in Lake Huron: results of
#'   multi-year field tests and simulations. Animal Biotelemetry, 4(1), p.19.
#'   \cr <https://animalbiotelemetry.biomedcentral.com/articles/10.1186/s40317-016-0112-9>
#'
#' @examples
#' # EXAMPLE 1 - simulate detection on line of ten receivers
#'
#' # Define detection range function (to pass as rngFun)
#' # that returns detection probability for given distance
#' # assume logistic form of detection range curve where
#' #   dm = distance in meters
#' #   b = intercept and slope
#' pdrf <- function(dm, b = c(5.5, -1 / 120)) {
#'   p <- 1 / (1 + exp(-(b[1] + b[2] * dm)))
#'   return(p)
#' }
#'
#' # preview detection range curve
#' plot(pdrf(0:2000),
#'   type = "l", ylab = "Probability of detecting each coded burst",
#'   xlab = "Distance between receiver and transmitter"
#' )
#'
#' # Simulate detection using pdrf; default values otherwise
#' dp <- receiver_line_det_sim(rngFun = pdrf)
#' dp
#'
#' # Again with only 10 virtual fish and optional plot to see simulated data
#' dp <- receiver_line_det_sim(rngFun = pdrf, nsim = 10, showPlot = TRUE) # w/ optional plot
#' dp
#'
#' # Again but six receivers and allow fish to pass to left and right of line
#' dp <- receiver_line_det_sim(
#'   rngFun = pdrf, recSpc = rep(1000, 5),
#'   outerLim = c(1000, 1000), nsim = 10, showPlot = TRUE
#' )
#' dp
#'
#' # Again but four receivers with irregular spacing
#' dp <- receiver_line_det_sim(
#'   rngFun = pdrf, recSpc = c(2000, 4000, 2000),
#'   outerLim = c(1000, 1000), nsim = 10, showPlot = TRUE
#' )
#' dp
#'
#'
#' # EXAMPLE 2 - summarize detection probability vs. receiver spacing
#'
#' # two receivers only, spaced 'spc' m apart
#' # define scenarios where two receiver are spaced
#' spc <- seq(100, 5000, 100) # two receivers spaced 100, 200, ... 5000 m
#' # loop through scenarios, estimate detection probability for each
#' for (i in 1:length(spc)) {
#'   if (i == 1) dp <- numeric(length(spc)) # pre-allocate
#'   dp[i] <- receiver_line_det_sim(recSpc = spc[i], rngFun = pdrf)
#' }
#' cbind(spc, dp) # view results
#' # plot results
#' plot(spc, dp,
#'   type = "o", ylim = c(0, 1),
#'   xlab = "distance between receivers in meters",
#'   ylab = "proportion of virtual fish detected"
#' )
#' # e.g., >95% virtual fish detected up to 1400 m spacing in this example
#'
#'
#' # EXAMPLE 3 - summarize detection probability vs. fish swim speed
#'
#' # define scenarios of fish movement rate
#' swim <- seq(0.1, 5.0, 0.1) # constant velocity
#' for (i in 1:length(swim)) {
#'   if (i == 1) dp <- numeric(length(swim)) # pre-allocate
#'   dp[i] <- receiver_line_det_sim(vel = swim[i], rngFun = pdrf)
#' }
#' cbind(swim, dp) # view results
#' # plot results
#' plot(swim, dp,
#'   type = "o", ylim = c(0, 1), xlab = "fish movement rate, m/s",
#'   ylab = "proportion of virtual fish detected"
#' )
#' # e.g., >95% virtual fish detected up to 1.7 m/s rate in this example
#' # e.g., declines linearly above 1.7 m/s
#'
#'
#' # EXAMPLE 4 - empirical detection range curve instead of logistic
#'
#' # create data frame with observed det. efficiency (p) at each distance (x)
#' edr <- data.frame(
#'   x = c(0, 363, 444, 530, 636, 714, 794, 889, 920), # tag-receiver distance
#'   p = c(1, 1, 0.96, 0.71, 0.67, 0.75, 0.88, 0.21, 0)
#' ) # detection prob
#'
#' # now create a function to return the detection probability
#' # based on distance and linear interpolation within edr
#' # i.e., estimate p at given x by "connecting the dots"
#' edrf <- function(dm, my.edr = edr) {
#'   p <- approx(x = my.edr$x, y = my.edr$p, xout = dm, rule = 2)$y
#'   return(p)
#' }
#'
#' # preview empirical detection range curve
#' plot(edrf(0:2000),
#'   type = "l",
#'   ylab = "probability of detecting each coded burst",
#'   xlab = "distance between receiver and transmitter, meters"
#' )
#'
#' # use empirical curve (edrf) in simulation
#' dp <- receiver_line_det_sim(rngFun = edrf, nsim = 10, showPlot = T) # w/ optional plot
#' dp
#'
#' @export
receiver_line_det_sim <- function(
    vel = 1, delayRng = c(120, 360), burstDur = 5.0,
    recSpc = 1000, maxDist = 2000, rngFun, outerLim = c(0, 0), nsim = 1000, showPlot = FALSE) {
  # check if rngFun is function
  if (any(!is.function(rngFun))) {
    stop(paste0(
      "Error: argument 'rngFun' must be a function...\n",
      "see ?receiver_line_det_sim\n check: is.function(rngFun)"
    ))
  }


  # Define receiver line

  if (any(is.na(recSpc))) recSpc <- 0 # to simulate one receiver
  xLim <- c(0, sum(recSpc) + sum(outerLim))
  recLoc <- c(outerLim[1], outerLim[1] + cumsum(recSpc))
  yLim <- c(-maxDist, maxDist)


  # Simulate tag transmissions

  nTrns <- floor((diff(yLim) / vel) / delayRng[1]) # number of transmissions

  # sample delays
  del <- matrix(runif(nTrns * nsim, delayRng[1], delayRng[2]),
    nrow = nsim, ncol = nTrns
  )
  del <- del + burstDur # add burst duration (for Vemco)
  trans <- t(apply(del, 1, cumsum)) # time series of signal transmissions
  # "center" the fish track over the receiver line; with some randomness
  trans <- trans - matrix(runif(nsim, trans[, nTrns / 2], trans[, (nTrns / 2) + 1]),
    nrow = nsim, ncol = nTrns
  )
  # row = simulated fish; col = signal transmission
  fsh.x <- matrix(runif(nsim, xLim[1], xLim[2]), nrow = nsim, ncol = nTrns)
  # convert from time to distance from start
  fsh.y <- matrix(trans * vel, nrow = nsim, ncol = nTrns)


  # Optional quick and dirty plot just to see what is happening
  if (showPlot) {
    plot(NA,
      xlim = xLim, ylim = yLim, asp = c(1, 1),
      xlab = "Distance (in meters) along receiver line",
      ylab = "Distance (in meters) along fish path"
    )
    # fish tracks and transmissions
    for (i in 1:nsim) {
      lines(fsh.x[i, ], fsh.y[i, ], col = "grey") # fish tracks
      points(fsh.x[i, ], fsh.y[i, ], pch = 20, cex = 0.8) # signal transmissions
    }
    # receiver locations
    points(recLoc, rep(0, length(recLoc)), pch = 21, bg = "red", cex = 1.2)
    legend("topleft",
      legend = c("receiver", "sim. fish path", "tag transmit"),
      pch = c(21, 124, 20), col = c("black", "grey", "black"), pt.bg = c("red", NA, NA),
      pt.cex = c(1.2, 1, 0.8)
    )
  }


  # Simulate detections

  # calculate distances between transmissions and receivers
  for (i in 1:length(recLoc)) { # loop through receivers
    if (i == 1) { # pre-allocate objects, if first receiver
      succ <- detP <- distM <- vector("list", length(recLoc))
      nDets <- matrix(NA, nrow = nsim, ncol = length(recLoc)) # col = receiver
    }
    # tag-receiver distances in meters
    distM[[i]] <- sqrt((fsh.x - recLoc[i])^2 + (fsh.y)^2)
    # detection probabilities
    detP[[i]] <- matrix(rngFun(distM[[i]]), nrow = nsim)
    # detected=1, not=0
    succ[[i]] <- matrix(rbinom(length(detP[[i]]), 1, detP[[i]]), nrow = nsim)
    # number of times each transmitter detected on ith receiver
    nDets[, i] <- rowSums(succ[[i]])
  }

  # max detects on any one receiver for each transmitter
  maxDet <- apply(nDets, 1, max)
  # proportion of transmitters detected more than once on any receiver
  detProb <- mean(maxDet > 1)

  return(detProb)
}
