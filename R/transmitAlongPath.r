#' @export
#simulate tag signal transmissions along a path
transmitAlongPath <- function(path=NA,vel=0.5,delayRng=c(60,180),burstDur=5.0){
    #cumulative distance travelled in meters 
    path$cumdistm <- c(0,cumsum(sqrt(diff(path$x)^2 + diff(path$y)^2)))
    path$etime <- path$cumdistm/vel #elapse time in seconds
    ntrns <- max(path$etime)/(delayRng[1]+burstDur)
    ints <- runif(ntrns,delayRng[1]+burstDur,delayRng[2]+burstDur)
    ints[1] <- runif(1,0,ints[1]) #draw random the start time
    etime <- cumsum(ints) #elapsed time
    etime <- etime[etime <= max(path$etime)] #subset trans during track duration

    #interpolate transmit locations along track
    trns <- data.frame(
          x = approx(path$etime, path$x, xout=etime)$y,      
          y = approx(path$etime, path$y, xout=etime)$y,
          et = etime)
                
    return(trns)
}
