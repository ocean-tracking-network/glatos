\name{transmitAlongPath}
\Rdversion{1.1}
\alias{transmitAlongPath}
\title{
Function to telemetry transmitter signals along a path
}
\description{
Simulate tag signal transmission along a pre-defined path (x, y coords)
based on constant movement velocity, transmitter delay range, and duration
of signal.
}
\usage{
transmitAlongPath(path=NA,vel=0.5,delayRng=c(60,180),burstDur=5.0)
}
\arguments{
  \item{path}{ data frame of x and y coordinates (min. two rows) that define path}
  \item{vel}{ movement velocity along track; assumed constant }
  \item{delayRng}{ range of delays to be drawn from uniform distribution }
  \item{burstDur}{ duration of each encoded signal }
}
\details{
Delays are drawn from uniform distribution defined by delay range.
First, elapsed time in seconds at each node in path is calculated based on path
length and velocity. Next, delays are simulated and burst durations are added to
each delay to determine the time of each signal transmission. Location of
each signal transmission along the path is linearly interpolated.
}
\value{
A data frame with columns
  \item{x}{ x coordinates for start of each transmission }
  \item{y}{ y coordinates for start of each transmission }
  \item{et}{ elapsed time to start of each transmission }
}
\author{
C. Holbrook (cholbrook@usgs.gov)
}
\examples{
mypath <- data.frame(x=seq(0,1000,100),y=seq(0,1000,100))
mytrns <- transmitAlongPath(mypath,vel=0.5,delayRng=c(60,180),burstDur=5.0)
plot(mypath,type="b")
points(mytrns,pch=20,col="red")
}