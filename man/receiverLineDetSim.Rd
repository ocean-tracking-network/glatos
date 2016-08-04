\name{receiverLineDetSim}
\Rdversion{0.1}
\alias{receiverLineDetSim}
\title{
Simulate detection of acoustic-tagged fish crossing a receiver line
}
\description{
Estimate, by simulation, the probability of detecting an acoustic-tagged
fish on a receiver line, given constant fish velocity (ground speed),
receiver spacing, number of receivers, and detection range curve.
}
\usage{
receiverLineDetSim(vel=1,delayRng=c(120,360),burstDur=5.0,recSpc=100,
    maxDist=2000,rngFun,outerLim=c(0,0),nsim=1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vel}{numeric (scalar); fish velocity in meters per second}
  \item{delayRng}{numeric (2-element vector); acoustic tag delay (i.e., interval between coded bursts) range in seconds}
  \item{burstDur}{numeric (scalar); duration of coded burst (i.e., pulse train) in seconds; default is for Vemco Global Coding }
  \item{recSpc}{numeric (vector); distance in meters between receivers; length of vector is N-1, where N is number of receivers}
  \item{maxDist}{numeric (scalar); maximum distance between tagged fish and receiver during simulation}
  \item{rngFun}{function; returns detection probability for a given distance between tag and receiver}
  \item{outerLim}{numeric (2-element vector); space (in meters) to left (first element) and right (last element) of receiver line in which simulated fish can pass line}
  \item{nsim}{integer; number of simulations (i.e., number of fish)}
  \item{showPlot}{boolean; Should a plot be drawn showing receivers and fish paths?}
}
\details{
Virtual tagged fish (N=nsim) are "swum" through a virtual receiver line. The
first element of 'recSpc' determines spacing between first two receivers in the
line, and each subsequent element of 'recSpc' determine spacing of subsequent
receivers along the line, such that the number of receivers is equal to
length(recSpc) + 1. Each fish moves at constant velocity (vel) along a line
perpendicular to the receiver line. The location of each fish path along the
receiver line is random (drawn from uniform distribution), and fish can pass
outside the receiver line (to the left of the first receiver or right of last
receiver) if outerLim[1] or outerLim[2] are greater than 0 meters. Each fish
starts and ends about 'maxDist' meters from the receiver line.

A simulated tag signal is transmitted every delayRng[1] to delayRng[2] seconds.
At time of each transmission, the distance is calculated between the tag and
each receiver, and rngFun is used to calculated the probability (p) that the
signal was detected on each receiver. Detection or non-detection on each
receiver is determined by a draw from a Bernoulli distribution with probability
p.
}
\value{
a data frame with
  \item{detProb}{proportion of simulated fish that were detected more than once on any single receiver}
}
\references{
None
}
\author{
C. Holbrook
}
\note{
TBD
}


\seealso{
TBD
}
\examples{

 library(glatos)

 #empirical detection range data
  edr <- data.frame(
    x=c(0,363,444,530,636,714,794,889,920), #tag-receiver distance
    p=c(1,1,0.96,0.71,0.67,0.75,0.88,0.21,0)) # detection prob

  #now create a function to return the detection probability
  # based on distance and linear interpolation within edr
  edrf <- function(dm, my.edr=edr) {
    p <- approx(x=my.edr$x,y=my.edr$p,xout=dm, rule=2)$y
    return(p)
  }

  #take a look at the detection range curve
  plot(edrf(0:2000),type="l")

  #two receivers; only 10 fish with plot "just to visualize the simulation"
  resAll1 <- receiverLineDetSim(rngFun=edrf,nsim=10, showPlot=T)

  #same but more receivers and allow fish to pass to left and right of line
  resAll2 <- receiverLineDetSim(rngFun=edrf, recSpc=rep(1000,5),
    outerLim=c(1000, 1000), nsim=10, showPlot=T)


  #define parametric (logistic) range curve
  # b = intercept and slope
  pdrf <- function(dm, b=c(5.5, -1/120)){
    p <- 1/(1+exp(-(b[1]+b[2]*dm)))
    return(p)
  }

  #take a look at the detection range curve
  plot(pdrf(0:2000),type="l")

  #two receivers; default values otherwise
  resAll3 <- receiverLineDetSim(rngFun=pdrf)

  #two receivers; allow fish to pass 1000 m to left and 2000 m to right of line
  resAll4 <- receiverLineDetSim(rngFun=pdrf,outerLim=c(1000, 2000))

  #four receivers; irregular spacing
  resAll5 <- receiverLineDetSim(recSpc=c(2000,4000,2000), rngFun=pdrf)

  #summarize detection probability as a function of receiver spacing
  #two receivers only
  spacing <- seq(100,5000, 100) #every 100 m from 100 to 5000 m
  for(i in 1:length(spacing)){
    if(i==1) resAll <- receiverLineDetSim(recSpc=spacing[i], rngFun=pdrf)
    if(i>1) resAll[i,] <- receiverLineDetSim(recSpc=spacing[i], rngFun=pdrf)
    if(i==length(spacing)) resAll$spacing <- spacing
  }

  plot(detProb~spacing, resAll, type="b")


  #summarize detection probability as a function of fish swim speed
  swim <- seq(0.1, 5.0, 0.1)
  for(i in 1:length(swim)){
    if(i==1) resAll <- receiverLineDetSim(vel=swim[i], rngFun=pdrf)
    if(i>1) resAll[i,] <- receiverLineDetSim(vel=swim[i], rngFun=pdrf)
    if(i==length(spacing)) resAll$swim <- swim
  }

  plot(detProb~swim, resAll, type="b")


  #summarize detection probability as a function of fish swim speed; TEN receivers
  swim <- seq(0.1, 5.0, 0.1)
  for(i in 1:length(swim)){
    if(i==1) resAll <- receiverLineDetSim(vel=swim[i], recSpc = rep(1000,9), rngFun=pdrf)
    if(i>1) resAll[i,] <- receiverLineDetSim(vel=swim[i], recSpc =  rep(1000,9), rngFun=pdrf)
    if(i==length(spacing)) resAll$swim <- swim
  }

  plot(detProb~swim, resAll, type="b", col="red")


}

