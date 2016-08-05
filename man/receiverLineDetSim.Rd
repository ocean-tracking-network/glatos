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
  \item{recSpc}{numeric (vector); distance in meters between receivers; length of vector is N-1, where N is number of receivers; will simulate one receiver if recSpc = NA}
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
length(recSpc) + 1. Only one receiver will be simulated if recSpc = NA. 
Each fish moves at constant velocity (vel) along a line
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

 #EXAMPLE 1 - simulate detection on line of ten receivers
   
  #Define detection range function (to pass as rngFun) 
  # that returns detection probability for given distance
   # assume logistic form of detection range curve where 
   #   dm = distance in meters
   #   b = intercept and slope
  pdrf <- function(dm, b=c(5.5, -1/120)){
     p <- 1/(1+exp(-(b[1]+b[2]*dm)))
     return(p)
  }
 
  #preview detection range curve
  plot(pdrf(0:2000),type="l",ylab="Probability of detecting each coded burst", 
	xlab="Distance between receiver and transmitter")
 
  #Simulate detection using pdrf; default values otherwise
  dp <- receiverLineDetSim(rngFun=pdrf)
  dp
 
  #Again with only 10 virtual fish and optional plot to see simulated data
  dp <- receiverLineDetSim(rngFun=pdrf, nsim=10, showPlot=T) #with optional plot
  dp
 
  #Again but six receivers and allow fish to pass to left and right of line
  dp <- receiverLineDetSim(rngFun=pdrf, recSpc=rep(1000,5),
 	outerLim=c(1000, 1000), nsim=10, showPlot=T)
  dp
 
  #Again but four receivers with irregular spacing
  dp <- receiverLineDetSim(rngFun=pdrf, recSpc=c(2000,4000,2000),
  	outerLim=c(1000, 1000), nsim=10, showPlot=T)
  dp
 
 
 #EXAMPLE 2 - summarize detection probability as a function of receiver spacing
  
  #two receivers only, spaced 'spc' m apart
  #define scenarious where two receiver are spaced 
  spc <- seq(100,5000, 100) #two receivers spaced 100, 200, ... 5000 m
  #loop through scenarious, estimate detection probability for each
  for(i in 1:length(spc)){
    if(i==1) dp <- numeric(length(spc)) #preallocate
    dp[i] <- receiverLineDetSim(recSpc=spc[i], rngFun=pdrf)
  }
  cbind(spc,dp) #view results  
  #plot results
  plot(spc, dp, type="o",ylim=c(0,1), xlab="distance between receivers in meters",
	ylab="proportion of virtual fish detected") 
  # e.g., >95% virtual fish detected up to 1400 m spacing in this example
 
 
 #EXAMPLE 3 - summarize detection probability as a function of fish swim speed
  
  #define scenarios of fish movement rate
  swim <- seq(0.1, 5.0, 0.1) #constant velocity
  for(i in 1:length(swim)){
    if(i==1) dp <- numeric(length(swim)) #preallocate
    dp[i] <- receiverLineDetSim(vel=swim[i], rngFun=pdrf)
  }
  cbind(swim,dp) #view results
  #plot results
  plot(swim, dp, type="o", ylim=c(0,1), xlab="fish movement rate, m/s",
	ylab="proportion of virtual fish detected")
  # e.g., >95% virtual fish detected up to 1.7 m/s rate in this example
  # e.g., declines linearly above 1.7 m/s
 

 #EXAMPLE 4 - empirical detection range curve instead of logistic
  
  #create data frame with observed detection efficiency (p) at each distance (x)
  edr <- data.frame(
    x=c(0,363,444,530,636,714,794,889,920), #tag-receiver distance
    p=c(1,1,0.96,0.71,0.67,0.75,0.88,0.21,0)) # detection prob

  #now create a function to return the detection probability
  # based on distance and linear interpolation within edr
  # i.e., estimate p at given x by "connecting the dots" 
  edrf <- function(dm, my.edr=edr) {
    p <- approx(x=my.edr$x,y=my.edr$p,xout=dm, rule=2)$y
    return(p)
  }

  #preview empirical detection range curve
  plot(edrf(0:2000),type="l",ylab="probability of detecting each coded burst", 
	xlab="distance between receiver and transmitter, meters")

  #use empirical curve (edrf) in simulation
  dp <- receiverLineDetSim(rngFun=edrf, nsim=10, showPlot=T) #with optional plot
  dp

}

