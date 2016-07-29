\name{crw}
\Rdversion{1.1}
\alias{crw}
\title{
Function to simulate a correlated random walk
}
\description{
Simulate a random walk as series of equal-length steps with turning angles 
drawn from a normal distribution.
}
\usage{
crw(theta=c(0,5), stepLen=10, initPos=c(0,0), initHeading=0, nsteps=10000)
}
\arguments{
  \item{theta}{Turn angle parameters (theta[1] = mean; theta[2] = sd) from normal distribution.}
  \item{stepLen}{Total distance moved in each step.}
  \item{initPos}{Inital position (initPos[1]=x, initPos[2]=y).}
  \item{initHeading}{Initial heading in degrees.}
  \item{nsteps}{Number of steps to simulate.}
}
\details{
First, nsteps turn angles are drawn from a normal distribution. Second, the cumulative sum of the
vector of turn angles defines the heading within each step. The x and y component vectors in each 
are then calculated and summed to obtain the simualted path.
}
\value{
A two-column data frame
\item{x}{x coordinates}
\item{y}{y coordinates}
}
\author{
C. Holbrook (cholbrook@usgs.gov) 
}
\note{
adapted from code provided by Tom Binder
}
\examples{
foo <- crw(theta=c(0,5), stepLen=10, initPos=c(0,0), initHeading=0, nsteps=10)
plot(foo,type="b",pch=20,cex=0.2,asp=c(1,1)) 
}