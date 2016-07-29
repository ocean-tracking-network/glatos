\name{crwInPolygon}
\Rdversion{1.3}
\alias{crwInPolygon}
\title{
Function to simulate a correlated random walk inside a polygon
}
\description{
Uses function crw to simulate a random walk as series of equal-length steps with
turning angles drawn from a normal distribution inside a polygon.
}
\usage{
crwInPolygon(polyg=data.frame(x=c(0,0,150000,150000),y=c(0,50000,50000,0)),
              theta=c(0,10),
              stepLen=100,
              initPos=c(NA,NA),
              initHeading=NA,
              nsteps=30)
}
\arguments{
  \item{polyg}{A polygon defined as data frame with columns x and y.}
Parameters passed to crw
  \item{theta}{Turn angle parameters (theta[1] = mean; theta[2] = sd) from normal distribution.}
  \item{stepLen}{Total distance moved in each step.}
  \item{initPos}{Inital position (initPos[1]=x, initPos[2]=y).}
  \item{initHeading}{Initial heading in degrees.}
  \item{nsteps}{Number of steps to simulate.}
}
\details{
If initPos = NA, then a starting point is randomly selected within the polygon
boundary. A path is simulated forward using the crw function. Initial heading is
also randomly selected if initHeading = NA. When a step crosses the polygon
boundary, a new heading for that step is drawn (from turn angle distribution
truncated by polygon boundary), and the remainder of the track is rotated
accordingly.
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
The path is constructed in 1000-step segments (currently hard coded at 1000).
In the event that the truncated turn angle distribution (at boundary) leaves
no possible solution (i.e., path has entered a crevice in the polygon boundary)
then the simulation will stop and an error will indicate that the path is
stuck at a boundary. Solution is to try simulation again or to simplify/smooth
polygon boundary.
}
\examples{
mypolygon <- data.frame(x=c(-50,-50, 50, 50),y=c(-50,50,50,-50))
foo <- crwInPolygon(mypolygon,theta=c(0,5), stepLen=10, initPos=c(0,0),
  initHeading=0, nsteps=50)
plot(foo,type="b",pch=20,cex=0.2,asp=c(1,1),xlim=range(mypolygon$x),
  ylim=range(mypolygon$y))
polygon(mypolygon)
}
