\name{rotatePoints}
\Rdversion{1.1}
\alias{rotatePoints}
\title{
Function to rotate points around a point in a 2-d plane
}
\description{
Rotates points around a point in a 2-d plane
}
\usage{
rotatePoints(x,y,theta,focus)
}
\arguments{
  \item{x}{ vector of x coordinates; minimum of 2 }
  \item{y}{ vector of y coordinates; minimum of 2 }
  \item{theta}{ angle of rotation in degrees; positive is clockwise }
  \item{focus}{ vector of x, y coordinates for the point around with x and y
  points with rotate }
}
\details{
Points are shifted to be centered at the focus, then rotated using a
rotation matrix, then shifted back to original focus.
}
\value{
a two-column data frame with rotated x, y coordinates
}
\author{
C. Holbrook (cholbrook@usgs.gov)
}
\examples{
x <- runif(10,0,10)
y <- runif(10,0,10)
plot(x,y,type="b",pch=20)
foo <- rotatePoints(x, y, 20, c(5, 5))
points(foo$x,foo$y,type="b",pch=20,col="red")
}
