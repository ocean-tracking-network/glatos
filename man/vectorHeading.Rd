\name{vectorHeading}
\Rdversion{1.1}
\alias{vectorHeading}
\title{
Function to calculate direction (heading) of a vector (in degrees)
}
\description{
Calculates direction (in degrees) of a vector.
}
\usage{
vectorHeading(x,y)
}
\arguments{
  \item{x}{ vector of x coordinates; minimum of 2 }
  \item{y}{ vector of y coordinates; minimum of 2 }
}
\details{
Calculates direction (in degrees) for each of k-1 vectors, 
where k = length(x) - 1 = length(y). 
Lenghts of x and y must be equal.
}
\value{
heading in degrees (or vector of headings if length(x) > 2)
}
\author{
C. Holbrook (cholbrook@usgs.gov)
}
\examples{
x=c(2,4)
y=c(2,4)
vectorHeading(x, y)

x2=c(2,4,2)
y2=c(2,4,2)
vectorHeading(x2, y2)
}