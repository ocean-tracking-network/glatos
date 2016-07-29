\name{detectTransmissions}
\Rdversion{1.1}
\alias{detectTransmissions}
\title{
Simulate detection of transmitter signals in a receiver network
}
\description{
Simulates detection of transmitter signals in a receiver network based on
detection range curve (detection probability as a function of distance),
location of transmitter, and location of receivers.
}
\usage{
detectTransmissions(trnsLoc=NA,recLoc=NA,detRngFun=NA)
}
\arguments{
  \item{trnsLoc}{ data frame with locations (columns x, y) and times (column et)
   where signals were transmitted }
  \item{recLoc}{ data frame with receiver locations (columns x, y) }
  \item{detRngFun}{ a function that defines detection range curve;
  must return a vector of detection probabilities given distance }
}
\details{
Distances between each signal transmission location and receiver are culculated
using pythagorean theorem. Probability of detecting each signal on each receiver
is determined from the detection range curve. Detection of each signal on each
receiver is determined stochastically by draws from a Bernoulli distribution
with probability p (detection prob).
}
\value{
  \item{dtc}{Simulated detection data with unique signal transmission id (column
  trns_id), unique receiver id (column recv_id), receiver locations (columns
  recv_x, recv_y), signal transmision locations (columns trns_x, trns_y), and
  elapsed time (column etime)}
}
\author{
C. Holbrook (cholbrook@usgs.gov)
}
\note{
none
}

\examples{
TBD
}