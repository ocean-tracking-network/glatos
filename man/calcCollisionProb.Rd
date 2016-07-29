\name{calcCollisionProb}
\Rdversion{1.1}
\alias{calcCollisionProb}
\title{
Estimate probability of collision for PPM-type co-located telemetry transmitters
}
\description{
Estimate probability of collision for PPM-type co-located telemetry transmitters
}
\usage{
calcCollisionProb(delayRng = c(60, 180), burstDur = 5.0, maxTags = 50,
nTrans = 10000)
}
\arguments{
  \item{delayRng}{minimum and maximum delay in seconds}
  \item{burstDur}{duration of each coded burst (i.e., pulse train) in seconds}
  \item{maxTags}{maximum number of co-located tags (within detection range at
    same time)}
  \item{nTrans}{number of transmissions to simulate for each co-located tag}
}
\details{
Calculates the detection probability associated with collision, given delay
range (delayRng), burst duration (burstDur), maximum number of co-located tags
(maxTags), and number of simulated transmission per tag (nTrans). The
Simulation estimates detection probability due only to collisions (i.e., when
no other variables influence detection probability) and assuming that all tags
are co-located at a receiver for the duration of the simulation.
}
\value{
data frame containing summary statistics
\item{nTags}{Number of tags within detection range at one time}
\item{min}{Minimum detection probability among simulated tags}
\item{q1}{First quartile of detection probabilities among simulated tags}
\item{median}{Median detection probability among simulated tags}
\item{q3}{Third quartile of detection probabilities among simulated tags}
\item{max}{Maximum detection probability among simulated tags}
\item{mean}{Mean detection probability among simulated tags}
\item{expDetsPerHr}{Expected number of detections per hour assuming perfect detection probability, given the number of tags within detection range}
\item{totDetsPerHr}{Observed number of detections per hour for a given number of tags}
\item{effDelay}{Eeffective delay of the transmitter after incorporating collisions}
\item{detsPerTagPerHr}{Mean number of detections per hour per tag}
}
\references{
For application example, see:
Binder, T.R., Holbrook, C.M., Hayden, T.A. and Krueger, C.C., 2016. Spatial and
  temporal variation in positioning probability of acoustic telemetry arrays:
  fine-scale variability and complex interactions. Animal Biotelemetry, 4(1):1.
  http://animalbiotelemetry.biomedcentral.com/articles/10.1186/s40317-016-0097-4
}
\author{
C. Holbrook (cholbrook@usgs.gov) and T. Binder
}
\note{
TBD
}

\seealso{
TBD
}
\examples{
# parameters analagous to Vemco tag, global coding, 45 s nominal delay
foo <- calcCollisionProb(delayRng = c(45, 90), burstDur = 5.12, maxTags = 50,
    nTrans = 10000)
# plot probability of collision by subtracting detection probability from 1
plot(med~nTags, data=foo, type='p', pch=20, ylim=c(0,1),
  xlab="# of transmitters within range", ylab="Probability of collision")
}


