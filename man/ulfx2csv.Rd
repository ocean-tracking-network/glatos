\name{ulfx2csv}
\Rdversion{1.1}
\alias{ulfx2csv}
\title{
Convert Vemco ULFx (Vemco XML) file to two CSV files (one with detection data, one with receiver event data)
}
\description{
Create two CSV files (one with detection data, one with receiver event data) from a ULFx (Vemco XML) file  
}
\usage{
ulfx2csv(ulfxFile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ulfxFile}{
Path and file name to ulfx file (or just file name if the file is in working directory)
}

}
\details{
The ULFx file is parsed into components and exported as CSV files matching VUE (v.2.2.7) default output. The output files are written to the same directory as the ULFx file, and '_detections.csv' and '_receiverEvents.csv' are appended to the VRL file name.
}
\value{
}
\references{
None
}
\author{
C. Holbrook
}
\note{
The following fields in detection data are not yet supported: transmitter_name, transmitter_serial, station_name.
This was developed and tested for VR2W receivers. No other receivers are supported, but if it is attempted, feedback is appreciated.
}


\seealso{
vrl2csv, vrl2ulfx
}
\examples{
library(glatos)

#get path to example VRL
myVRL <- system.file("extdata", "VR2W_109924_20110718_1.vrl",package="glatos")
vrl2ulfx(dirname(myVRL)) #output ULFx
#get path to ULFx
myULFx <- system.file("extdata", "VR2W_109924_20110718_1.ULFx",package="glatos")
ulfx2csv(myULFx) #output CSVs
}

