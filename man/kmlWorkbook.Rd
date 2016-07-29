\name{kmlWorkbook}
\Rdversion{1.1}
\alias{kmlWorkbook}
\title{
Make a KML (or KMZ) file for viewing GLATOS receiver and release locations in Google Earth.
}
\description{
Receiver data (deployment location, deployment timestamp, and recovery timestamp) and tagging data (release location, release timestamp) are imported from a zipped GLATOS Workbook archive and used to make a KML (and optionally, KMZ) for viewing receiver deployments  and release locations in Google Earth.
}
\usage{
kmlWorkbook(zipFile,browse=F,kmz=F,labelSize=0.6,iconSize=0.6,showOngoing=T,endDate="2020-01-01")
}
\arguments{
  \item{zipfile}{full path and filename of zipped GLATOS workbook (this is the ZIPPED archive that gets uploaded to GLATOSWeb)}
  \item{browse}{if TRUE, user is asked to select zipfile using windows explorer; default=FALSE}
  \item{kmz}{if TRUE, a KMZ file (zipped KML file) will also be created; default=FALSE}
  \item{labelSize}{size of placemark labels (only shown when placemark is highlighted by user)}
  \item{iconSize}{size of icon for placements}
  \item{showOngoing}{logical; indicates if ongoing stations (missing recovery timestamp should be included}
  \item{endDate}{end date for any ongoing stations (if showOngoing == T)}
}
\details{
Receiver locations will be visible between deployment and recovery timestamps at each location. Release locations will be displayed when the display window includes the date of release.
}
\value{
A KML (and optionally, KMZ) file is written to the directory that contains the zipped GLATOS workbook. Nothing is returned to the R console.
}
\references{
www.data.glos.us/glatos
}
\author{
Chris Holbrook
}
\note{
}
\examples{
library(glatos)

#get path to example GLATOS Data Workbook
zipFile <- system.file("extdata", "SMRSL_GLATOS_20140828.xlsm.zip",package="glatos")
kmlWorkbook(zipFile,browse=F,kmz=T,labelSize=0.6,iconSize=0.6,showOngoing=T,endDate="2020-01-01")
}



