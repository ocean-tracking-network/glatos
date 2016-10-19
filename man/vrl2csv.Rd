\name{vrl2csv}
\Rdversion{1.1}
\alias{vrl2csv}
\title{
Convert Vemco VRL file(s) to CSV format
}
\description{
Convert Vemco VRL file(s) to CSV format by invoking a system command in VUE (> 2.06; courtesy of Tim Stone, Vemco). 
}
\usage{
vrl2csv(vrl, vueExePath="C:\\Program Files (x86)\\Vemco\\VUE")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vrl}{
string (or vector of strings) containing names of VRL files or a single directory containing VRL files)
}
  \item{vueExePath}{
Path to VUE.exe (default may not always be correct)
}

}
\details{
If 'vrl' is a directory, then all VRL files in that directory will be converted to CSV. Otherwise, only those files specified in 'vrl' will be converted. Each output CSV file will have same name as its source VRL file. 
}
\value{
Vector of csv file names.
}
\references{
None
}
\author{
C. Holbrook
}
\note{
Receiver event data are not exported because that functionality is not supported by the VUE system command.

To get the path to VUE.exe, right click on the icon, select "Properties", and then copy text in "Target" box.
 
To create a CSV for each time-corrected VRL file, first time-correct the files using the VRL editor in VUE (under Tools menu).  To speed up that process, uncheck the "Import" checkbox next to each filename, then run vrl2csv to create a CSV for each edited (e.g. time corrected) VRL.
}


\seealso{
vrl2ulfx, ulfx2csv
}
\examples{
#get path to example VRL
myVRL <- system.file("extdata", "VR2W_109924_20110718_1.vrl",package="glatos")
vrl2csv(dirname(myVRL)) #file name input
vrl2csv(myVRL) #directory input
}

