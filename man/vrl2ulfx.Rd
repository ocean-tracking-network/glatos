\name{vrl2ulfx}
\Rdversion{1.1}
\alias{vrl2ulfx}
\title{
Convert Vemco VRL file(s) to ULFx (Vemco XML) format
}
\description{
Convert Vemco VRL file(s) to ULFx (Vemco XML) format by invoking a system command in VUE (> 2.06; courtesy of Tim Stone, Vemco). 
}
\usage{
vrl2ulfx(vrl, vueExePath="C:\\Program Files (x86)\\Vemco\\VUE")
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
If 'vrl' is a directory, then all VRL files in that directory will be converted to ULFx. Otherwise, only those files specified in 'vrl' will be converted. Each output ULFx file will have same name as its source VRL file. 
}
\value{
Vector of ULFx file names.
}
\references{
None
}
\author{
C. Holbrook
}
\note{
To get the path to VUE.exe, right click on the icon, select "Properties", and then copy text in "Target" box.
To create a ULFX for each time-corrected VRL file, first time-correct the files using the VRL editor in VUE (under Tool menu).  To speed up that process, uncheck the "Import" checkbox next to each filename, then run VRLtoCSV to create a CSV for each edited (e.g. time corrected) VRL.
}


\seealso{
vrl2csv, ulfx2csv
}
\examples{
myVRL <- system.file("extdata", "VR2W_109924_20110718_1.vrl",package="glatos")
vrl2ulfx(dirname(myVRL)) #file name input
vrl2ulfx(myVRL) #directory input
}

