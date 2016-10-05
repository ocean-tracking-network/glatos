\name{vrl2csv}
\Rdversion{1.1}
\alias{vrl2csv}
\title{
Convert Vemco VRL file to CSV
}
\description{
Create a comma separate values (CSV) text file from a Vemco VRL file. 
}
\usage{
vrl2csv(vrlDir, vueExePath="C:\\Program Files (x86)\\Vemco\\VUE")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vrlDir}{
Path to directory containing VRL files
}
  \item{vueExePath}{
Path to VUE.exe (default may not always be correct)
}

}
\details{
The output CSV file will have same name as the VRL file. This invokes a system command in VUE (> 2.06; courtesy of Tim Stone, Vemco).  To get the path to VUE.exe, right click on the icon, select "Properties", and then copy text in "Target" box.
To create a CSV for each time-corrected VRL file, first time-correct the files using the VRL editor in VUE (under Tool menu).  To speed up that process, uncheck the "Import" checkbox next to each filename, then run VRLtoCSV to create a CSV for each edited (e.g. time corrected) VRL.
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
Receiver event data are not exported. That functionality is not supported by the VUE system command.
}


\seealso{
TBD
}
\examples{
}

