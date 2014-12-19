\name{CDF2RData}
\alias{CDF2RData}
\title{
CDF2RData is a function to do peakpicking based on \emph{xcms and CAMERA}
}
\description{
\code{\link[MeHaloCoA]{CDF2RData}} is a function to do peakpicking based on \emph{xcms and CAMERA}, the base of the code is from R. Wehrens and was addapted for the \emph{MeHaloCoA} package
}
\usage{
CDF2RData(indir = "", outdir=NULL, files=NULL, settingslist)
}

\arguments{
  \item{indir}{
The directory where your converted LC-MS data are (accepted format are mzML, mzXML, mzData, CDF) see \emph{mzR package} for more details on formats \cr if indir="" (default) the selection of your data repository is interactive
}
  \item{outdir}{
	The directory where the results have to be saved, if NULL (default) your working directory is used
}
  \item{files}{
Default= NULL, if you don't want to process all the files in \emph{indir} you can use the \emph{files} parameter.
The path to the file(s) you want to be processed (accepted format are mzML, mzXML, mzData, CDF) see \emph{mzR package} for more details on formats.
e.g : files=c("c:/mydocs/file1.mzML", "c:/mydocs/files32.mzML")
}
  \item{settingslist}{
\code{settingslist} a peakpicking setting list as \code{\link{xcmsSettings}}.\cr
ex: mylist<-list(method="matchedFilter",ppm=15,mzdiff=0.05,rtrange=c(5,15), mzrange=c(100,500))
}
}

\value{
A list of xsAnnotate objects with each slot being the result of xcms and CAMERA peakpicking process, a slot named samples is added to that list with sample names, and an other names settings keep track of your peakpicking parameters
two files are saved in outdir: an.rda with xsAnnotate list and settings.rda with your settings
}
\references{
"Wehrens, R., Weingart, G., Mattivi, F., n.d. metaMS: An open-source pipeline for GC-MS-based untargeted metabolomics. Journal of Chromatography B. doi:10.1016/j.jchromb.2014.02.051"
}
\author{
Yann GUITTON
}
\note{
Many Thanks to R. Wherens for the starting codes
}
\seealso{
\code{\link{xcmsSettings}}
}

\examples{
runGUI()
\dontrun{
## Not run:
data(xcmsSettings)
an<-CDF2RData(indir = system.file("doc/mzData", package="MeHaloCoA"), settingslist=xcmsSettings)

## or with files option
data(xcmsSettings)
myfile<-file.path(system.file("doc/mzData", package="MeHaloCoA"),"Q-ToF_Data1.mzData")
an<-CDF2RData(files=myfile, settingslist=xcmsSettings)
}
}
