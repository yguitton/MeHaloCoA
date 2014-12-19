\name{xcmsSettings}
\alias{xcmsSettings}
\docType{data}
\title{
Example settings for MeHaloCoA peakpicking
}
\description{
example settings needed for function \emph{CDF2RData}, theses settings are mainly parameters for peakpicking with \emph{xcms} and \emph{CAMERA}. for CAMERA only groupFWHM is used.
you can add mzrange for ex mzrange=c(100,1000) or/and rtrange (in minutes) rtrange=c(5,15) 
}
\usage{data(xcmsSettings)}
\format{
  The format is a list with any \code{\link[xcms:xcmsSet]{xcmsSet}} parameters and if wanted you can add perfwhm parameter from \code{\link[CAMERA]{groupFWHM}} value. e.g \code{mySettings<-list(fwhm=15,snthresh=3,step=0.01,mzdiff=0.05, perfwhm=0.6)}
}
\details{
The base of the settings are taken from \emph{metaMS} package see references
}
\source{
"Wehrens, R., Weingart, G., Mattivi, F., n.d. metaMS: An open-source pipeline for GC-MS-based untargeted metabolomics. Journal of Chromatography B. doi:10.1016/j.jchromb.2014.02.051"
}

\examples{
data(xcmsSettings)
#see all parameters
xcmsSettings

#if you want to change a parameter
xcmsSettings$perfwhm<-0.80

#if you want to add a new parameter
xcmsSettings$nSlaves<-1

#if you want to remove a parameter
xcmsSettings$nSlaves<-NULL


#if you want to create a complete list of parameters
myparam<-list(perfwhm=0.6,
				snthresh=3,
				max=50,
				step=0.01,
				steps=2,
				mzdiff=0.05,
				profmethod="bin",
				nSlaves=1)

}
\keyword{datasets}
