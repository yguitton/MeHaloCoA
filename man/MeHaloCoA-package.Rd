\name{MeHaloCoA-package}
\alias{MeHaloCoA-package}
\alias{MeHaloCoA}
\docType{package}
\title{
MarinE HALOgenated COmpound Analysis \cr
}
\description{
Fast search of compounds exhibiting chlorinated or brominated pattern (isotopic profile and mass defect) inside a hight-resolution LC-MS data set. 
\cr First Step is peakpicking with \code{\link[MeHaloCoA]{CDF2RData}}\cr
\cr Second Step is halogen Isotopic Profile search with \code{\link[MeHaloCoA]{cldetect}}
\cr \emph{All Steps can be done with ONE command line} \code{\link[MeHaloCoA]{runGUI}}
\cr\emph{NOTE} : with little modification you can search any kind of isotopic profile ([M+K]+ for example will have m2=1.998 and Thresh=7
}
\details{
\tabular{ll}{
Package: \tab MeHaloCoA\cr
Type: \tab Package\cr
Version: \tab 0.99.0\cr
Date: \tab 2014-12-09\cr
License: \tab GPL (>=2)\cr
}

}
\author{
Yann GUITTON, Catherine Roullier

Maintainer:Yann GUITTON <yann.guitton@gmail.com>

}
\references{
"Sleno, L., 2012. The use of mass defect in modern mass spectrometry: Mass defect in mass spectrometry. Journal of Mass Spectrometry 47, 226-236. doi:10.1002/jms.2953" \cr
"Wehrens, R., Weingart, G., Mattivi, F., n.d. metaMS: An open-source pipeline for GC-MS-based untargeted metabolomics. Journal of Chromatography B. doi:10.1016/j.jchromb.2014.02.051"

}
\examples{
runGUI()
## Not run:
\dontrun{
#Easy launch version, only ONE command line
library(MeHaloCoA)

#or
runGUI()


#command line version
library(MeHaloCoA)
#do the peak picking
data(xcmsSettings) #load peak picking parameters
an<-CDF2RData(indir = system.file("doc/mzData", package="MeHaloCoA"), settingslist=xcmsSettings)
#if indir="" interactive selection of directory

i=1 #just first file is processed with cldetect
data(cldetectSettings) #load halogens detection parameters
matcl<-do.call("cldetect", c(an[[i]],cldetectSettings))
}
}
\keyword{ package }

