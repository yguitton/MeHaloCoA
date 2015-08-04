\name{cldetectSettings}
\alias{cldetectSettings}
\docType{data}
\title{
Example settings for MeHaloCoA Cl or Br detection in peaks
}
\description{
example settings needed for function \code{\link[MeHaloCoA]{cldetect}}, theses settings are mainly parameters for detection of typical isotopic profile and mass defect of Cl or Br
}
\usage{data(cldetectSettings)}
\format{
  The format is a \cr
List of 12
 $ outdir  : chr ""\cr
 $ plotps  : logi TRUE\cr
 $ deprof  : logi FALSE\cr
 $ Thresh   : num 30\cr
 $ m1      : num 1\cr
 $ m2      : num 2\cr
 $ mdiff   : num 0.05\cr
 $ ppmerr  : num 50\cr
 $ val     : chr "maxo"\cr
 $ mdiff2  : num 0.001\cr
 $ myfile2 : chr "halo_file_short.csv"\cr
}
\value{
Nothing
}
\seealso{
\code{\link{cldetect}}
}

\examples{
#load parameters
data(cldetectSettings)
#see all parameters
cldetectSettings

#if you want to change a parameter
cldetectSettings$Thresh<-90 #for Bromine


#if you want to create a complete list of parameters for Br
myparam<-list(outdir="",
				ppmerr=15,
				Thresh=90,
				m2=1.998,
				val="maxo",
				myfile2="Bromine_list.csv")

}
\keyword{datasets}
