\name{cldetect.pcgroup}
\alias{cldetect.pcgroup}
\title{
Search inside pseudospectrum  clues for the presence of Cl or Br 
}
\description{
Fast search of compounds exhibiting chlorinated or brominated pattern (isotopic profile and mass defect) inside a hight-resolution LC-MS data set. After pseudospectrum creation \cr
by CDF2RData or with xcms/CAMERA packages
}
\usage{
cldetect.pcgroup(outdir="",xsAnnotate,pcgroup=c(1:2), plotps=TRUE, deprof=FALSE,
Thresh=30, m1=1.0030, m2=1.997, mdiff=0.05, ppmerr=50, val="into",
 mdiff2=0.001,  myfile="halo_list.csv",myfile2="halo_list_short.csv", m1need=FALSE)
}

\arguments{
  \item{outdir}{
your results directory
}
  \item{xsAnnotate}{
object of class xsAnnoatet made by \code{CDF2RData} or by CAMERA an<-xsannotate
}
  \item{pcgroup}{
a number or a vector with the pcgroup that should be processed
}
 \item{plotps}{
Plot pseudospectra and EICs for each considered peaks in jpeg format the result repository
default = TRUE. Put FALSE if you have error message like could not find file type (you may have moved your original datas)
}
  \item{deprof}{
default=FALSE if TRUE \code{\link[RMassBank]{deprofile.scan}} from RMassBank is applied on each pspectra, mainly used if your LC-MS data are not well centroided by your file converter
}

  \item{Thresh}{
Threshold of intensity for M+2 here 30% for Cl (Use 90 % for Br). 
}
  \item{m1}{
mass defect between M and M+1 isotope (here 1.0033)
}
  \item{m2}{
mass defect between M and M+2 isotope (here 1.997 for Cl)
}
  \item{mdiff}{
mdiff differences in Da applied to neutral loss search
}
  \item{ppmerr}{
ppm margin tolerated by \code{cldetect}
}
  \item{val}{
val for use of into or maxo in funM2 and deprofile.scan
}
  \item{mdiff2}{
mdiff2 differences in Da applied  to IFC chromatogram 
}
  \item{myfile}{
name of the output file default "halo_list.csv"
}
  \item{myfile2}{
 name of the output file default "halo_list_short.csv" is a resume from all individual \code{myfile} in the case off several files have been processed
}
  \item{m1need}{
 Should M+1 isotope be present? if FALSE (Default) peaks with no M+1 are considered (interesting for low intensity compounds). If TRUE only complete isotopic profiles are taken into account.
}

}

\value{
A matrix with the Cl or Br peaks annotated. \cr  Draw nice Isotopic Filtered Chromatogram (IFC) summary plot where Cl or Br containings peaks are pointed out on the TIC chromatogram in a PDF file inside the Result directory

}
\references{
"Sleno, L., 2012. The use of mass defect in modern mass spectrometry: Mass defect in mass spectrometry. Journal of Mass Spectrometry 47, 226-236. doi:10.1002/jms.2953" \cr
"Wehrens, R., Weingart, G., Mattivi, F., n.d. metaMS: An open-source pipeline for GC-MS-based untargeted metabolomics. Journal of Chromatography B. doi:10.1016/j.jchromb.2014.02.051"
}
\author{
Yann GUITTON
}


\examples{
runGUI()
## Not run:
\dontrun{
library(MeHaloCoA)
data(xcmsSettings) #load peak picking parameters
data(cldetectSettings) #load halogens detection parameters

myfile<-file.path(system.file("extdata", package="MeHaloCoA"),"Q-ToF_Data1.mzData")
an<-CDF2RData(files=myfile, settingslist=xcmsSettings)
result<-cldetect.pcgroup(outdir=getwd(),xsAnnotate=an[[1]],pcgroup=1, plotps=TRUE, deprof=FALSE,
  Thresh=30, m1=1.0030, m2=1.997, mdiff=0.05, ppmerr=50, val="into",
 mdiff2=0.001,  myfile="halo_list.csv",myfile2="halo_list_short.csv", m1need=FALSE)
}
}

