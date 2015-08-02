\name{cldetect.scan}
\alias{cldetect.scan}
\title{
Search inside peaks mass spectrum clues for the presence of Cl or Br with a scan by scan process
}
\description{
Fast search of compounds exhibiting chlorinated or brominated pattern (isotopic profile and mass defect) inside a hight-resolution LC-MS data set. No needs of peakpicking step
}
\usage{
cldetect.scan(outdir = "", file, noise=500, deprof = FALSE,  
 Thresh = 30, m1 = 1.003, m2 = 1.997, mdiff = 0.05, ppmerr = 25,  
 mdiff2 = 0.001, myfile = "halo_list.csv", myfile2 = "halo_list_short.csv",
 m1need=FALSE, N=NULL, scanrange=NULL)
}

\arguments{
  \item{outdir}{
your results directory
}
  \item{file}{
complete path to your file (mzXML, CDF, mzML)
}$  \item{noise}{
noise level above which ions (m/z) are considered in count
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
mdiff et mdiff2 differences in Da applied to neutral loss search and to IFC chromatogram respectively
}
  \item{ppmerr}{
ppm margin tolerated by \code{cldetect}
}

  \item{mdiff2}{
mdiff et mdiff2 differences in Da applied to neutral loss search and to IFC chromatogram respectively
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
  \item{N}{
 N is an option by default put to NULL, N is the number of scan that can be merged before calculation, if N=5, scan 1 & 2 (and the two last scans) are not modified but scans 3 is equal to the mean of scans 1+2+3+4+5. mean scan is calculated with plotSpec from xcms on the basis 
 of an xcmsRaw object with profstep=mzabs. Be careful this option generate a real slowing (in a future version we will add an nSlaves option). This option was added in order to re-create the same calculation as described in the Zhu et al paper. to the best of our knowledge the AMSA-IPF algorithm described is not available:
 "Zhu, P., Tong, W., Alton, K., Chowdhury, S., 2009. An Accurate-Mass-Based Spectral-Averaging Isotope-Pattern-Filtering Algorithm for Extraction of Drug Metabolites Possessing a Distinct Isotope Pattern from LC-MS Data. Analytical Chemistry 81, 5910-5917. doi:10.1021/ac900626d"
}
  \item{scanrange}{
 scanrange is a vector with start : end scan (eg 50:60, or c(50:60), by default NULL which means all scans
}
}

\value{
A matrix with the Cl or Br peaks annotated. \cr  Draw nice Isotopic Filtered Chromatogram (IFC) summary plot where Cl or Br containing peaks are pointed out on the TIC chromatogram in a PDF file inside the Result directory

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

#No peakpicking needed 
myfile=file.path(system.file("extdata", package="MeHaloCoA"),"Q-ToF_Data1.mzData")
result<-cldetect.scan(outdir=getwd(),file=myfile)
}
}

