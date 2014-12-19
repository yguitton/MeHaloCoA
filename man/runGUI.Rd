\name{runGUI}
\alias{runGUI}
\title{Launch the Graphical User Interface for MeHaloCoA}
\description{
Easy function for the GUI version of the MeHaloCoA package
}
\usage{runGUI()}
\details{
Just type runGC() and a Graphical User Interface appears (need tcltk). Tested under windows and Ubuntu.
}
\value{
A matrix with the Cl or Br peaks annotated. \cr  Draw nice Isotopic Filtered Chromatogram (IFC) summary plot where Cl or Br containing peaks are pointed out on the TIC chromatogram in a PDF file inside the Result directory
with \code{\link[MeHaloCoA]{runGUI}} type MeHaloCoAenv$an to see the CDF2RData object and type MeHaloCoAenv$matcl to see the result of cldetect
}
\author{
Yann GUITTON
}
\examples{
runGUI()
}
