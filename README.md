MeHaloCoA
=========

R package dedicated to halogen-containing compounds detection in LC-hrMS

Installation
First install all dependencies as needed.

install.packages("devtools")
library(devtools)


source("http://bioconductor.org/biocLite.R")
biocLite()
library(BiocInstaller)
biocLite("xcms")
biocLite("CAMERA")

then install MeHaloCoA
install_github("yguitton/mehalocoa")
library(MeHaloCoA)
