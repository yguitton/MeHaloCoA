#Code modified from 
#http://stackoverflow.com/questions/23862306/stop-r-from-overwriting-graphics-files

projectpdf <- function(projectName, dir,ext="pdf", ...) {
  # get a list of existing plots for the project
    plots <- list.files(path=dir, pattern=paste0(projectName))
  # pull out numeric component of the plot files
    nums <- as.numeric(gsub(paste(projectName, paste(".",ext,sep=""), sep="|"), "", plots))
  if(length(nums)==0){
    last<-0
    }else
    {
    last <- max(nums)
  }
  if(is.na(last) ){last<-0}
  # Create a new file name with an incremented counter.
    newFile <- paste0(projectName, sprintf("%03d", last + 1), paste(".", ext,sep=""))
  # now call pdf
  if(ext=="pdf"){
    pdf(file.path(dir, newFile), ...)
  }
   if(ext=="png"){
    png(file.path(dir, newFile), ...)
  } 
  if(ext=="jpeg"){
    jpeg(file.path(dir, newFile), ...)
  }
  
}


#From RMassBank Package and to avoid dependencies just for that function
#' Calculate ppm values
#' 
#' Calculates ppm values for a given mass.
#' 
#' This is a helper function used in RMassBank code.
#' 
#' @param mass The "real" mass
#' @param dppm The mass deviation to calculate
#' @param l Boolean: return limits? Defaults to FALSE.
#' @param p Boolean: return ppm error itself? Defaults to FALSE.
#' @return By default (\code{l=FALSE, p=FALSE}) the function returns the mass plus the 
#' ppm error (for 123.00000 and 10 ppm: 123.00123, or for 123 and -10 ppm: 
#' 122.99877).
#' 
#' For \code{l=TRUE}, the function returns the upper and lower limit (sic!)
#' For \code{p=TRUE}, just the difference itself is returned (0.00123 for 123/10ppm).
#' @examples ppm(100, 10)
#' @author Michael A. Stravs, Eawag <michael.stravs@@eawag.ch>
#' @export
ppm <- function(mass, dppm, l=FALSE, p=FALSE)
{
    if(p) return(mass*dppm*1e-6)
    dmass <- mass * (1 + dppm*1e-6)
    if(l) dmass <- c(dmass, mass * (1 - dppm*1e-6))
    return(dmass)
}

## # auxiliaries
## emass <- 0.0005485799
## pmass <- 1.007276565
## hmass <- 1.007825