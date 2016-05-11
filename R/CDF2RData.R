#Version 3 June 2014 with file selection option not only all indir compatible files
#and do.call in order to allow all possible parameters for xcmsSet if they are in settingslist
#with mzrange or/and rtrange option
#Much of that code is from R. Wehrens 

CDF2RData <-function(indir="",outdir=NULL,files=NULL, settingslist) {
if(is.null(indir)|| indir=="" && !is.null(files)){
    
    path<-unique(dirname(files))[1] #only first path is used
    samples<-files
    print("Your sample(s)")
    print(samples)
}
if(is.null(indir)|| indir!="" && is.null(files)){
    print(paste("Your Data directory is:",indir))
    path<-indir    
    #selection of your files cdf, mzXML, mzML, mzData
    filepattern <- c("[Cc][Dd][Ff]", "[Nn][Cc]", "([Mm][Zz])?[Xx][Mm][Ll]","[Mm][Zz][Dd][Aa][Tt][Aa]", "[Mm][Zz][Mm][Ll]")
    filepattern <- paste(paste("\\.", filepattern, "$", sep = ""),collapse = "|") 
    samples<-list.files(path=path, pattern=filepattern, all.files=FALSE,recursive=TRUE,full.names=TRUE,ignore.case=FALSE)
    print("Your sample(s)")
    print(samples)
}
if(is.null(indir)|| indir=="" && is.null(files)){
    print(paste("Your Data directory is:",indir))
    path<-indir    
    
    if ((path != "") == FALSE){
        path=tclvalue(tkchooseDirectory(title="Please, select your Data directory"))
    }
    #selection of your files cdf, mzXML, mzML, mzData
    filepattern <- c("[Cc][Dd][Ff]", "[Nn][Cc]", "([Mm][Zz])?[Xx][Mm][Ll]","[Mm][Zz][Dd][Aa][Tt][Aa]", "[Mm][Zz][Mm][Ll]")
    filepattern <- paste(paste("\\.", filepattern, "$", sep = ""),collapse = "|") 
    samples<-list.files(path=path, pattern=filepattern, all.files=FALSE,recursive=TRUE,full.names=TRUE,ignore.case=FALSE)
    print("Your sample(s)")
    print(samples)
}

if(is.null(outdir)|| outdir==""){
    print(paste("Your Result directory is:",getwd()))
}else
{
    print(paste("Your Result directory is:",outdir))    
}

print("Your settings")
print(settingslist)



if(any(which(!is.na(match(c("rtrange","mzrange","perfwhm"),names(settingslist)))))){
  ## xset is a list of xcmsSet objects
  capture.output(xset <- lapply(samples,
                                function(x)
                                do.call(xcmsSet,c(files=x,settingslist[-which(names(settingslist)=="rtrange" | names(settingslist)=="mzrange"|names(settingslist)=="perfwhm")]))),
                 file = NULL)
    
##add rtrange, mzrange avec un filtre sur xset
#xset2 <- xset[which(xset@peaks[,"mz"]<600 & which(xset@peaks[,"rt"]>100),]
#
if(!is.na(match("rtrange",names(settingslist)))){
    xset1 <- lapply(xset,FUN=function(x){x@peaks<-x@peaks[which(x@peaks[,"rt"]<60*settingslist$rtrange[2] & x@peaks[,"rt"]>60*settingslist$rtrange[1]),]})
    for (i in 1:length(xset)){
            xset[[i]]@peaks<-xset1[[i]]
        }
    rm(xset1)
}
if(!is.na(match("mzrange",names(settingslist)))){
    xset1 <- lapply(xset,FUN=function(x){x@peaks<-x@peaks[which(x@peaks[,"mz"]<settingslist$mzrange[2] & x@peaks[,"mz"]>settingslist$mzrange[1]),]})
    for (i in 1:length(xset)){
            xset[[i]]@peaks<-xset1[[i]]
        }
    rm(xset1)
    assign("xset",xset,envir = as.environment(MeHaloCoAenv))
}
  an<-lapply(xset,
         function(x) {
           y <- xsAnnotate(x, sample = 1)
           capture.output(z <- groupFWHM(y, perfwhm = settingslist$perfwhm),
                          file = NULL)
           z})
}
else{
     ## xset is a list of xcmsSet objects
     capture.output(xset <- lapply(samples,
                                    function(x)
                                    do.call(xcmsSet,c(files=x,settingslist))),
                     file = NULL)
    ##add rtrange, mzrange avec un filtre sur xset
    #xset2 <- xset[which(xset@peaks[,"mz"]<600 & which(xset@peaks[,"rt"]>100),]
    #
    
    if(!is.na(match("rtrange",names(settingslist)))){
        xset1<- lapply(xset,FUN=function(x){x@peaks<-x@peaks[which(x@peaks[,"rt"]<60*settingslist$rtrange[2] & x@peaks[,"rt"]>60*settingslist$rtrange[1]),]})
        for (i in 1:length(xset)){
            xset[[i]]@peaks<-xset1[[i]]
        }
        
        rm(xset1)
    }
    if(!is.na(match("mzrange",names(settingslist)))){
        xset1 <- lapply(xset,FUN=function(x){x@peaks<-x@peaks[which(x@peaks[,"mz"]<settingslist$mzrange[2] & x@peaks[,"mz"]>settingslist$mzrange[1]),]})
        for (i in 1:length(xset)){
            xset[[i]]@peaks<-xset1[[i]]
        }
        rm(xset1)
        assign("xset",xset,envir = as.environment(MeHaloCoAenv))
    }
      an<-lapply(xset,
             function(x) {
               y <- xsAnnotate(x, sample = 1)
               capture.output(z <- groupFWHM(y),
                              file = NULL)
     z})
}           

if(is.null(outdir)|| outdir==""){       
        outdir<-file.path(getwd(),paste("Results_",format(Sys.time(), "%Y_%B_%d_%Hh%M"),sep=""))
        dir.create(outdir)
    }
    else{
        outdir<-file.path(outdir,paste("Results_",format(Sys.time(), "%Y_%B_%d_%Hh%M"), sep=""))
        dir.create(outdir)
}
    
an$settings<-settingslist


an$samples<-samples
save(an, file=file.path(outdir,"an.rda"))
save(settingslist, file=file.path(outdir,"settingslist.rda"))

print(paste("Results saved in ", outdir, "/an.rda file", sep=""))
return(an)
}
