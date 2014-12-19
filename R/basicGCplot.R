basicGCplot <-
function(outdir="",xsAnnotate, pspecrange=NULL, check="")
{

path<-outdir
if ((path != "") == FALSE) {

path=tclvalue(tkchooseDirectory(title="Please, select your Result directory"))
}
outdir<-file.path(path,paste("EICs_Results_",format(Sys.time(), "%Y_%B_%d_%Hh%M"),sep=""))

dir.create(outdir, showWarnings = FALSE)

##good JPEG code for an = xsAnnotate
#list les pspectra avec plus d'une masse
#use an from annotate

an<-xsAnnotate
k<-NULL
i=1
size=length(an@pspectra) 

#remove pspectra with not enought fragments 
if (check!=""){
for (i in 1:size) {
    if (length(an@pspectra[[i]])>=check){ #10 for GC EI 2 for LC-MS
        k<-c(k,i)
    }#end if
}#end for
}else
{
k=c(1:size)
}

if  (is.null(pspecrange)){
for (i in 1:length(k)){
j=k[i]
jpeg(filename=file.path(outdir, paste("pspectra_pc",j,".JPG", sep="")), width=800)
 par(mfrow=c(1,2))
 plotEICs(an, pspec=j, maxlabel=2)
 plotPsSpectrum(an, pspec=j, maxlabel=2)
 dev.off()
} 
}else
{
for (i in pspecrange){
j=k[i]
jpeg(filename=file.path(outdir, paste("pspectra_pc",j,".JPG", sep="")), width=800)
 par(mfrow=c(1,2))
 plotEICs(an, pspec=j, maxlabel=2)
 plotPsSpectrum(an, pspec=j, maxlabel=2)
 dev.off()
} 
}
}
