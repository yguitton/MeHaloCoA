cldetect.pcgroup <-
function(outdir="",xsAnnotate,pcgroup=c(1:2), plotps=TRUE, deprof=FALSE, Thresh=30, m1=1.0030, m2=1.997, mdiff=0.05, ppmerr=50, val="into", mdiff2=0.001, myfile="halo_list.csv",myfile2="halo_list_short.csv", m1need=FALSE){

 if (!class(xsAnnotate) == "xsAnnotate") {
        if(class(xsAnnotate)=="list"){
            if(length(which(lapply(an,function(x)class(x))=="xsAnnotate"))>=1){
                xsAnnotate<-xsAnnotate[[as.vector(which(lapply(an,function(x)class(x))=="xsAnnotate"))]]
            }
            else{
                stop("Parameter object is no xsAnnotate object\n")
            }
        }
        else{
            stop("Parameter object is no xsAnnotate object\n")
        }
        
        
    }

#find output directory
path<-outdir
    if ((path != "") == FALSE) {
        #require(tcltk)
        path=tclvalue(tkchooseDirectory(title="Please, select your Results directory"))
    }

an=xsAnnotate
an@pspectra<-an@pspectra[pcgroup]

#find pspectrum with 2 Da neutral loss for Chlorine, Bromine it is ok
hits <- findNeutralLossSpecs(an, mzdiff=m2, mzabs=mdiff, mzppm=ppmerr) 
good<-which(hits==TRUE)
#go to raw data to double check 
if (length(good)<length(an@pspectra)){
    hitraw<-findNeutralLoss.scan(file=an@xcmsSet@filepath, mzdiff=m2, mzabs=mdiff, mzppm=ppmerr) 
    if (length(hitraw)>length(hits)){
        print("additionnal spectra found!")
    }
}

#Find M+2 isotope and its intensity and compare to M+1 and M intensities into can be changed by maxo with val option


funM2<-function(target, massadduct1=m1, massadduct2=m2, mzabs=mdiff, ppmerr=ppmerr, Thresh=Thresh, val=val){

ls_halogenated<-list()
ls_Mlist<-list()
ls_test<-list()
M<-peak[which(peak[,"mz"]==target),val]
M1<-subset(peak,peak[,1]<=(ppm(target,ppmerr,l=TRUE)[1]+massadduct1) & (ppm(target,ppmerr,l=TRUE)[2]+massadduct1)<=peak[,1])#+ 10 ppm error pour le m+2 en limite haute
M2<-subset(peak,peak[,1]<=(ppm(target,ppmerr,l=TRUE)[1]+massadduct2) & (ppm(target,ppmerr,l=TRUE)[2]+massadduct2)<=peak[,1])



    if (length(M2[,"mz"])!=0){
    #Si M+1 et M+2 OK
        if ( length(M1[,"mz"])!=0){
                test2<-(peak[which(peak[,"mz"]==M2[,"mz"]),val]*100)/peak[which(peak[,"mz"]==target),val][1]
                test1<-(peak[which(peak[,"mz"]==M1[,"mz"]),val]*100)/peak[which(peak[,"mz"]==target),val][1]

            #list des test

#test1<150 is to avoid peaking M+1 isotopes as a M
                if ((test2>=Thresh) && (test1<150) && (test2<600)){
                        
                        if((M1[,"mz"]-target)[1]>=massadduct1-mzabs){
                            Halo1<-"OK"
                        }else
                        {
                            Halo1<-"No"
                        }

                        if((M2[,"mz"]-target)[1]<=massadduct2+mzabs){
                            Halo2<-"OK"
                        }else
                        {
                            Halo2<-"No"
                        }

                        if(Halo1=="OK" && Halo2=="OK" ){
                            Halo3<-"OK"
                        }else
                        {
                            Halo3<-"No"
                        }


                        if(Halo3=="OK" && test2>90 ){
                            Br<-"OK"
                        }else
                        {
                            Br<-"No"
                        }

                        if(Halo3=="OK" && test2>Thresh ){
                            Cl<-"OK"
                        }else
                        {
                            Cl<-"No"
                        }

                        RT<-round(median(getpspectra(an, grp=as.vector(good)[j])[, "rt"])/60,3)
                        if (Halo2=="OK"){
                            cat(j,good[j],RT,M,target,M1[,"mz"][1],M2[,"mz"][1],(M1[,"mz"]-target)[1],(M2[,"mz"]-target)[1],test1[1],test2[1],Halo3,Br,Cl,"\n", file = zz, sep = ",")
                            ls_halogenated<-good[j]
                        }
                }
        
        }
        
        if(m1need==FALSE){
        #Si M+2 OK mais pas M+1 (pas assez intense)
            if ( length(M1[,"mz"])==0){
                    test2<-(peak[which(peak[,"mz"]==M2[,"mz"]),val]*100)/peak[which(peak[,"mz"]==target),val][1]
                    test1<-"TOO LOW"

                #list des test


                    if ((test2>=Thresh) && (test2<600)){
                            
                            Halo1<-"M+1 Too Low"
                            Halo3<-"M+1 Too Low"
                            Br<-"M+1 Too Low"
                            Cl<-"M+1 Too Low"
                            
                            if((M2[,"mz"]-target)[1]<=massadduct2+mzabs){
                                Halo2<-"OK"
                            }else
                            {
                                Halo2<-"No"
                            }

                            RT<-round(median(getpspectra(an, grp=as.vector(good)[j])[, "rt"])/60,3)
                            if (Halo2=="OK"){
                                cat(j,good[j],RT,M,target,M1[,"mz"][1],M2[,"mz"][1],(M1[,"mz"]-target)[1],(M2[,"mz"]-target)[1],test1[1],test2[1],Halo3,Br,Cl,"\n", file = zz, sep = ",")                                
                               ls_halogenated<-good[j]
                           }
                    }
                    
            
            }
        }#end if M+1 available
    }    
    if (length(ls_halogenated)!=0){
        # reslist<-list("ls_halogenated"=ls_halogenated, "ls_Mlist"=ls_Mlist, "ls_test"=ls_test)
        # return(reslist)
        return(ls_halogenated)
    }

}# end funM2

#j in long = lenght good
zz<-file(file.path(path,myfile), "w")
cat("j","pcgroup","RT","Intensity","M","M1","M2","M1-M","M2-M","M1/M","M2/M","MassDefect filters OK","Br","Cl","\n", file = zz, sep = ",")
ls_good<-list()
for (j in 1:length(good)){
    ls_target<-list()
    peak<-getpspectra(an, good[j])
    #remove NaN containing lignes
    peak<-peak[complete.cases(peak), ]

    if (deprof==TRUE){
    options(warn=-1)
    if(require(RMassBank)){ 
        peak<-RMassBank::deprofile.scan(peak[,c("mz",val)])
        colnames(peak)<-c("mz",val)
        options(warn=0)
    }else{
        stop("Couldn't load RMassBank")
    }
    }

    #ls_target<-peak[sort.list(peak[,"into"], decreasing=TRUE), ][,1]
    ls_target<-peak[,1]

    ls_halogenated<-lapply(ls_target, function(x)funM2(target=x, massadduct1=m1, massadduct2=m2, mzabs=mdiff, ppmerr=ppmerr, Thresh=Thresh, val=val))

    #ls_halogenated
    ls_good[[j]]<-peak[which(match(ls_halogenated,good[j])==1),"mz"]
}
close(zz)
data<-read.table(file.path(path,myfile),header=TRUE, sep=",")
clvect<-unique(data[which(data[,"Cl"]=="OK"),"pcgroup"])
brvect<-unique(data[which(data[,"Br"]=="OK"),"pcgroup"])
ls_good2<-lapply(ls_good,function(x)length(x))
print(ls_good2)
if (plotps==TRUE && length(good[which(ls_good2>=1)])>=1){
    



#outdirbasicGCplot for outputs just to keep track off the name
outdirbasicGCplot<-file.path(path,paste("EICs_Results_",format(Sys.time(), "%Y_%B_%d_%Hh%M"),sep=""))
#plot pspectra and EICs for good peaks
    basicGCplot(outdir=path,xsAnnotate=an,pspecrange= as.vector(good[which(ls_good2>=1)]), check="")
    
#if bug change an@xcmsSet@filepaths<-"path to your  file"
#Plot IFC
    
    #matrix1 with ls_good list m/z +/-  mdiff2
    matrix1<-cbind((unlist(ls_good[which(ls_good2>=1)])-mdiff2),(unlist(ls_good[which(ls_good2>=1)])+mdiff2))
    
    mzrange<-matrix(ncol=2, data=matrix1)
    print(dim(matrix1))
    #1- faire max int de chque pcgroup (peut-etre lors des tests dans fichier final) attention xs = xcmsSet or cldetect accepte seulment le an voir getEICs from CAMERA
    peakT<-getPeaklist(an) #peak table
    
    malist<-NULL #start at NULL
    for (i in 1:dim(mzrange)[1]){
        malist<-c(malist,which(peakT[,"mz"]>mzrange[i,1] & peakT[,"mz"]<mzrange[i,2]))
        print(i) #debug
    }    
    
    EIC.matrix<-getAllPeakEICs(an,index=rep(1,nrow(an@groupInfo)))
    EIC.matrix$EIC[!is.finite(EIC.matrix$EIC)]<-0
    
    #gestion du cas ou is.null(dim(EIC.matrix$EIC[malist,])) ou colSums ne fonctionne pas
    if(!is.null(dim(EIC.matrix$EIC[malist,]))){
        plot(x=unlist(EIC.matrix$scantime),y=colSums(EIC.matrix$EIC[malist,])+max(colSums(EIC.matrix$EIC))*0.05, ylim=c(0,max(colSums(EIC.matrix$EIC))),type="l", col="red", main=paste("IFC for \n",basename(an@xcmsSet@filepaths)), xlab="seconds", ylab="Intensity") #IFC 
        points(x=unlist(EIC.matrix$scantime),y=colSums(EIC.matrix$EIC), type="l",col="grey") # TIC
        legend("topright", legend=c("IFC","TIC","Highly Interesting Point"), lty=1, lwd=2, col=c("red","grey","blue"), bty="n")
    
    }else
    {
        plot(x=unlist(EIC.matrix$scantime),y=colSums(EIC.matrix$EIC), type="l", col="grey", main=paste("IFC for \n",basename(an@xcmsSet@filepaths),"\n a problem exist!"), xlab="seconds", ylab="Intensity") #IFC 
    }

    #mat a refaire si plotps=False
    rt<-list()
    cl<-list()
    br<-list()
    for(i in 1:length(as.vector(good[which(ls_good2>=1)]))){
        rt[i] <- round(median(getpspectra(an, grp=as.vector(good[which(ls_good2>=1)])[i])[, "rt"])/60,3)
        cl[i]<-!is.na(match(as.vector(good[which(ls_good2>=1)])[i], clvect))
        br[i]<-!is.na(match(as.vector(good[which(ls_good2>=1)])[i], brvect))
    }

    mat<-data.frame(cbind(as.numeric(rt),cl,br,lapply(ls_good[which(ls_good2>=1)], FUN=function(x)paste0(round(as.numeric(x),3), collapse="|"))))
    colnames(mat)<-c("RT","Cl OK","Br OK","MZs")
    mat<-mat[sort.list(unlist(mat[,"RT"]),decreasing = FALSE), ]
    #gestion du cas ou length(which(mat[,"Cl OK"]=="TRUE"))<1
    if (length(which(mat[,"Cl OK"]=="TRUE"))>=1){
        arrows(as.numeric(mat[which(mat[,"Cl OK"]=="TRUE"),"RT"])*60, x1=as.numeric(mat[which(mat[,"Cl OK"]=="TRUE"),"RT"])*60, y0=max(colSums(EIC.matrix$EIC))*0.5, y1=0,col = "blue", lty = "solid", lwd = 2, length = 0.04, angle = 45, code = 1)
    }
}


if (length(good[which(ls_good2>=1)])>=1){
    cat("Nan Mais Halo Quoi!,\n Good News, putative chlorinated compound found \n")
    if (plotps==TRUE){
        print(paste("EICs and Mass Spectra are in ",outdirbasicGCplot, sep=""), sep="")
        return(mat)
    }else{
    #return(ls_good[which(ls_good2>=1)])
    #Now for RTs for all peaks in the whole an list()
    
    
        rt<-list()
        cl<-list()
        br<-list()
        for(i in 1:length(as.vector(good[which(ls_good2>=1)]))){
            rt[i] <- round(median(getpspectra(an, grp=as.vector(good[which(ls_good2>=1)])[i])[, "rt"])/60,3)
            cl[i]<-!is.na(match(as.vector(good[which(ls_good2>=1)])[i], clvect))
            br[i]<-!is.na(match(as.vector(good[which(ls_good2>=1)])[i], brvect))
        }

        mat<-data.frame(cbind(as.numeric(rt),cl,br,lapply(ls_good[which(ls_good2>=1)], FUN=function(x)paste0(round(as.numeric(x),3), collapse="|"))))
        colnames(mat)<-c("RT","Cl OK","Br OK","MZs")
        mat<-mat[sort.list(unlist(mat[,"RT"]),decreasing = FALSE), ]
        # mat<-as.data.frame(matrix(unlist(mat), ncol=4))
        # colnames(mat)<-c("RT","Cl OK","Br OK","MZs")
        # write.table(mat, file=file.path(path, myfile2), row.name=F)
    
        
        return(mat)
    
    }
    print("Putative chlorinated m/z are below")
    
}else
{
    cat(paste("Sorry, NO putative chlorinated compound found \n same player try again \n", sep=""))
    mat<-data.frame("No peak", "No peak","No peak", "No peak")
    colnames(mat)<-c("RT","Cl OK","Br OK","MZs")
    return(mat)
}

}
