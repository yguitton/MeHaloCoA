#cldetect for scan by scan  calculation

cldetect.scan<-
function(outdir="",file, noise=500,  deprof=FALSE,  Thresh=30, m1=1.0030, m2=1.997, mdiff=0.05, ppmerr=25,  mdiff2=0.001, myfile="halo_list.csv",
myfile2="halo_list_short.csv", m1need=FALSE, N=NULL, scanrange=NULL){
#from cldetect just for compatibility
plotps=FALSE
val="into"

#find output directory
path<-outdir
    if ((path != "") == FALSE) {
        #require(tcltk)
        path=tclvalue(tkchooseDirectory(title="Please, select your Results directory"))
    }

#find pspectrum with 2 Da neutral loss for Chlorine, Bromine it is ok
hits <-.findNeutralLoss.scan(file=file, mzdiff=m2, mzabs=mdiff, noise=noise, mzppm=ppmerr, N=N, scanrange=scanrange) 
if (!is.null(N)){
    good<-which(hits$hits==TRUE)
    #debug
    #print(good)
}else
{
    good<-which(hits==TRUE)
}
#open file, need mzR for processing
    object <- openMSfile(file)
    
#Find M+2 isotope and its intensity and compare to M+1 and M intensities into can be changed by maxo with val option
funM2<-function(target, massadduct1=m1, massadduct2=m2, mzabs=mdiff, ppmerr=ppmerr, Thresh=Thresh, val=val){

ls_halogenated<-list()
ls_Mlist<-list()
ls_test<-list()
M<-peak[which(peak[,"mz"]==target),val] #intensity of M
M1<-subset(peak,peak[,1]<=(ppm(target,ppmerr,l=TRUE)[1]+massadduct1) & (ppm(target,ppmerr,l=TRUE)[2]+massadduct1)<=peak[,1])#+ 10 ppm error pour le m+2 en limite haute
M2<-subset(peak,peak[,1]<=(ppm(target,ppmerr,l=TRUE)[1]+massadduct2) & (ppm(target,ppmerr,l=TRUE)[2]+massadduct2)<=peak[,1])


    if (length(M2[,"mz"])!=0){

#test
if ((length(M1[,"mz"])>1) || (length(M2[,"mz"])>1)){ 
    
    print(paste("For m/z target=",target, "several M+1 or M+2 found", sep=" "))
    print(paste("M1",M1[,"mz"], sep=" "))
    print(paste("M2",M2[,"mz"], sep=" "))
    stop("WARNING: should consider lowering ppmerr value",call.=FALSE)
}
#test fin

    #Si M+1 et M+2 OK
        if ( length(M1[,"mz"])!=0){
                test2<-(peak[which(peak[,"mz"]==M2[,"mz"]),val]*100)/M[1]
                test1<-(peak[which(peak[,"mz"]==M1[,"mz"]),val]*100)/M[1]

            #list des test


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

                        #RT<-round(median(getpspectra(an, grp=as.vector(good)[j])[, "rt"])/60,3)
                        # cat("j","scan","RT","M","M1","M2","M1-M","M2-M","M1/M","M2/M","Mass defect filters OK","Br","Cl","\n", file = zz, sep = ",")
                            if (!is.null(scanrange)){
                                start1=min(scanrange)
                                RT<-round( header(object, scans=target)$retentionTime/60,3)
                                if (Halo2=="OK"){
                                    cat(j,start1+good[j],RT,M,target,M1[,"mz"][1],M2[,"mz"][1],(M1[,"mz"]-target)[1],(M2[,"mz"]-target)[1],test1[1],test2[1],Halo3,Br,Cl,"\n", file = zz, sep = ",")
                                     ls_halogenated<-good[j]
                                }
                            }else
                            {
                                RT<-round( header(object, scans=target)$retentionTime/60,3)
                                if (Halo2=="OK"){
                                    cat(j,good[j],RT,M,target,M1[,"mz"][1],M2[,"mz"][1],(M1[,"mz"]-target)[1],(M2[,"mz"]-target)[1],test1[1],test2[1],Halo3,Br,Cl,"\n", file = zz, sep = ",")
                                     ls_halogenated<-good[j]
                                }
                            }
                      
                }
        
        }
        
        
        if (m1need==FALSE){
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
                            #gestion of scanrange not null
                            if (!is.null(scanrange)){
                                start1=min(scanrange)
                                RT<-round( header(object, scans=target)$retentionTime/60,3)
                                if (Halo2=="OK"){
                                    cat(j,start1+good[j],RT,M,target,M1[,"mz"][1],M2[,"mz"][1],(M1[,"mz"]-target)[1],(M2[,"mz"]-target)[1],test1[1],test2[1],Halo3,Br,Cl,"\n", file = zz, sep = ",")
                                    ls_halogenated<-good[j]
                                }
                                }else
                            {
                                RT<-round( header(object, scans=target)$retentionTime/60,3)
                                if (Halo2=="OK"){
                                    cat(j,good[j],RT,M,target,M1[,"mz"][1],M2[,"mz"][1],(M1[,"mz"]-target)[1],(M2[,"mz"]-target)[1],test1[1],test2[1],Halo3,Br,Cl,"\n", file = zz, sep = ",")
                                    ls_halogenated<-good[j]
                                }
                            }
                                
                           ls_halogenated<-good[j]
                    }
            
            }
        }#end m1need
    }    
    if (length(ls_halogenated)!=0){
        # reslist<-list("ls_halogenated"=ls_halogenated, "ls_Mlist"=ls_Mlist, "ls_test"=ls_test)
        # return(reslist)
        return(ls_halogenated)
    }

}# end funM2


zz<-file(file.path(path,myfile), "w")
cat("j","scan","RT","Intensity","M","M1","M2","M1-M","M2-M","M1/M","M2/M","Mass defect filters OK","Br","Cl","\n", file = zz, sep = ",")
ls_good<-list()

for (j in 1:length(good)){
print(j)
    ls_target<-list()
    if ( !is.null(N)){
        peak<-hits$mz[[good[j]]]
    }else
    {
        if ( !is.null(scanrange)){
            peak<-mzR::peaks(object, scans=min(scanrange)-1+good[j])
        }else
        {
            peak<-mzR::peaks(object, scans=good[j])
        }
    }
    #remove NaN containing lignes

    peak<-peak[complete.cases(peak), ]
    


    if (deprof==TRUE){
        options(warn=-1)
        if(require(RMassBank)){ 
            peak<-RMassBank::deprofile.scan(peak[,1,2])
            colnames(peak)<-c("mz",val)
            options(warn=0)
        }else{
            stop("Couldn't load RMassBank")
        }
    }

    #ls_target<-peak[sort.list(peak[,"into"], decreasing=TRUE), ][,1]
    colnames(peak)<-c("mz",val)
    ls_target<-peak[,1]
    
    ls_halogenated<-lapply(ls_target, function(x)funM2(target=x, massadduct1=m1, massadduct2=m2, mzabs=mdiff, ppmerr=ppmerr, Thresh=Thresh, val=val))


    #ls_halogenated
    ls_good[[j]]<-peak[which(match(ls_halogenated,good[j])==1),"mz"]
}
close(zz)
data<-read.table(file.path(path,myfile),header=TRUE, sep=",")
clvect<-unique(data[which(data[,"Cl"]=="OK"),"scan"])
brvect<-unique(data[which(data[,"Br"]=="OK"),"scan"])
ls_good2<-lapply(ls_good,function(x)length(x))
print(ls_good2)


# Plot IFC
if (length(good[which(ls_good2>=1)])>=1){
    
        rt<-list()
        int<-list()
        cl<-list()
        br<-list()
        for(i in 1:length(as.vector(good[which(ls_good2>=1)]))){
            if (!is.null(scanrange)){
                start1=min(scanrange)
                rt[[i]] <-round( header(object, scans=as.vector(start1+good[which(ls_good2>=1)])[i])$retentionTime/60,3)
                int[[i]]<-colSums(peaks(object, scans=as.vector(start1+good[which(ls_good2>=1)])[i]))[2]
            }else
            {
                rt[[i]] <-round( header(object, scans=as.vector(good[which(ls_good2>=1)])[i])$retentionTime/60,3)
                int[[i]]<-colSums(peaks(object, scans=as.vector(good[which(ls_good2>=1)])[i]))[2]
            }
            cl[[i]]<-!is.na(match(as.vector(good[which(ls_good2>=1)])[i], clvect))
            br[[i]]<-!is.na(match(as.vector(good[which(ls_good2>=1)])[i], brvect))
        }
        plot(x=as.numeric(unlist(rt)),y=as.numeric(unlist(int)), type="h", col="red", main=paste("IFC for \n",basename(file)), xlab="RT (min)", ylab="Intensity")
        ticint<-unlist(lapply(peaks(object), FUN=function(x){colSums(x)[2]}))
        ticrt<-header(object)$retentionTime/60
        points(cbind(ticrt,ticint), type="l", col="black")
        legend("topright", legend=c("IFScans","TIC","Highly Interesting Point"), lty=1, lwd=2, col=c("red","black","blue"), bty="n")
        if (length(clvect)>=1){
                arrows(as.numeric(round( header(object, scans=as.vector(clvect))$retentionTime/60,3)), y0=max(ticint)*.05, y1=0,col = "blue", lty = "solid", lwd = 2, length = 0.03, angle = 45, code = 1)
        }
        
}


if (length(good[which(ls_good2>=1)])>=1){
    cat(paste("Nan Mais Halo Quoi!,\n Good News, putative chlorinated compound found \n"))
    if (plotps==TRUE){
        return(mat)
    }else{
    #return(ls_good[which(ls_good2>=1)])
    #Now for RTs for all peaks in the whole an list()
    
    
        rt<-list()
        cl<-list()
        br<-list()
        for(i in 1:length(as.vector(good[which(ls_good2>=1)]))){
            
            rt[i] <-round( header(object, scans=as.vector(good[which(ls_good2>=1)])[i])$retentionTime/60,3)
            cl[i]<-!is.na(match(as.vector(good[which(ls_good2>=1)])[i], clvect))
            br[i]<-!is.na(match(as.vector(good[which(ls_good2>=1)])[i], brvect))
        }

        mat<-data.frame(cbind(as.numeric(rt),cl,br,lapply(ls_good[which(ls_good2>=1)], FUN=function(x)paste0(round(as.numeric(x),3), collapse="|"))))
        colnames(mat)<-c("RT","Cl OK","Br OK","MZs")
        mat<-mat[sort.list(unlist(mat[,"RT"]),decreasing = FALSE), ]
    
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
close(object)
}
