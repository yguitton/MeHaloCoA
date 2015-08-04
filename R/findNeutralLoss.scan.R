
#function modified from CAMERA package
#hits<-.findNeutralLoss.scan(file, mzdiff = 18.01, mzabs = 0.01, mzppm = 5, noise=500)

#function addapted from findNeutralLoss in CAMERA package
# @param file your complet path to your file (mzXML or CDF, mzML, mzData)
# @param mzdiff searched neutral loss in Da
# @param mzabs max difference allowed in Da
# @param mzppm max difference allowed in ppm
# @param noise level of noise above which an ion is concidered for calculations default 500 count
.findNeutralLoss.scan<-
function (file, mzdiff = NULL, mzabs = 00.5, mzppm = 10, noise=500, N=NULL, scanrange=NULL) 
{
plotSpec1<-function(object, ...) {

    sel <- profRange(object, ...)

   
    points <- cbind(profMz(object)[sel$massidx],
                    rowMeans(object@env$profile[sel$massidx,sel$scanidx,drop=FALSE]))
    

    invisible(points)
}
    #open file, need mzR
    object <- openMSfile(file)
    
    if (is.null(mzdiff)) {
        stop("Parameter mzdiff must be set\n")
    }
    if (!is.numeric(mzdiff) || any(mzdiff <= 0)) {
        stop("Parameter mzdiff must be a positive numeric value\n")
    }
    if (!is.numeric(mzabs) || mzabs < 0) {
        stop("Parameter mzabs must be a positive numeric value\n")
    }
    if (!is.numeric(mzppm) || mzppm < 0) {
        stop("Parameter mzppm must be a positive numeric value\n")
    }
    if (!is.numeric(noise) || noise < 0) {
        stop("Parameter noise must be a positive numeric value\n")
    }
       
    nlmin <- mzdiff - mzabs
    nlmax <- mzdiff + mzabs
    
    #reduction from scanrange value if needed
    if ( !is.null(scanrange)){
        mz<-mzR::peaks(object, scans=scanrange)
    }else
    {
        mz <- mzR::peaks(object, scans=1:length(mzR::peaks(object)))
    }
    
    #if N is not NULL
    if ( !is.null(N)){
        if (N%%2==0){
            N=N+1
            print(paste("the value of N was set to ",N))
        }
            lmz<-length(mz)
            start=1+N%/%2
            end=lmz-(1+N%/%2)
            
        if (!is.null(scanrange)){
            start1=min(scanrange)+N%/%2
            end1=max(scanrange)-(1+N%/%2)
            print( paste(start1,"-modified-",end1))
        }else
        {    
            print( paste(start,"-",end))
        }
        
        for(z in start:end){
            if (!is.null(scanrange)){
                z1<-min(scanrange)+z-1
                xr<-xcmsRaw(file,scanrange=c(z1-N%/%2,z1+N%/%2))
            }else
            {
                xr<-xcmsRaw(file,scanrange=c(z-N%/%2,z+N%/%2))
            }
            profStep(xr)<-mzabs
            mztemp<-plotSpec1(xr)
            mz[[z]]<-mztemp[which(mztemp[,2]!=0),]
        }
    
    }#end if N
    
    #print(mz)
    
    hits <- lapply(mz, FUN=function(j) {
                
                j=j[which(j[,2]>noise),1]
            
                nl <- as.matrix(dist(j, method = "manhattan"))
                
                hit <- FALSE
                if(length(j)>0){
                    for (x in 1:length(j)) {
                        mzadd <- j[x] * mzppm * 10^-6
                        y=1
                            if (length(which(nl[x,] > (nlmin[y] - mzadd) & nl[x,] < (nlmax[y] + mzadd)))>0) {
                                    # print(length(which(nl[x,]  > (nlmin[y] - mzadd) & nl[x,]  < (nlmax[y] + mzadd))))
                                    # print(paste(j[x]," and ", j[which(nl[x,] > (nlmin[y] - mzadd) & nl[x,] < (nlmax[y] + mzadd))], j[which(nl[x,] > (nlmin[y] - mzadd) & nl[x,] < (nlmax[y] + mzadd))]-j[x]))
                                    hit <- TRUE
                                    break
                              } #end if
                        
                    }#end for
                }#end if
                hit
        }#end function
        )#end lapply
    close(object)
    if (!is.null(N)){
    #in order to keep track of the modified mz list
        hit<-list(hits=hits,mz=mz)
        return(hit)
    }
    else{
        return(hits)
    }
}

