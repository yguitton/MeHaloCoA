# MeHaloCoA Tcl/Tk GUI for halogeneted compound search in MS data.
# Part of the code is based on the ade4TkGUI package by Jean Thioulouse <jthioulouse@biomserv.univ-lyon1.fr>, Stephane
#       Dray <dray@biomserv.univ-lyon1.fr>
#CDF2RData function dialog frame
#with profmethodvar

dialog.mehalocoa.CDF2RData <-
function(method="")
{
 if(method==""){
    cat("Please choose a method")
 }
 
 if(method=="matchedFilter"){
      cat("working with matchedFilter")
#
# Main dialog window with title
#
    tt <- tktoplevel()
    tkwm.title(tt,"MeHaloCoA-matchedFilter")    
#
# Variables for text fields
#
    
    indirvar <- tclVar(as.character(system.file("extdata", package="MeHaloCoA"))) # 
    infilevar<-tclVar() # fichier selectionne un par un et pas en bloque
    outdirvar <- tclVar(as.character(getwd()))
    #rbValue <- tclVar()
#
# Variable for number fields
#
    fwhmvar<-tclVar(12)
    #sigmavar<-tclVar(as.numeric(round(as.numeric(tclvalue(fwhmvar))/2.3548,2)))
    maxvar <- tclVar(50)
    snthreshvar <- tclVar(3)
    stepvar<-tclVar(0.1)
    stepsvar<-tclVar(2)
    mzdiffvar<-tclVar(0.05)
    anvar<-tclVar("an")
    profmethodvar<-tclVar("binlinbase")
    perfwhmvar<-tclVar(1)
    scanrangevar<-tclVar("NULL")
    rtrangeminvar<-tclVar("")
    rtrangemaxvar<-tclVar("")
    mzrangeminvar<-tclVar("")
    mzrangemaxvar<-tclVar("")
#

#
# Title
#
    TFrame <- tkframe(tt, relief="groove")
    labh <- tklabel(TFrame, bitmap="questhead")
    tkgrid(tklabel(TFrame,text="CDF2RData-matchedFilter", font="Times 18", foreground="red"), labh)
    tkbind(labh, "<Button-1>", function() print(help("CDF2RData")))
    tkgrid(TFrame)
#
# In and Out directories (option = files paths)
#    
    IOFrame <- tkframe(tt, relief="groove", borderwidth=2)
    tkgrid(tklabel(IOFrame,text="- Input & Output  files -", foreground="blue"), columnspan=8)
    indir.entry <- tkentry(IOFrame, textvariable=indirvar, width=55)
    outdir.entry <- tkentry(IOFrame, textvariable=outdirvar, width=55)
    "delinsert"<-function(arg,title1){
        tkdelete(arg, 0, "end")
        tkinsert(arg, "end", tclvalue(tkchooseDirectory(initialdir=getwd(),title=title1)))
        }
    choosedir.but <- tkbutton(IOFrame, text="Browse",command=function()  delinsert(arg=indir.entry, title1="Your Data Directory"))
    choosedir.but2 <- tkbutton(IOFrame, text="Browse",command=function() delinsert(arg=outdir.entry, title1="Your Result Directory"))
    tkgrid(tklabel(IOFrame,text="Data path (indir) : "), indir.entry,choosedir.but, tklabel(IOFrame,text="  Path to your Data") , sticky="w")
    tkgrid(tklabel(IOFrame,text="Output path (outdir) : "), outdir.entry,choosedir.but2, tklabel(IOFrame,text="  Path for Results") ,sticky="w")
    tkgrid(IOFrame, sticky="we")


#settings list for peak peacking

    
    PARAMFrame <- tkframe(tt, relief="groove", borderwidth=2)
    tkgrid(tklabel(PARAMFrame,text="                                               - xcms matchedfilter Parameters -", foreground="blue"), columnspan=8)
    
    fwhm.entry <- tkentry(PARAMFrame, textvariable=fwhmvar, width=6, state="normal")
    #sigma.entry <- tkentry(PARAMFrame, textvariable=sigmavar, width=6, state="normal")
    max.entry <- tkentry(PARAMFrame, textvariable=maxvar, width=6, state="normal")
    snthresh.entry <- tkentry(PARAMFrame, textvariable=snthreshvar, width=6, state="normal")
    step.entry <- tkentry(PARAMFrame, textvariable=stepvar, width=6, state="normal")
    steps.entry <- tkentry(PARAMFrame, textvariable=stepsvar, width=6, state="normal")
    mzdiff.entry <- tkentry(PARAMFrame, textvariable=mzdiffvar, width=6, state="normal")
    profmethod.entry <- tkentry(PARAMFrame, textvariable=profmethodvar, width=10, state="normal")
    perfwhm.entry <- tkentry(PARAMFrame, textvariable=perfwhmvar, width=6, state="normal")
    scanrange.entry <- tkentry(PARAMFrame, textvariable=scanrangevar, width=6, state="normal")
    mzrangemin.entry <- tkentry(PARAMFrame, textvariable=mzrangeminvar, width=6, state="normal")
    mzrangemax.entry <- tkentry(PARAMFrame, textvariable=mzrangemaxvar, width=6, state="normal")
    rtrangemin.entry <- tkentry(PARAMFrame, textvariable=rtrangeminvar, width=6, state="normal")
    rtrangemax.entry <- tkentry(PARAMFrame, textvariable=rtrangemaxvar, width=6, state="normal")
    
    
    tkgrid(tklabel(PARAMFrame,text="Peak fwhm (fwhm) : "), fwhm.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="S/N ratio (snthresh) : "), snthresh.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="mzdiff: "),mzdiff.entry, sticky="w")

      
    tkgrid(tklabel(PARAMFrame,text="                                              - OPTIONNAL Advanced xcms matchedfilter Parameters -", foreground="blue"), columnspan=8)
    #tkgrid(tklabel(PARAMFrame,text="sigma : "), sigma.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="max number of EIC : "),max.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="Step: "),step.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="Steps: "), steps.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="profmethod : "), profmethod.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="perfwhm: "),perfwhm.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="scanrange: "),scanrange.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="RT range(minute) RT MIN: "), rtrangemin.entry,tklabel(PARAMFrame,text="RT MAX : "),rtrangemax.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="MZ range MZ MIN: "), mzrangemin.entry,tklabel(PARAMFrame,text="MZ MAX : "),mzrangemax.entry, sticky="w")
    tkgrid(PARAMFrame, sticky="we")
    
    
    ####buttons
    #
# Local variables
#

    done <- tclVar(0)    # To terminate the dialog
    

        
################################
# Function to reset all dialog elements to default values
################################
    "reset" <- function()
    {
        tclvalue(indirvar)<-tclVar(as.character(system.file("extdata", package="MeHaloCoA")))
        tclvalue(anvar)<-"an"
        tclvalue(outdirvar)<-getwd()
        tclvalue(snthreshvar)<-3
        tclvalue(maxvar) <- 50
        tclvalue(fwhmvar)<-12
        #tclvalue(sigmavar)<-5.09
        tclvalue(stepvar)<-0.01
        tclvalue(stepsvar)<-2
        tclvalue(mzdiffvar)<-0.05
        tclvalue(profmethodvar)<-"binlinbase"
        tclvalue(perfwhmvar)<-1
        tclvalue(rtrangeminvar)<-""
        tclvalue(rtrangemaxvar)<-""
        tclvalue(mzrangeminvar)<-""
        tclvalue(mzrangemaxvar)<-""

    }
        "build" <- function()
    {
                indir<-as.character(tclvalue(indirvar))
                if (indir=="" ||is.null(indir)==TRUE){
                            indir<-tclvalue(tkchooseDirectory(title="Choose a folder with converted files"))
                }
        if(tclvalue(scanrangevar)=="NULL"){
            scanrange<-NULL
        }else
        {
            print("scanrange option used")
            scanrange<-tclvalue(scanrangevar)
        }
        if(tclvalue(mzrangeminvar)==""){
            mzrange<-NULL
        }else
        {
            print("mzrange option used")
            mzrange<-c(as.numeric(tclvalue(mzrangeminvar)),as.numeric(tclvalue(mzrangemaxvar)))
        }
        if(tclvalue(rtrangeminvar)==""){
            rtrange<-NULL
        }else
        {
            print("rtrange option used")
            rtrange<-c(as.numeric(tclvalue(rtrangeminvar)),as.numeric(tclvalue(rtrangemaxvar)))
        }
        if(is.null(rtrange) & is.null(mzrange)){
            settingslist=list(fwhm = as.numeric(tclvalue(fwhmvar)), 
                max = as.numeric(tclvalue(maxvar)), snthresh = as.numeric(tclvalue(snthreshvar)), step = as.numeric(tclvalue(stepvar)), steps = as.numeric(tclvalue(stepsvar)),
                mzdiff = as.numeric(tclvalue(mzdiffvar)), profmethod=tclvalue(profmethodvar),perfwhm=as.numeric(tclvalue(perfwhmvar)), scanrange=scanrange)
        }
        if(is.null(rtrange) & !is.null(mzrange)){
             settingslist=list(fwhm = as.numeric(tclvalue(fwhmvar)), 
                max = as.numeric(tclvalue(maxvar)), snthresh = as.numeric(tclvalue(snthreshvar)), step = as.numeric(tclvalue(stepvar)), steps = as.numeric(tclvalue(stepsvar)),
                mzdiff = as.numeric(tclvalue(mzdiffvar)), profmethod=tclvalue(profmethodvar),perfwhm=as.numeric(tclvalue(perfwhmvar)), scanrange=scanrange, mzrange=mzrange)
        }
        if(!is.null(rtrange) & !is.null(mzrange)){
            settingslist=list(fwhm = as.numeric(tclvalue(fwhmvar)), 
                max = as.numeric(tclvalue(maxvar)), snthresh = as.numeric(tclvalue(snthreshvar)), step = as.numeric(tclvalue(stepvar)), steps = as.numeric(tclvalue(stepsvar)),
                mzdiff = as.numeric(tclvalue(mzdiffvar)), profmethod=tclvalue(profmethodvar),perfwhm=as.numeric(tclvalue(perfwhmvar)), scanrange=scanrange,rtrange=rtrange, mzrange=mzrange)
        }
        if(!is.null(rtrange) & is.null(mzrange)){
         settingslist=list(fwhm = as.numeric(tclvalue(fwhmvar)), 
                max = as.numeric(tclvalue(maxvar)), snthresh = as.numeric(tclvalue(snthreshvar)), step = as.numeric(tclvalue(stepvar)), steps = as.numeric(tclvalue(stepsvar)),
                mzdiff = as.numeric(tclvalue(mzdiffvar)), profmethod=tclvalue(profmethodvar),perfwhm=as.numeric(tclvalue(perfwhmvar)), scanrange=scanrange, rtrange=rtrange)
        }
        substitute(do.call(CDF2RData,list(indir=indir,outdir=tclvalue(outdirvar), settingslist=settingslist)))
    
    }
################################
# Function to launch computations
################################
    "execcomp" <- function()
    {
    
    # Create an object
    #
        
        dudiname <- parse(text=paste("\"",tclvalue(anvar)[[1]],"\"",sep=""))
    #
    # Build and display the command line so that the user can check it
    #
        cmd <- build()
        if (cmd == 0) return(0)
            pr1 <- substr(options("prompt")$prompt, 1,2)
            #cat(eval(dudiname), " <- ", deparse(cmd, width = 256), cat("\n"), pr1, sep="")
    #
    # Execute the command
    #
        
        mydudi <- eval.parent(cmd)
        #bringToTop(which=-1)
        assign(eval(dudiname), mydudi, envir=as.environment(MeHaloCoAenv))
    
    
        
    print("Processing...")
    Sys.sleep(0.5)
    tkdestroy(tt)
    
            
            if (exists(tclvalue(anvar), envir=as.environment(MeHaloCoAenv))){
                res<-tclvalue(tkmessageBox(title="Done",message= "Peakpicking Step is Done",icon="info",type="okcancel"))
                Sys.sleep(0.5)
                if(res=="ok"){
                    MeHaloCoA::dialog.mehalocoa.cldetect()
                }
                #print(get("an", envir=as.environment(MeHaloCoAenv)))
            }
    
        #print(get(tclvalue(anvar), envir=MeHaloCoAenv))
        #return(get(tclvalue(anvar), envir=MeHaloCoAenv))
    }
#
# Reset Cancel and Submit buttons
#
    RCSFrame <- tkframe(tt, relief="groove")
    reset.but <- tkbutton(RCSFrame, text="Reset", command=reset)
    cancel.but <- tkbutton(RCSFrame, text="Cancel", command=function() tkdestroy(tt))
    submit.but <- tkbutton(RCSFrame, text="Submit", default="active", command=function() execcomp())
    tkgrid(cancel.but, submit.but, reset.but, ipadx=20)    
    tkgrid(RCSFrame)
#
# If window is closed by user, terminate the dialog
#
    tkbind(tt, "<Destroy>", function() tclvalue(done)<-2)
    tkbind(tt, "<KeyPress-Return>", function() execcomp())
    tkbind(tt, "<KeyPress-Escape>", function() tkdestroy(tt))
    ###end buttons
    
    
    HELPFrame <- tkframe(tt, relief="groove", borderwidth=2)
    tkgrid(tklabel(HELPFrame,text="- xcms matchedfilter help -" , foreground="blue"),columnspan=2,  sticky="we")
    tkgrid(tklabel(HELPFrame,text="fwhm:     full width at half maximum of matched filtration gaussian model peak. Only used to calculate the actual sigma, see below." ),columnspan=2,  sticky="w") 
    tkgrid(tklabel(HELPFrame,text="snthresh: signal to noise ratio cutoff" ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="step:     step size to use for profile generation" ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="mzdiff:     minimum difference in m/z for peaks with overlapping retention times" ),columnspan=2,  sticky="w")
    
#    tkgrid(tklabel(HELPFrame,text="sigma:     standard deviation (width) of matched filtration model peak Default=fwhm/2.3548" ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="max:     maximum number of peaks per extracted ion chromatogram" ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="steps:     number of steps to merge prior to filtration" ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="profmethod: can be bin, binlin, binlinbase, intlin    " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="perfwhm:     from CAMERA package Default=0.6, percentage of the width of the FWHM" ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="scanrange: scan range to process (c(100,15000)" ),columnspan=2,  sticky="w")
    
    

    tkgrid(HELPFrame, sticky="w")

#
# User closed the window
#
    if(tclvalue(done)=="2") return()
 }
 
 if(method=="centWave"){
cat("working with centWave")  
#
# Main dialog window with title
#
    tt <- tktoplevel()
    tkwm.title(tt,"MeHaloCoA- centWave")    
#
# Variables for text fields
#
    
    indirvar <- tclVar(as.character(system.file("extdata", package="MeHaloCoA"))) # 
    infilevar<-tclVar() # fichier selectionne un par un et pas en bloque
    outdirvar <- tclVar(as.character(getwd()))
    #rbValue <- tclVar()
#
# Variable for number fields
#
    ppmvar<-tclVar(15)
    peakwidthminvar<-tclVar(8)
    peakwidthmaxvar<-tclVar(25)
    snthreshvar <- tclVar(3)
    prefilterstepvar<-tclVar(3)
    prefilternoisevar<-tclVar(0)
    centerfunvar<-tclVar("wMean")
    integratevar<-tclVar(1)
    mzdiffvar<-tclVar(0.05)
    fitgaussvar<-tclVar("TRUE")
    scanrangevar<-tclVar("NULL")
    noisevar<-tclVar(10000)
    anvar<-tclVar("an")
    profmethodvar<-tclVar("binlinbase")
    rtrangeminvar<-tclVar("")
    rtrangemaxvar<-tclVar("")
    mzrangeminvar<-tclVar("")
    mzrangemaxvar<-tclVar("")
    
# Checkboxes
#
#    mznfvar <- tclVar(0)
#    quantnfvar <- tclVar(1)  #for quant=TRUE/FALSE option in MS.Clust with Agilent files


#
# Title
#
    TFrame <- tkframe(tt, relief="groove")
    labh <- tklabel(TFrame, bitmap="questhead")
    tkgrid(tklabel(TFrame,text="CDF2RData -centWave", font="Times 18", foreground="red"), labh)
    tkbind(labh, "<Button-1>", function() print(help("CDF2RData")))
    tkgrid(TFrame)
#
# In and Out directories (option = files paths)
#    
    IOFrame <- tkframe(tt, relief="groove", borderwidth=2)
    tkgrid(tklabel(IOFrame,text="- Input & Output  files -", foreground="blue"), columnspan=8)
    indir.entry <- tkentry(IOFrame, textvariable=indirvar, width=55)
    outdir.entry <- tkentry(IOFrame, textvariable=outdirvar, width=55)
    "delinsert"<-function(arg,title1){
        tkdelete(arg, 0, "end")
        tkinsert(arg, "end", tclvalue(tkchooseDirectory(initialdir=getwd(),title=title1)))
        }
    choosedir.but <- tkbutton(IOFrame, text="Browse",command=function()  delinsert(arg=indir.entry, title1="Your Data Directory"))
    choosedir.but2 <- tkbutton(IOFrame, text="Browse",command=function() delinsert(arg=outdir.entry, title1="Your Result Directory"))
    tkgrid(tklabel(IOFrame,text="Data path (indir) : "), indir.entry,choosedir.but, tklabel(IOFrame,text="  Path to your Data") , sticky="w")
    tkgrid(tklabel(IOFrame,text="Output path (outdir) : "), outdir.entry,choosedir.but2, tklabel(IOFrame,text="  Path for Results") ,sticky="w")
    tkgrid(IOFrame, sticky="we")


#settings list for peak peacking

    
    PARAMFrame <- tkframe(tt, relief="groove", borderwidth=2)
    tkgrid(tklabel(PARAMFrame,text="                                               - xcms centWave Parameters -", foreground="blue"), columnspan=8)
    
    ppm.entry <- tkentry(PARAMFrame, textvariable=ppmvar, width=6, state="normal")
    peakwidthmin.entry <- tkentry(PARAMFrame, textvariable=peakwidthminvar, width=6, state="normal")
    peakwidthmax.entry <- tkentry(PARAMFrame, textvariable=peakwidthmaxvar, width=6, state="normal")
    snthresh.entry <- tkentry(PARAMFrame, textvariable=snthreshvar, width=6, state="normal")
    prefilterstep.entry<- tkentry(PARAMFrame, textvariable=prefilterstepvar, width=6, state="normal")
    prefilternoise.entry<- tkentry(PARAMFrame, textvariable=prefilternoisevar, width=6, state="normal")
    centerfun.entry <- tkentry(PARAMFrame, textvariable=centerfunvar, width=10, state="normal")
    integrate.entry <- tkentry(PARAMFrame, textvariable=integratevar, width=6, state="normal")
    mzdiff.entry <- tkentry(PARAMFrame, textvariable=mzdiffvar, width=6, state="normal")
    fitgauss.entry <- tkentry(PARAMFrame, textvariable=fitgaussvar, width=6, state="normal")
    scanrange.entry <- tkentry(PARAMFrame, textvariable=scanrangevar, width=6, state="normal")
    noise.entry <- tkentry(PARAMFrame, textvariable=noisevar, width=6, state="normal")
    profmethod.entry <- tkentry(PARAMFrame, textvariable=profmethodvar, width=10, state="normal")
    mzrangemin.entry <- tkentry(PARAMFrame, textvariable=mzrangeminvar, width=6, state="normal")
    mzrangemax.entry <- tkentry(PARAMFrame, textvariable=mzrangemaxvar, width=6, state="normal")
    rtrangemin.entry <- tkentry(PARAMFrame, textvariable=rtrangeminvar, width=6, state="normal")
    rtrangemax.entry <- tkentry(PARAMFrame, textvariable=rtrangemaxvar, width=6, state="normal")
    
    tkgrid(tklabel(PARAMFrame,text="Peakwidth (min) : "), peakwidthmin.entry,tklabel(PARAMFrame,text="(max) : "),peakwidthmax.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="S/N ratio (snthresh) : "), snthresh.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="Noise: "),noise.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="ppm : "), ppm.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="mzdiff: "),mzdiff.entry, sticky="w")
    
      
    tkgrid(tklabel(PARAMFrame,text="                                              - OPTIONNAL Advanced xcms centWave Parameters -", foreground="blue"), columnspan=8)
    tkgrid(tklabel(PARAMFrame,text="Prefilter (nb scan) : "), prefilterstep.entry,tklabel(PARAMFrame,text="prefilter noise : "),prefilternoise.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="integrate: "),integrate.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="mzCenterfun: "),centerfun.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="gaussian: "),fitgauss.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="scanrange: "),scanrange.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="profmethod : "), profmethod.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="RT range(minute) RT MIN: "), rtrangemin.entry,tklabel(PARAMFrame,text="RT MAX : "),rtrangemax.entry, sticky="w")
    tkgrid(tklabel(PARAMFrame,text="MZ range MZ MIN: "), mzrangemin.entry,tklabel(PARAMFrame,text="MZ MAX : "),mzrangemax.entry, sticky="w")
    tkgrid(PARAMFrame, sticky="we")
    
    
    ####buttons
    #
# Local variables
#

    done <- tclVar(0)    # To terminate the dialog
    

        
################################
# Function to reset all dialog elements to default values
################################
    "reset" <- function()
    {
        tclvalue(indirvar)<-tclVar(as.character(system.file("extdata", package="MeHaloCoA")))
        tclvalue(anvar)<-"an"
        tclvalue(outdirvar)<-getwd()
        tclvalue(snthreshvar)<-3
        tclvalue(scanrangevar)<-"NULL"
        tclvalue(mzdiffvar)<-0.05
        tclvalue(profmethodvar)<-"bin"
        tclvalue(rtrangeminvar)<-""
        tclvalue(rtrangemaxvar)<-""
        tclvalue(mzrangeminvar)<-""
        tclvalue(mzrangemaxvar)<-""
    

    }
    # ppmvar<-tclVar(15)
    # peakwidthminvar<-tclVar(8)
    # peakwidthmaxvar<-tclVar(8)
    # snthreshvar <- tclVar(3)
    # prefilterstepvar<-tclVar(3)
    # prefilternoisevar<-tclVar(5000)
    # centerfunvar<-tclVar("wMean")
    # integratevar<-tclVar(1)
    # mzdiffvar<-tclVar(0.05)
    # fitgaussvar<-tclVar(TRUE)
    # scanrangevar<-NULL
    # noisevar<-tclVar(10000)
    # anvar<-tclVar("an")
    # profmethodvar<-tclVar("bin")
        "build" <- function()
    {
                indir<-as.character(tclvalue(indirvar))
                if (indir=="" ||is.null(indir)==TRUE){
                            indir<-tclvalue(tkchooseDirectory(title="Choose a folder with converted files"))
                }
                
        peakwidth<-c(as.numeric(tclvalue(peakwidthminvar)),as.numeric(tclvalue(peakwidthmaxvar)))
        prefilter<-c(as.numeric(tclvalue(prefilterstepvar)),as.numeric(tclvalue(prefilternoisevar)))
        
        if(tclvalue(scanrangevar)=="NULL"){
            scanrange<-NULL
        }else
        {
            print("scanrange option used")
            scanrange<-tclvalue(scanrangevar)
        }
            substitute(do.call(CDF2RData,list(indir=indir,outdir=tclvalue(outdirvar), 
                settingslist=list(method="centWave", 
                                    ppm=as.numeric(tclvalue(ppmvar)),
                                    peakwidth=peakwidth,
                                    snthresh = as.numeric(tclvalue(snthreshvar)),
                                    prefilter=prefilter,
                                    mzCenterFun=tclvalue(centerfunvar),
                                    integrate=as.numeric(tclvalue(integratevar)),
                                    mzdiff = as.numeric(tclvalue(mzdiffvar)),
                                    fitgauss=tclvalue(fitgaussvar),
                                    scanrange=scanrange,
                                    noise=as.numeric(tclvalue(noisevar)),
                                    profmethod=tclvalue(profmethodvar)
                                    
                                )
                )))
            
    
    }
################################
# Function to launch computations
################################
    "execcomp" <- function()
    {
    
    # Create an object
    #
        
        dudiname <- parse(text=paste("\"",tclvalue(anvar)[[1]],"\"",sep=""))
    #
    # Build and display the command line so that the user can check it
    #
        cmd <- build()
        if (cmd == 0) return(0)
            pr1 <- substr(options("prompt")$prompt, 1,2)
            #cat(eval(dudiname), " <- ", deparse(cmd, width = 256), cat("\n"), pr1, sep="")
    #
    # Execute the command
    #
        
        mydudi <- eval.parent(cmd)
        #bringToTop(which=-1)
        assign(eval(dudiname), mydudi, envir=as.environment(MeHaloCoAenv))
    
    
        
    print("Processing...")
    Sys.sleep(0.5)
    tkdestroy(tt)
    
            
            if (exists(tclvalue(anvar), envir=as.environment(MeHaloCoAenv))){
                res<-tclvalue(tkmessageBox(title="Done",message= "Peakpicking Step is Done",icon="info",type="okcancel"))
                Sys.sleep(0.5)
                if(res=="ok"){
                    MeHaloCoA::dialog.mehalocoa.cldetect()
                }
                #print(get("an", envir=as.environment(MeHaloCoAenv)))
            }
    
        #print(get(tclvalue(anvar), envir=MeHaloCoAenv))
        #return(get(tclvalue(anvar), envir=MeHaloCoAenv))
    }
#
# Reset Cancel and Submit buttons
#
    RCSFrame <- tkframe(tt, relief="groove")
    reset.but <- tkbutton(RCSFrame, text="Reset", command=reset)
    cancel.but <- tkbutton(RCSFrame, text="Cancel", command=function() tkdestroy(tt))
    submit.but <- tkbutton(RCSFrame, text="Submit", default="active", command=function() execcomp())
    tkgrid(cancel.but, submit.but, reset.but, ipadx=20)    
    tkgrid(RCSFrame)
#
# If window is closed by user, terminate the dialog
#
    tkbind(tt, "<Destroy>", function() tclvalue(done)<-2)
    tkbind(tt, "<KeyPress-Return>", function() execcomp())
    tkbind(tt, "<KeyPress-Escape>", function() tkdestroy(tt))
    ###end buttons
    
    
    HELPFrame <- tkframe(tt, relief="groove", borderwidth=2)
    tkgrid(tklabel(HELPFrame,text="- xcms centWave help -" , foreground="blue"),columnspan=2,  sticky="we")
    tkgrid(tklabel(HELPFrame,text="peakwidth: Chromatographic peak width, given as range (min,max) in seconds" ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="snthresh: signal to noise ratio cutoff" ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="ppm: maxmial tolerated m/z deviation in consecutive scans, in ppm (parts per million)" ),columnspan=2,  sticky="w") 
    tkgrid(tklabel(HELPFrame,text="mzdiff:     minimum difference in m/z for peaks with overlapping retention times" ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="prefilter: prefilter=c(k,I). Prefilter step for the first phase. Mass traces are only retained if they contain at least k peaks with intensity >= I." ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="mzCenterFun: Function to calculate the m/z center of the feature (wMean,wMeanApex3 or meanApex3)" ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="integrate: Integration method. If =1 peak limits are found through descent on the mexican hat filtered data, if =2 the descent is done on the real data. Method 2 is very accurate but prone to noise" ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="profmethod: can be bin, binlin, binlinbase, intlin    " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="fitgauss: logical, if TRUE a Gaussian is fitted to each peak " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="scanrange: scan range to process (c(100,15000)" ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="noise: centroids with intensity < noise are omitted from ROI detection" ),columnspan=2,  sticky="w")
    tkgrid(HELPFrame, sticky="w")

#
# User closed the window
#
    if(tclvalue(done)=="2") return()
 }
 
}

