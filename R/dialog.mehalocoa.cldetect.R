# MeHaloCoA Tcl/Tk GUI for halogeneted compound search in MS data.
# Part of the code is based on the ade4TkGUI package by Jean Thioulouse <jthioulouse@biomserv.univ-lyon1.fr>, Stephane
#       Dray <dray@biomserv.univ-lyon1.fr>
#cldetect function dialog frame

dialog.mehalocoa.cldetect <-
function(method="")
{

if(method==""){
        #
    # Main dialog window with title
    #
        tt <- tktoplevel()
        tkwm.title(tt,"MeHaloCoA")    
    #
    # Variables for text fields
    #
        
        
        outdirvar <- tclVar(as.character(getwd()))
        matclvar<-tclVar("matcl")
    #
    # Variable for number fields
    #
        xsAnnotatevar<-tclVar("an")
        plotpsvar<-tclVar("TRUE")
        deprofvar<-tclVar("FALSE")
        #clppmerrvar<-tclVar(50)
        Threshvar<-tclVar(20)
        m1var<-tclVar(1.003)
        m2var<-tclVar(1.997)
        mdiffvar<-tclVar(0.05)
        ppmerrvar<-tclVar(50)
        valvar<-tclVar("maxo")
        mdiff2var<-tclVar(0.001)
        myfilevar<-tclVar("halo_list.csv")
        myfile2var<-tclVar("halo_list_short.csv")
        m1needvar<-tclVar("FALSE")

    #
    # Title
    #
        TFrame <- tkframe(tt, relief="groove")
        labh <- tklabel(TFrame, bitmap="questhead")
        tkgrid(tklabel(TFrame,text="MeHaloCoA::cldetect", font="Times 18", foreground="red"), labh)
        tkbind(labh, "<Button-1>", function() print(help("cldetect")))
        tkgrid(TFrame)
    #
    # In and Out directories (option = files paths)
    #    
        IOFrame <- tkframe(tt, relief="groove", borderwidth=2)
        tkgrid(tklabel(IOFrame,text=" - Input & Output  files -", foreground="blue"), columnspan=8)
        outdir.entry <- tkentry(IOFrame, textvariable=outdirvar, width=55)
        "delinsert"<-function(arg,title1){
            tkdelete(arg, 0, "end")
            tkinsert(arg, "end", tclvalue(tkchooseDirectory(initialdir=getwd(),title=title1)))
        }
        choosedir.but2 <- tkbutton(IOFrame, text="Browse",command=function() delinsert(arg=outdir.entry, title1="Your Result Directory"))
        tkgrid(tklabel(IOFrame,text="Output path (outdir) : "), outdir.entry,choosedir.but2,tklabel(IOFrame,text="Path for Results "), sticky="w")
        tkgrid(IOFrame, sticky="we")


    #settings list for halogen detection algorithm
    # "xsAnnotate" "plotps"     "deprof"     "clppmerr"   "Thresh"      "m1"         "m2"         "mdiff"      "ppmerr"    
    # "val"        "mdiff2"     "myfile"     "myfile2" 
        
        PARAMFrame <- tkframe(tt, relief="groove", borderwidth=2)
        tkgrid(tklabel(PARAMFrame,text="                                                - cldetect Parameters -", foreground="blue"), columnspan=8)
        
        xsAnnotate.entry <- tkentry(PARAMFrame, textvariable=xsAnnotatevar, width=6, state="normal")
        plotps.entry <- tkentry(PARAMFrame, textvariable=plotpsvar, width=6, state="normal")
        deprof.entry <- tkentry(PARAMFrame, textvariable=deprofvar, width=6, state="normal")
        #clppmerr.entry <- tkentry(PARAMFrame, textvariable=clppmerrvar, width=6, state="normal")
        Thresh.entry <- tkentry(PARAMFrame, textvariable=Threshvar, width=6, state="normal")
        m1.entry <- tkentry(PARAMFrame, textvariable=m1var, width=6, state="normal")
        m2.entry <- tkentry(PARAMFrame, textvariable=m2var, width=6, state="normal")
        mdiff.entry <- tkentry(PARAMFrame, textvariable=mdiffvar, width=6, state="normal")
        ppmerr.entry <- tkentry(PARAMFrame, textvariable=ppmerrvar, width=6, state="normal")
        val.entry <- tkentry(PARAMFrame, textvariable=valvar, width=10, state="normal")
        mdiff2.entry <- tkentry(PARAMFrame, textvariable=mdiff2var, width=6, state="normal")
        myfile.entry <- tkentry(PARAMFrame, textvariable=myfilevar, width=25, state="normal")
        myfile2.entry <- tkentry(PARAMFrame, textvariable=myfile2var, width=25, state="normal")
        m1need.entry <- tkentry(PARAMFrame, textvariable=m1needvar, width=6, state="normal")
        
        
        tkgrid(tklabel(PARAMFrame,text="xsAnnotate object : "), xsAnnotate.entry, sticky="w")
        
        tkgrid(tklabel(PARAMFrame,text="m1 : "),m1.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="m2 : "),m2.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="Thresh : "), Thresh.entry, sticky="w")
        #tkgrid(tklabel(PARAMFrame,text="clppmerr: "),clppmerr.entry, sticky="w")
        
        tkgrid(tklabel(PARAMFrame,text="                                                - OPTIONNAL Advanced cldetect Parameters -", foreground="blue"), columnspan=8)
        tkgrid(tklabel(PARAMFrame,text="plotps : "), plotps.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="mdiff: "), mdiff.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="ppmerr: "),ppmerr.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="m1need: "),m1need.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="val : "), val.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="mdiff2: "), mdiff2.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="myfile: "),myfile.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="myfile2: "),myfile2.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="deprof: "),deprof.entry, sticky="w")
        tkgrid(PARAMFrame, sticky="we")
        

    #
    # Local variables
    #

        done <- tclVar(0)    # To terminate the dialog
        

            
    ################################
    # Function to reset all dialog elements to default values
    ################################
        
        "reset" <- function()
        {
            tclvalue(outdirvar)<-tclVar(as.character(getwd()))
            tclvalue(matclvar)<-"matcl"
            tclvalue(xsAnnotatevar)<-"an"
            tclvalue(plotpsvar)<-"TRUE"
            tclvalue(deprofvar)<-"FALSE"
        #    tclvalue(clppmerrvar)<-50
            tclvalue(Threshvar)<-20
            tclvalue(m1var)<-1.003
            tclvalue(m2var)<-1.997
            tclvalue(mdiffvar)<-0.05
            tclvalue(ppmerrvar)<-50
            tclvalue(valvar)<-"maxo"
            tclvalue(mdiff2var)<-0.001
            tclvalue(myfilevar)<-"halo_list.csv"
            tclvalue(myfile2var)<-"halo_list_short.csv"
            tclvalue(m1needvar)<-"FALSE"
        }
        
    ################################
    # Function to launch computations
    ################################
        "execcomp" <- function()
        {
        outdir<-as.character(tclvalue(outdirvar))
        if (outdir=="" ||is.null(outdir)==TRUE){
                    outdir<-tclvalue(tkchooseDirectory(title="Choose your Results folder"))
        }
        
            
        print("Processing...")
        Sys.sleep(0.5)
        tkdestroy(tt)
           
        #test if xsAnnotate object exists in MeHaloCoAenv or .GlobalEnv
        if(length(which(ls("MeHaloCoAenv")==tclvalue(xsAnnotatevar)))==0){
            an<-get(ls(.GlobalEnv)[which(ls(.GlobalEnv)==tclvalue(xsAnnotatevar))], envir=as.environment(.GlobalEnv))
            print("Note: Use of an xsAnnotate object from GlobalEnv as entry")
        }else
        {
            an<-get(ls("MeHaloCoAenv")[which(ls("MeHaloCoAenv")==tclvalue(xsAnnotatevar))], envir=as.environment("MeHaloCoAenv"))
        }

        if((class(an)=="xsAnnotate")==FALSE){
            xsAnnotate<-an[as.vector(which(lapply(an,function(x)class(x))=="xsAnnotate"))]
        }else
        {
            xsAnnotate<-an
        }
        print(xsAnnotate)

    if (length(xsAnnotate)>1){  
      print("several files")
       assign("matcl",vector(mode="list", length=length(xsAnnotate)), envir=as.environment(MeHaloCoAenv))
       
       #projectpdf is a function that avoid overwriting IFC.pdf
      projectpdf(dir=outdir,projectName="IFC", ext="pdf")
     # pdf(file=file.path(outdir,"IFC%03d.pdf"))
          mat<-list()
          for (i in 1:length(xsAnnotate)){
            MeHaloCoAenv$matcl[[i]]<-do.call(cldetect,c(outdir=outdir,xsAnnotate=xsAnnotate[[i]],list( 
            plotps=tclvalue(plotpsvar),
            deprof=tclvalue(deprofvar),
         #   clppmerr=as.numeric(tclvalue(clppmerrvar)),
            Thresh=as.numeric(tclvalue(Threshvar)),
            m1=as.numeric(tclvalue(m1var)),
            m2=as.numeric(tclvalue(m2var)),
            mdiff=as.numeric(tclvalue(mdiffvar)),
            ppmerr=as.numeric(tclvalue(ppmerrvar)),
            val=tclvalue(valvar),
            mdiff2=as.numeric(tclvalue(mdiff2var)),
            myfile=paste("file_",i,"_",tclvalue(myfilevar)),
            myfile2=tclvalue(myfile2var),
            m1need=tclvalue(m1needvar)
          )
          ))
          }
    dev.off()
            for (i in 1:length(MeHaloCoAenv$matcl)){
                MeHaloCoAenv$matcl[[i]]<-cbind(basename(xsAnnotate[[i]]@xcmsSet@filepaths),MeHaloCoAenv$matcl[[i]])
                MeHaloCoAenv$matcl[[i]] <- data.frame(lapply(MeHaloCoAenv$matcl[[i]], as.character), stringsAsFactors=FALSE)
                colnames(MeHaloCoAenv$matcl[[i]])[1]<-"File"
            }
            mat<-do.call("rbind",MeHaloCoAenv$matcl)
            
            write.table(mat, file=file.path(outdir, tclvalue(myfile2var)), row.names=FALSE, sep="\t")
    }
    else{
      projectpdf(dir=outdir,projectName="IFC", ext="pdf")
      # pdf(file=file.path(outdir,"IFC%03d.pdf"))
        assign(tclvalue(matclvar), do.call(cldetect,c(outdir=outdir,xsAnnotate=xsAnnotate,list( 
          plotps=tclvalue(plotpsvar),
          deprof=tclvalue(deprofvar),
        #  clppmerr=as.numeric(tclvalue(clppmerrvar)),
          Thresh=as.numeric(tclvalue(Threshvar)),
          m1=as.numeric(tclvalue(m1var)),
          m2=as.numeric(tclvalue(m2var)),
          mdiff=as.numeric(tclvalue(mdiffvar)),
          ppmerr=as.numeric(tclvalue(ppmerrvar)),
          val=tclvalue(valvar),
          mdiff2=as.numeric(tclvalue(mdiff2var)),
          myfile=tclvalue(myfilevar),
          myfile2=tclvalue(myfile2var),
          m1need=tclvalue(m1needvar)
        )
        )), envir=as.environment("MeHaloCoAenv"))
      dev.off()
    }
                
            
                tkmessageBox(title="Done",message= "cldetect Step is Done",icon="info",type="ok")
                
            
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

    HELPFrame <- tkframe(tt, relief="groove", borderwidth=2)
    tkgrid(tklabel(HELPFrame,text="- cldetect help -" , foreground="blue"),columnspan=2,  sticky="we")
    tkgrid(tklabel(HELPFrame,text="outdir:     your results directory " ),columnspan=2,  sticky="w") 
    tkgrid(tklabel(HELPFrame,text="xsAnnotate: an R CAMERA object created by CDF2Data.R function  or xs<-xcmsSet() then an<-annotate(xs) " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="plotps:     Draw nice Isotopic Filtered Chromatogram (IFC)  " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="deprof: default=FALSE if TRUE deprofile.scan from RMassBank is applied on each pspectra" ),columnspan=2,  sticky="w")
    #tkgrid(tklabel(HELPFrame,text="clppmerr: error on M, M+1 or M+2 isotopes " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="Thresh: threshold of intensity for M+2 here 30 " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="m1: mass defect between M and M+1 isotope (here 1.0033) " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="m2: mass defect between M and M+2 isotope (here 1.997 for Cl) " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="mdiff: mdiff differences in Da applied to neutral loss search" ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="ppmerr: ppm margin tolerated by cldetect " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="m1need: Should M+1 isotope be present? if FALSE peaks with no M+1 are considered (interesting for low intensity compounds) " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="val: can be into or maxo, area  or intensity respectively  " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="mdiff2: mdiff2 differences in Da applied to IFC chromatogram drawing " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="myfile: name of the output file default halo_list.csv" ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="myfile2: name of the output file default halo_list_short.csv is a resume from all individual myfile\n in the case off several files have been processed " ),columnspan=2,  sticky="w")

    tkgrid(HELPFrame, sticky="we")
    #
    # User closed the window
    #
        if(tclvalue(done)=="2") return()
}
if(method=="scan"){
#
    # Main dialog window with title
    #
        tt <- tktoplevel()
        tkwm.title(tt,"MeHaloCoA")    
    #
    # Variables for text fields
    #
        
        
        outdirvar <- tclVar(as.character(getwd()))
        indirvar <- tclVar(as.character(system.file("doc/mzData", package="MeHaloCoA"))) # 
        matclvar<-tclVar("matcl")
    #
    # Variable for number fields
    #
        # xsAnnotatevar<-tclVar("an")
        plotpsvar<-tclVar("TRUE")
        deprofvar<-tclVar("FALSE")
        noisevar<-tclVar(500)
        Nvar<-tclVar(5)
        Threshvar<-tclVar(20)
        m1var<-tclVar(1.003)
        m2var<-tclVar(1.997)
        mdiffvar<-tclVar(0.05)
        ppmerrvar<-tclVar(50)
        # valvar<-tclVar("maxo")
        mdiff2var<-tclVar(0.001)
        myfilevar<-tclVar("halo_list.csv")
        myfile2var<-tclVar("halo_list_short.csv")
        m1needvar<-tclVar("FALSE")

    #
    # Title
    #
        TFrame <- tkframe(tt, relief="groove")
        labh <- tklabel(TFrame, bitmap="questhead")
        tkgrid(tklabel(TFrame,text="cldetect.scan", font="Times 18", foreground="red"), labh)
        tkbind(labh, "<Button-1>", function() print(help("cldetect.scan")))
        tkgrid(TFrame)
    #
    # In and Out directories (option = files paths)
    #    
        IOFrame <- tkframe(tt, relief="groove", borderwidth=2)
        tkgrid(tklabel(IOFrame,text=" - Input & Output  files -", foreground="blue"), columnspan=8)
        indir.entry <- tkentry(IOFrame, textvariable=indirvar, width=55)
        outdir.entry <- tkentry(IOFrame, textvariable=outdirvar, width=55)
        "delinsert"<-function(arg,title1){
            tkdelete(arg, 0, "end")
            tkinsert(arg, "end", tclvalue(tkchooseDirectory(initialdir=getwd(),title=title1)))
        }
        choosedir.but <- tkbutton(IOFrame, text="Browse",command=function()  delinsert(arg=indir.entry, title1="Your Data Directory"))
        choosedir.but2 <- tkbutton(IOFrame, text="Browse",command=function() delinsert(arg=outdir.entry, title1="Your Result Directory"))
        tkgrid(tklabel(IOFrame,text="Data path (indir) : "), indir.entry,choosedir.but, tklabel(IOFrame,text="  Path to your Data") , sticky="w")
        tkgrid(tklabel(IOFrame,text="Output path (outdir) : "), outdir.entry,choosedir.but2,tklabel(IOFrame,text="Path for Results "), sticky="w")
        tkgrid(IOFrame, sticky="we")


    #settings list for halogen detection algorithm
    # "xsAnnotate" "plotps"     "deprof"     "clppmerr"   "Thresh"      "m1"         "m2"         "mdiff"      "ppmerr"    
    # "val"        "mdiff2"     "myfile"     "myfile2" 
        
        PARAMFrame <- tkframe(tt, relief="groove", borderwidth=2)
        tkgrid(tklabel(PARAMFrame,text="                                                - cldetect.scan Parameters -", foreground="blue"), columnspan=8)
        
        # xsAnnotate.entry <- tkentry(PARAMFrame, textvariable=xsAnnotatevar, width=6, state="normal")
        # plotps.entry <- tkentry(PARAMFrame, textvariable=plotpsvar, width=6, state="normal")
        deprof.entry <- tkentry(PARAMFrame, textvariable=deprofvar, width=6, state="normal")
        noise.entry <- tkentry(PARAMFrame, textvariable=noisevar, width=6, state="normal")
        N.entry <- tkentry(PARAMFrame, textvariable=Nvar, width=6, state="normal")
        Thresh.entry <- tkentry(PARAMFrame, textvariable=Threshvar, width=6, state="normal")
        m1.entry <- tkentry(PARAMFrame, textvariable=m1var, width=6, state="normal")
        m2.entry <- tkentry(PARAMFrame, textvariable=m2var, width=6, state="normal")
        mdiff.entry <- tkentry(PARAMFrame, textvariable=mdiffvar, width=6, state="normal")
        ppmerr.entry <- tkentry(PARAMFrame, textvariable=ppmerrvar, width=6, state="normal")
        # val.entry <- tkentry(PARAMFrame, textvariable=valvar, width=10, state="normal")
        mdiff2.entry <- tkentry(PARAMFrame, textvariable=mdiff2var, width=6, state="normal")
        myfile.entry <- tkentry(PARAMFrame, textvariable=myfilevar, width=25, state="normal")
        myfile2.entry <- tkentry(PARAMFrame, textvariable=myfile2var, width=25, state="normal")
        m1need.entry <- tkentry(PARAMFrame, textvariable=m1needvar, width=6, state="normal")
        
        
        # tkgrid(tklabel(PARAMFrame,text="xsAnnotate object : "), xsAnnotate.entry, sticky="w")
        
        tkgrid(tklabel(PARAMFrame,text="m1 : "),m1.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="m2 : "),m2.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="Thresh : "), Thresh.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="noise: "),noise.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="N: "),N.entry, sticky="w")
        
        tkgrid(tklabel(PARAMFrame,text="                                                - OPTIONNAL Advanced cldetect.scan Parameters -", foreground="blue"), columnspan=8)
        # tkgrid(tklabel(PARAMFrame,text="plotps : "), plotps.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="mdiff: "), mdiff.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="ppmerr: "),ppmerr.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="m1need: "),m1need.entry, sticky="w")
        # tkgrid(tklabel(PARAMFrame,text="val : "), val.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="mdiff2: "), mdiff2.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="myfile: "),myfile.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="myfile2: "),myfile2.entry, sticky="w")
        tkgrid(tklabel(PARAMFrame,text="deprof: "),deprof.entry, sticky="w")
        tkgrid(PARAMFrame, sticky="we")
        

    #
    # Local variables
    #

        done <- tclVar(0)    # To terminate the dialog
        

            
    ################################
    # Function to reset all dialog elements to default values
    ################################
        
        "reset" <- function()
        {
            tclvalue(indirvar)<-tclVar(as.character(getwd()))
            tclvalue(outdirvar)<-tclVar(as.character(getwd()))
            tclvalue(matclvar)<-"matcl"
            # tclvalue(xsAnnotatevar)<-"an"
            # tclvalue(plotpsvar)<-"TRUE"
            tclvalue(deprofvar)<-"FALSE"
            tclvalue(noisevar)<-500
            tclvalue(Nvar)<-5
            tclvalue(Threshvar)<-20
            tclvalue(m1var)<-1.003
            tclvalue(m2var)<-1.997
            tclvalue(mdiffvar)<-0.05
            tclvalue(ppmerrvar)<-50
            # tclvalue(valvar)<-"maxo"
            tclvalue(mdiff2var)<-0.001
            tclvalue(myfilevar)<-"halo_list.csv"
            tclvalue(myfile2var)<-"halo_list_short.csv"
            tclvalue(m1needvar)<-"FALSE"
        }
        
    ################################
    # Function to launch computations
    ################################
        "execcomp" <- function()
        {
        outdir<-as.character(tclvalue(outdirvar))
        if (outdir=="" ||is.null(outdir)==TRUE){
                    outdir<-tclvalue(tkchooseDirectory(title="Choose your Results folder"))
        }
        indir<-tclvalue(indirvar)
            
        print("Processing...")
        Sys.sleep(0.5)
        tkdestroy(tt)
           
        #test if xsAnnotate object exists in MeHaloCoAenv or .GlobalEnv
        # if(length(which(ls(MeHaloCoAenv)==tclvalue(xsAnnotatevar)))==0){
            # an<-get(ls(.GlobalEnv)[which(ls(.GlobalEnv)==tclvalue(xsAnnotatevar))], envir=as.environment(.GlobalEnv))
            # print("Note: Use of an xsAnnotate object from GlobalEnv as entry")
        # }else
        # {
            # an<-get(ls(MeHaloCoAenv)[which(ls(MeHaloCoAenv)==tclvalue(xsAnnotatevar))], envir=as.environment(MeHaloCoAenv))
        # }

        # if((class(an)=="xsAnnotate")==FALSE){
            # xsAnnotate<-an[as.vector(which(lapply(an,function(x)class(x))=="xsAnnotate"))]
        # }else
        # {
            # xsAnnotate<-an
        # }
        # print(xsAnnotate)
    filepattern <- c("[Cc][Dd][Ff]", "[Nn][Cc]", "([Mm][Zz])?[Xx][Mm][Ll]","[Mm][Zz][Dd][Aa][Tt][Aa]", "[Mm][Zz][Mm][Ll]")
    filepattern <- paste(paste("\\.", filepattern, "$", sep = ""),collapse = "|") 
    samples<-list.files(path=tclvalue(indirvar), pattern=filepattern, all.files=FALSE,recursive=TRUE,full.names=TRUE,ignore.case=FALSE)
    
    if (length(samples)>1){  
      print("several files")
       assign("matcl",vector(mode="list", length=length(samples)), envir=as.environment("MeHaloCoAenv"))
       
     
      projectpdf(dir=outdir,projectName="IFC", ext="pdf")
    
          mat<-list()
          for (i in 1:length(samples)){
        #   scanrange=NULL
            MeHaloCoAenv$matcl[[i]]<-do.call(cldetect.scan,c(outdir=outdir,file=samples[[i]],list( 
            # plotps=tclvalue(plotpsvar),
            deprof=tclvalue(deprofvar),
           noise=as.numeric(tclvalue(noisevar)),
           N=as.numeric(tclvalue(Nvar)),
            Thresh=as.numeric(tclvalue(Threshvar)),
            m1=as.numeric(tclvalue(m1var)),
            m2=as.numeric(tclvalue(m2var)),
            mdiff=as.numeric(tclvalue(mdiffvar)),
            ppmerr=as.numeric(tclvalue(ppmerrvar)),
            mdiff2=as.numeric(tclvalue(mdiff2var)),
            myfile=paste("file_",i,"_",tclvalue(myfilevar)),
            myfile2=tclvalue(myfile2var),
            m1need=tclvalue(m1needvar)
          )
          ))
          }
    dev.off()
            for (i in 1:length(MeHaloCoAenv$matcl)){
                MeHaloCoAenv$matcl[[i]]<-cbind(basename(samples[[i]]),MeHaloCoAenv$matcl[[i]])
                MeHaloCoAenv$matcl[[i]] <- data.frame(lapply(MeHaloCoAenv$matcl[[i]], as.character), stringsAsFactors=FALSE)
                colnames(MeHaloCoAenv$matcl[[i]])[1]<-"File"
            }
            mat<-do.call("rbind",MeHaloCoAenv$matcl)
            
            write.table(mat, file=file.path(outdir, tclvalue(myfile2var)), row.names=FALSE, sep="\t")
    }
    else{
      projectpdf(dir=outdir,projectName="IFC", ext="pdf")
      
        assign(tclvalue(matclvar), do.call(cldetect.scan,c(outdir=outdir,file=samples[[1]],list( 
          # plotps=tclvalue(plotpsvar),
          deprof=tclvalue(deprofvar),
        noise=as.numeric(tclvalue(noisevar)),
           N=as.numeric(tclvalue(Nvar)),
          Thresh=as.numeric(tclvalue(Threshvar)),
          m1=as.numeric(tclvalue(m1var)),
          m2=as.numeric(tclvalue(m2var)),
          mdiff=as.numeric(tclvalue(mdiffvar)),
          ppmerr=as.numeric(tclvalue(ppmerrvar)),
          # val=tclvalue(valvar),
          mdiff2=as.numeric(tclvalue(mdiff2var)),
          myfile=tclvalue(myfilevar),
          myfile2=tclvalue(myfile2var),
          m1need=tclvalue(m1needvar)
        )
        )), envir=as.environment("MeHaloCoAenv"))
      dev.off()
    }
                
            
                tkmessageBox(title="Done",message= "cldetect Step is Done",icon="info",type="ok")
                
            
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

    HELPFrame <- tkframe(tt, relief="groove", borderwidth=2)
    tkgrid(tklabel(HELPFrame,text="- cldetect help -" , foreground="blue"),columnspan=2,  sticky="we")
    tkgrid(tklabel(HELPFrame,text="indir:     your Data directory " ),columnspan=2,  sticky="w") 
    tkgrid(tklabel(HELPFrame,text="outdir:     your results directory " ),columnspan=2,  sticky="w") 
    
    # tkgrid(tklabel(HELPFrame,text="plotps:     Draw nice Isotopic Filtered Chromatogram (IFC)  " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="deprof: default=FALSE if TRUE deprofile.scan from RMassBank is applied on each pspectra" ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="noise: noise level below which scan are no taken " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="N: number of scan merged before calculation N/2 scan before target + N/2 scan after " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="Thresh: threshold of intensity for M+2 here 30 " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="m1: mass defect between M and M+1 isotope (here 1.0033) " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="m2: mass defect between M and M+2 isotope (here 1.997 for Cl) " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="mdiff: mdiff differences in Da applied to neutral loss search" ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="ppmerr: ppm margin tolerated by cldetect " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="m1need: Should M+1 isotope be present? if FALSE peaks with no M+1 are considered (interesting for low intensity compounds) " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="val: can be into or maxo, area  or intensity respectively  " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="mdiff2: mdiff2 differences in Da applied to IFC chromatogram drawing " ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="myfile: name of the output file default halo_list.csv" ),columnspan=2,  sticky="w")
    tkgrid(tklabel(HELPFrame,text="myfile2: name of the output file default halo_list_short.csv is a resume from all individual myfile\n in the case off several files have been processed " ),columnspan=2,  sticky="w")

    tkgrid(HELPFrame, sticky="we")
    #
    # User closed the window
    #
        if(tclvalue(done)=="2") return()
}
        

}

