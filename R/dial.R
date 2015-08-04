#MeHaloCoA dialog box
# Part of the code is based on the ade4TkGUI package by Jean Thioulouse <jthioulouse@biomserv.univ-lyon1.fr>, Stephane
#       Dray <dray@biomserv.univ-lyon1.fr>
#
.dial <-
function()
{
# Main dialog window with title
#
    tt <- tktoplevel()
    tkwm.title(tt,"MeHaloCoA")

#
# Menu setup
#
    frame0 <- tkframe(tt, relief="groove", borderwidth=2, background="grey")

    topMenuMeHaloCoA <- tkmenubutton(frame0, text="Menu", background="grey")
    MeHaloCoAMenu <- tkmenu(topMenuMeHaloCoA, tearoff=TRUE)
    tkconfigure(topMenuMeHaloCoA, menu=MeHaloCoAMenu)
    
    openRecentMenu <- tkmenu(topMenuMeHaloCoA,tearoff=FALSE)
        tkadd(openRecentMenu,"command",label="xcms::matchedFilter",    command=function() .dialog.mehalocoa.CDF2RData(method="matchedFilter"))
        tkadd(openRecentMenu,"command",label="xcms::centWave",    command=function() .dialog.mehalocoa.CDF2RData(method="centWave"))
        tkadd(openRecentMenu,"command",label="scan by scan",    command=function() .dialog.mehalocoa.cldetect(method="scan"))
                
    
    openRecentMenu1 <- tkmenu(topMenuMeHaloCoA,tearoff=FALSE)
        tkadd(openRecentMenu1,"command",label="cldetect",    command=function() .dialog.mehalocoa.cldetect(method=""))
        tkadd(openRecentMenu1,"command",label="cldetect.pcgroup",    command=function() .dialog.mehalocoa.cldetect(method="pcgroup"))
        tkadd(openRecentMenu1,"command",label="cldetect.scan",    command=function() .dialog.mehalocoa.cldetect(method="scan"))
        
    
    
        
    tkadd(MeHaloCoAMenu,"cascade",label="Step1-PeakPicking...",menu=openRecentMenu)
    tkadd(MeHaloCoAMenu,"separator")    
    tkadd(MeHaloCoAMenu,"cascade",label="Step2-Isotopic Filter...",menu=openRecentMenu1)
    tkadd(MeHaloCoAMenu,"separator")    
    
    tkadd(MeHaloCoAMenu,"command",label="Quit R",command=function() q())
    
    topMenuHelp <- tkmenubutton(frame0, text="Help", background="grey")
    MeHaloCoAMenu <- tkmenu(topMenuHelp, tearoff=TRUE)
    tkconfigure(topMenuHelp, menu=MeHaloCoAMenu)
    tkadd(MeHaloCoAMenu,"command",label="MeHaloCoA(Help)",command=function()print(help("MeHaloCoA")) )
    
    
    topMenuURL <- tkmenubutton(frame0, text="Links", background="grey")
    MeHaloCoAMenu <- tkmenu(topMenuURL, tearoff=TRUE)
    tkconfigure(topMenuURL, menu=MeHaloCoAMenu)
    tkadd(MeHaloCoAMenu,"command",label="MeHaloCoA web site",command=function()shell.exec("http://sites.google.com/site/rpackageMeHaloCoA/") )

    
    tkpack(topMenuMeHaloCoA,topMenuHelp,topMenuURL,side="left")
    tkpack(frame0, expand="TRUE", fill="x")
#
# title and icons
#
    frame1 <- tkframe(tt, relief="groove", borderwidth=2, background="white")

    labh <- tklabel(frame1, bitmap="questhead", background="white")
    tkbind(labh, "<Button-1>", function() print(help("runGUI")))
    titre <- tklabel(frame1,text="MeHaloCoA even more easy", font="Times 14", foreground="red", background="white")
    
    helplab <- tklabel(frame1,text="- Right click buttons for help - click in check boxes to unselect -", font="Times 11", foreground="dark green", background="white")


    tkgrid(titre, labh, padx=10, sticky = "w")
    tkgrid(helplab, columnspan=4)
    tkpack(frame1, expand="TRUE", fill="x")
#
# MeHaloCoA step 1 button
#
    frame1b <- tkframe(tt,  borderwidth=2, background="white")
    tkpack(tklabel(frame1b,text="- MeHaloCoA -", font="Times 12", foreground="blue", background="white"))    
    tkpack(tklabel(frame1b,text="- Check a Peak Picking Options -", foreground="blue", background="white"))
#check box parameters
        matchedFiltervar <- tclVar(1)
        centWavevar <- tclVar(0)
        byscanvar <- tclVar(0)
        #button step 1 befo check box
    if(tclvalue(matchedFiltervar)==1 && tclvalue(centWavevar)==0 && tclvalue(byscanvar)==0){
        step1.but <- tkbutton(frame1b, text="Step1- PeakPicking MF",width=20,  command=function() {.dialog.mehalocoa.CDF2RData(method="matchedFilter");cat("\n MF\n")})
    }        
    
#check box
    scannf.cbut <- tkcheckbutton(frame1b,text="matchedFilter", variable=matchedFiltervar,
        command=function() if (tclvalue(matchedFiltervar)==1) {
                cat("MF ");
                tkconfigure(scannf.cbut1,state="disabled");tkconfigure(scannf.cbut2,state="disabled");
                tclvalue(matchedFiltervar)=1;tclvalue(centWavevar)=0;tclvalue(byscanvar)=0;
                tkconfigure(step1.but, text="Step1- PeakPicking MF",  command=function() {.dialog.mehalocoa.CDF2RData(method="matchedFilter")})
            }else{
                tkconfigure(scannf.cbut,state="normal");tkconfigure(scannf.cbut1,state="normal");tkconfigure(scannf.cbut2,state="normal");
                tclvalue(matchedFiltervar)=0;tclvalue(centWavevar)=0;tclvalue(byscanvar)=0;
                tkconfigure(step1.but, text="Step1- PeakPicking ?", command=function() {.dialog.mehalocoa.CDF2RData(method="")})}, background="grey")
                
    scannf.cbut1 <- tkcheckbutton(frame1b,text="centWave", variable=centWavevar,
        command=function() if (tclvalue(centWavevar)==1) {
                cat("CW ");
                tkconfigure(scannf.cbut,state="disabled");tkconfigure(scannf.cbut2,state="disabled");
                tclvalue(matchedFiltervar)=0;tclvalue(centWavevar)=1;tclvalue(byscanvar)=0;
                tkconfigure(step1.but, text="Step1- PeakPicking CW", command=function() {.dialog.mehalocoa.CDF2RData(method="centWave")})
            }else{
                tkconfigure(scannf.cbut,state="normal");tkconfigure(scannf.cbut1,state="normal");tkconfigure(scannf.cbut2,state="normal");
                tclvalue(matchedFiltervar)=0;tclvalue(centWavevar)=0;tclvalue(byscanvar)=0;
                tkconfigure(step1.but, text="Step1- PeakPicking ? ",  command=function() {.dialog.mehalocoa.CDF2RData(method="")})}, background="grey")
                
    scannf.cbut2 <- tkcheckbutton(frame1b,text="scan by scan", variable=byscanvar,
        command=function() if (tclvalue(byscanvar)==1) {
            cat("scan ");
            tkconfigure(scannf.cbut,state="disabled");tkconfigure(scannf.cbut1,state="disabled");
            tclvalue(matchedFiltervar)=0;tclvalue(centWavevar)=0;tclvalue(byscanvar)=1;
            tkconfigure(step1.but, text="All in one scan by scan", command=function() {.dialog.mehalocoa.cldetect(method="scan")});
            tkconfigure(step2.but, text="not needed", state="disabled",  command=function() {.dialog.mehalocoa.CDF2RData(method="")})
        }else{
            tkconfigure(scannf.cbut,state="normal");tkconfigure(scannf.cbut1,state="normal");tkconfigure(scannf.cbut2,state="normal");
            tclvalue(matchedFiltervar)=0;tclvalue(centWavevar)=0;tclvalue(byscanvar)=0;
            tkconfigure(step1.but, text="Step1- PeakPicking ?",  command=function() {.dialog.mehalocoa.CDF2RData(method="")});
            tkconfigure(step2.but, text="Step2- Isotopic Filter", state="normal",  command=function() {.dialog.mehalocoa.cldetect(method="")})}, background="grey")
            
            
     tkconfigure(scannf.cbut1,state="disabled")
     tkconfigure(scannf.cbut2,state="disabled")
     tkpack(scannf.cbut,scannf.cbut1,scannf.cbut2)
    
    
    tkpack(step1.but,ipadx=25, side="bottom", expand="TRUE")
    
    tkpack(frame1b, expand="TRUE", fill="x")

    tkbind(step1.but, "<Button-3>", function() print(help("CDF2RData")))
    

#
# MeHaloCoA Step 2 button
#
    frame2 <- tkframe(tt,  borderwidth=2, background="white")
    step2.but <- tkbutton(frame2, text="Step2- Isotopic Filter", width=20, command=function() .dialog.mehalocoa.cldetect(method=""))
    
    tkpack(step2.but,ipadx=25, side="top", expand="TRUE")
    tkpack(frame2, expand="TRUE", fill="x")
    
    tkbind(step2.but, "<Button-3>", function() print(help("cldetect")))
    

#
# Quit
#
    frame5 <- tkframe(tt, relief="groove", borderwidth=2, background="white")
    cancel.but <- tkbutton(frame5, text="Cancel", command=function() tkdestroy(tt), font="Times 14")
    quity.but <- tkbutton(frame5, text="Quit R (save)", command=function() q("yes"), font="Times 14")
    quitn.but <- tkbutton(frame5, text="Quit R (don't save)", command=function() q("no"),  font="Times 14")
    tkpack(quity.but, cancel.but, quitn.but, side="left", expand="TRUE", fill="x")
    tkpack(frame5, expand="TRUE", fill="x")
    tkfocus(tt)
    return(invisible())
##on.exit
#on.exit(detach(e1))
}

