.onLoad<-function(libname, pkgname){
#create environment for results if in interactive Mode
MeHaloCoAenv<-new.env()
invisible()
#this allow use of tcltk functionalities

#test if we can launch runGUI()
if (interactive()){
    try(runGUI())
}
}#end .onload
 
 


