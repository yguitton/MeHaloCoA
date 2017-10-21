.onLoad<- function(libname = find.package("MeHaloCoA"), pkgname = "MeHaloCoA") 
#create environment for results if in interactive Mode
MeHaloCoAenv<<-new.env()
# invisible()
#this allow use of tcltk functionalities
# attach(MeHaloCoAenv)
#test if we can launch runGUI()
if (interactive()){
    try(runGUI())
}
}#end .onload
 
 
# Remove config environment on unload -------------------------------------
.onUnload <- function(libname = find.package("MeHaloCoA"), pkgname = "MeHaloCoA") {
  rm(MeHaloCoAenv,envir = globalenv())
}

