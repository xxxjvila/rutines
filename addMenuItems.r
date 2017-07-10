##.path<-"U:/ULEC/Software/R cran project/rutines"
.path<-RutinesLocals




winMenuAdd("Rutines")
winMenuAdd("Rutines/Interactives")
winMenuAdd("Rutines/No interactives")


winMenuAddItem("Rutines/Interactives","analisi haplotips",paste("source(\"",.path,"/analisi haplotips (versio 2.7).r\")",sep=""))
#winMenuAddItem("Rutines/Interactives","calibration (VERIFICA)",paste("source(\"",.path,"/CALIBRATION (interactiu).r\")",sep=""))
winMenuAddItem("Rutines/Interactives","calibration (VERIFICA)","disable")
winMenuAdd("Rutines/Interactives/control de qualitat")
winMenuAddItem("Rutines/Interactives/control de qualitat","file",paste("source(\"",.path,"/control de qualitat.r\")",sep=""))
winMenuAddItem("Rutines/Interactives/control de qualitat","current file",paste("source(\"",.path,"/control de qualitat (current file).r\")",sep=""))
winMenuAddItem("Rutines/Interactives","correspondència etiquetes de valor",paste("source(\"",.path,"/correspondència etiquetes de valor.r\")",sep=""))
winMenuAddItem("Rutines/Interactives","dictionari de variables",paste("{source(\"",.path,"/dictionary.r\");dictionary()}",sep=""))
winMenuAddItem("Rutines/Interactives","lectura ACCESS",paste("source(\"",.path,"/lectura ACCESS.r\")",sep=""))
winMenuAddItem("Rutines/Interactives","matching",paste("source(\"",.path,"/matching.r\")",sep=""))
winMenuAddItem("Rutines/Interactives","taules_bivariades",paste("source(\"",.path,"/taules_bivariades.r\")",sep=""))



winMenuAddItem("Rutines/No interactives","spss_var",paste("source(\"",.path,"/spss_var.rpk\")",sep=""))





winMenuAdd("Utilitats")
winMenuAdd("Utilitats/Obrir arxiu")
winMenuAddItem("Utilitats","Tenca devices","graphics.off()")
winMenuAddItem("Utilitats/Obrir arxiu","current files",paste("{source(\"",.path,"/current.files.r\");current.files()}",sep=""))
winMenuAddItem("Utilitats/Obrir arxiu","file","shell.exec(choose.files(caption=\"Selecciona els arxius que vols obrir\"))")
winMenuAddItem("Utilitats","Cercar variables",paste("{source(\"",.path,"/cerca.variables.r\");cerca.variables()}",sep=""))
winMenuAddItem("Utilitats","Etiquetes i formats",paste("source(\"",.path,"/spss_var_interactiu.rpk\")",sep=""))
winMenuAddItem("Utilitats","Paste data.frame names",paste("source(\"",.path,"/print names data.frame.r\")",sep=""))
#winMenuAddItem("Utilitats","Visualitza data.frame o matrix",paste("{source(\"",.path,"/fix2.r\");fix2()}",sep=""))
winMenuAddItem("Utilitats","Arregla formats",paste("{source(\"",.path,"/arregla.formats.r\");arregla.formats()}",sep=""))


winMenuAdd("Bases de dades")
winMenuAdd("Bases de dades/Importar")
winMenuAddItem("Bases de dades/Importar","SPSS",paste("{source(\"",.path,"/read.spss4.r\");read.spss4()}",sep=""))
winMenuAddItem("Bases de dades/Importar","ACCESS",paste("{source(\"",.path,"/import.ACCESS.r\");import.ACCESS()}",sep=""))


winMenuAdd("Paquets utils")
winMenuAddItem("Paquets utils","Rcmdr",paste("source(\"",.path,"/download Rcmdr.r\")",sep=""))



winMenuAdd("Bases de dades")
winMenuAdd("Bases de dades/Exportar")
winMenuAddItem("Bases de dades/Exportar","SPSS",paste("{source(\"",.path,"/export.SPSS.r\");export.SPSS()}",sep=""))

