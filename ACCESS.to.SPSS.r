########### IMPORTA UNA TAULA DE ACCESS I INCORPORA DICCIONARI DE VARIABLES I FA UNA EXPORTACIÓ A SPSS ################



## carreguem les llibreries i funcions necessàries

if (!"carrega.llibreria"%in%objects()) source(file.path(RutinesLocals,"carrega.llibreria.r"))
carrega.llibreria(c("gtools","gdata"))
if (!"export.SPSS"%in%objects()) source(file.path(RutinesLocals,"export.SPSS.r"))
if (!"import.ACESS"%in%objects()) source(file.path(RutinesLocals,"import.ACCESS.r"))
shell.exec(file.path(RutinesLocals,"import.ACCESS.r"))
source(file.path(RutinesLocals,"fix2.r"))



ACCESS.to.SPSS<-function(file.mbd, file.spss, nom.taula, nom.dicc="",var.dicc=c("var","label","value.labels","table"),
			file.runsyntax="C:\\Archivos de programa\\SPSS\\runsyntx.exe"){

  nom.taula<-import.ACCESS(file=file.mbd,nom.taula=nom.taula)

  if (nom.dicc!=""){
    nom.dict<-import.ACCESS(file=file.mbd,nom.taula=nom.dicc)
    assign("dict",get(nom.dict))
    names(dict)<-tolower(names(dict))
  }
  
  for (i in 1:length(nom.taula)){

    assign("taula",get(nom.taula[i]))

    names(taula)<-tolower(names(taula))

    if (nom.dicc!=""){
    
      nom.taula.access<-attr(taula,"table.origen")
      dict2<-subset(dict,dict[,var.dicc[4]]==nom.taula.access)
      dict2[,var.dicc[1]]<-tolower(as.character(dict2[,var.dicc[1]]))
      comuns<-names(taula)[names(taula)%in%dict2[,var.dicc[1]]]
      comuns<-as.character(comuns)
      dict2[,var.dicc[2]]<-gsub("\"","",as.character(dict2[,var.dicc[2]]))

      for (j in which(names(taula)%in%comuns)){
        index<-which(dict2[,var.dicc[1]]==names(taula)[j])
        attr(taula[,j],"vari.label")<-dict2[index,var.dicc[2]]
        value.labels<-as.character(dict2[index,var.dicc[3]])
        if (!is.na(value.labels)){
          value.labels<-gsub(";",",\"",value.labels)
          value.labels<-gsub("=","\"=",value.labels)
          value.labels<-paste("value.labels=c(\"",value.labels,")",sep="")
          print(dict2[index,var.dicc[1]])
          eval(parse(text=value.labels))
          attr(taula[,j],"value.labels")<-value.labels
        }
      }

    }

    taula<-arregla.formats(taula)
  
    export.SPSS(taula,file.save=file.spss[i],file.runsyntax=file.runsyntax)

  }
  
}




####  EXEMPLE   ########


#source(file.path(RutinesLocals,"ACCESS.to.SPSS.r"))


#ACCESS.to.SPSS("U:\\Estudis\\Genetica\\MIGEN\\dades\\migen051206.mdb",
#            "U:\\Estudis\\Genetica\\MIGEN\\dades\\XXX.SAV",
#            nom.taula = "PARELLES",
#            nom.dicc = "",
#            var.dicc = c("var", "label", "value.labels", "table")) ## si no hi ha diccionari, no cal aquesta informacio


