########### IMPORTA UNA TAULA DE ACCESS I INCORPORA DICCIONARI DE VARIABLES I FA UNA EXPORTACIO A SPSS ################



## carreguem les llibreries i funcions necessaries

if (!"carrega.llibreria"%in%objects()) source(file.path(RutinesLocals,"carrega.llibreria.r"))
carrega.llibreria(c("gtools","gdata"))
if (!"export.SPSS"%in%objects()) source(file.path(RutinesLocals,"export.SPSS.r"))
if (!"import.ACESS"%in%objects()) source(file.path(RutinesLocals,"import.ACCESS.r"))
if (!"arregla.formats"%in%objects()) source(file.path(RutinesLocals,"arregla.formats.r"))



import.ACCESS2<-function(file.mbd,nom.taula,nom.variables=list("ALL"),nom.dicc="",file.spss="",var.dicc="",noms.taules=gsub(" ","_",nom.taula),fix.formats=TRUE){

#file.mbd="U:\\Estudis\\clinic\\mascara\\Dades\\TELEFORM\\mascara.mdb"
#nom.taula=c("dades minimes")
#nom.variables=list(c("ALL"))
#nom.dicc="Diccionario"
#file.spss=""
#var.dicc=c("nombre","Etiqueta_variable","Etiqueta_valor","tabla")
#noms.taules=c("mcr")
#fix.formats=TRUE

  nom.taula<-import.ACCESS(file=file.mbd,nom.taula=nom.taula,nom.variables=nom.variables,nom.taula.R=noms.taules)

  if (nom.dicc!=""){ # si existeix una taula de diccionari etiqueta la/es taula/es
    nom.dict<-import.ACCESS(file=file.mbd,nom.taula=nom.dicc)  
    assign("dict",get(nom.dict))
    names(dict)<-tolower(names(dict))
    dict<-arregla.formats(dict)
  }
  
  for (i in 1:length(nom.taula)){

    assign("taula",get(nom.taula[i]))
    names(taula)<-tolower(names(taula))
    nom.taula.access<-attr(taula,"table.origen")
    
    if (nom.dicc!=""){ # va etiquetant les taules

      dict2<-dict
      names(dict2)<-tolower(names(dict2))
      var.dicc<-tolower(var.dicc)

      if (length(var.dicc)==4) 
        dict2<-subset(dict,tolower(dict[,var.dicc[4]])==tolower(nom.taula.access))  # els noms de les taules no son case-sensitive.
      
      dict2[,var.dicc[1]]<-tolower(as.character(dict2[,var.dicc[1]]))
      
      comuns<-names(taula)[names(taula)%in%dict2[,var.dicc[1]]]
      comuns<-as.character(comuns)
      dict2<-subset(dict2,dict2[,var.dicc[1]]%in%comuns)
            
      noms.table<-table(dict2[,var.dicc[1]])
      noms.repes<-names(noms.table)[noms.table>1]
      dict2.repe<-subset(dict2,dict2[,var.dicc[1]]%in%noms.repes)
      dict2.repe<-dict2.repe[order(dict2.repe[,var.dicc[1]]),]
      if (length(noms.repes)>0){
        cat("Advertencia: les variables",paste(noms.repes,collapse=","),"no s'han etiquetat perque estan repetits\n")
        #print(dict2.repe)
        #fix2(dict2.repe)
        cat("\n")
        dict2<-subset(dict2,!dict2[,var.dicc[1]]%in%noms.repes)
        comuns<-names(taula)[names(taula)%in%dict2[,var.dicc[1]]]
        comuns<-as.character(comuns)      
      }
      
      dict2[,var.dicc[2]]<-gsub("\"","",as.character(dict2[,var.dicc[2]]))

      for (j in which(names(taula)%in%comuns)){
        index<-which(dict2[,var.dicc[1]]==names(taula)[j])
        attr(taula[,j],"vari.label")<-dict2[index,var.dicc[2]]
        value.labels<-as.character(dict2[index,var.dicc[3]])
        if (!is.na(value.labels) & value.labels!=""){
          value.labels<-gsub(" *= *","=",value.labels)
          value.labels<-gsub(" *; *",";",value.labels)
          value.labels<-gsub(";",",\"",value.labels)
          value.labels<-gsub("=","\"=",value.labels)
          value.labels<-paste("value.labels=c(\"",value.labels,")",sep="")
          print(dict2[index,var.dicc[1]])
          eval(parse(text=value.labels))
          attr(taula[,j],"value.labels")<-value.labels
        }
      }
    }

    if (fix.formats)
      taula<-arregla.formats(taula,force=TRUE)

    if (file.spss!=""){ # si es vol exportar a SPSS file.spss ha d'estar especificat
      export.SPSS(taula,file.save=ifelse(file.spss=="",choose.files(".sav",paste(nom.taula.access,"Guardar com")),file.spss[i]))
    }

    assign(noms.taules[i], taula, env=.GlobalEnv)

  }

  return(noms.taules)

}




