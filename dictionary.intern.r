
### CREA UN FITXER *.SPS DE VALUE LABELS I VARI LABELS D'UNA BASE DE DADES DE R ####


dictionary.intern<-function(taula=NULL,file.res=""){


# files: bases de dades SPSS (.sav) de la qual extreiem la informacio de les vari i value labels.
# file.res: arxiu de sintaxis (.sps) on va a parar tota aquesta informacio.

  inter=FALSE 

  if (is.null(taula)){
    inter=TRUE
    objects<-ls(envir=.GlobalEnv)
    sel<-select.list(objects,title="Data frame?")
    taula<-get(sel,envir=.GlobalEnv)
    if (!is.data.frame(taula)) stop("La taula escollida no Es un data.frame")
  }

  if (inter){
    file.res<-winDialogString(message="Arxiu de resultats (recorda posar l'extensio .sps)", default="")
  }
    
  write("\n\n",file.res)
      
  vari.labels<-unlist(sapply(taula, function(x) ifelse(is.null(attr(x,"vari.label")),"",attr(x,"vari.label")))) 
  noms.var<-names(taula)
  missings<-sapply(taula, function(x) attr(x,"miss.values"))     # els missings queden com una llista
  value.labels.list<-lapply(taula,function(x) attr(x,"value.labels"))


  ## etiquetes de les variables ##

  vari.labels.sint<-
      c("vari labels",
      paste("\t",noms.var,"\t\"",vari.labels,"\"",
      c(rep("",length(vari.labels)-1),"."),sep=""))

  write(paste("\n\n\n********** DICTIONARY: ****************.",sep=""),file.res,append=TRUE)
  write(" ",file.res,append=TRUE)
  write(" ",file.res,append=TRUE)
  write("*** VARI LABELS ***.",file.res,append=TRUE)
  write(vari.labels.sint,file.res,append=TRUE)

  ## etiquetes de valor ##

  value.labels.sint=NULL

  if (length(value.labels.list)>0){

    for (j in 1:length(value.labels.list)){
    
      #print(j);print(names(taula)[j])
    
      value.labels<-value.labels.list[[j]]

      if (!is.null(value.labels)){
		
		    values<-as.character(value.labels)

		    if (length(grep("\\)",values))==0){     # PER A LES VARIABLES FORMAT DATETIME NO SE COM EXPORTAR LES SEVES VALUE.LABELS A SPSS.
		
		      if (length(grep("[0-9]-",values))>0) values<-paste("'",values,"'",sep="")
        
          value.labels<-data.frame(labels=names(value.labels),values=values)
 
          aux<-c(paste("value labels",noms.var[j]),
              paste("\t",value.labels[,2],"\t\"",value.labels[,1],"\"",
              c(rep("",nrow(value.labels)-1),"."),sep=""))

          value.labels.sint=c(value.labels.sint," ",aux)

          if (length(value.labels.list[[j]])>1){
            taula.value.labels<-cbind(c(noms.var[j],rep("",nrow(value.labels)-1)),c(vari.labels[j],rep("",nrow(value.labels)-1)),value.labels)
          } else taula.value.labels<-c(noms.var[j],vari.labels[j],value.labels)

        }

      }
      
    }
    
    write(" ",file.res,append=TRUE)
    write("*** VALUE LABELS ***.",file.res,append=TRUE)
    write(value.labels.sint,file.res,append=TRUE)

  }
	
  ## missings ##

  if (length(missings)>0){  ## si hi ha algun missing declarat
    missing.sint=NULL
    for (j in 1:length(missings)){
      if (!is.null(missings[[j]])){
        aux.miss<-paste("(",paste(missings[[j]],collapse=" "),")",sep="")
        aux.miss<-gsub(":"," thru ",aux.miss)
        missing.sint<-c(missing.sint,paste("MISSING VALUES ",names(missings)[j]," ",aux.miss,".",sep=""))
      }
    }
    write(" ",file.res,append=TRUE)
    write("*** MISSING VALUES ***.",file.res,append=TRUE)
    write(" ",file.res,append=TRUE)
    write(missing.sint,file.res,append=TRUE)
  }
  #file.show(file.res)

}













