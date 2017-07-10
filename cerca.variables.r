cerca.variables<-function(dades=NULL,find=1,agrep=FALSE,max.distance=0.1){
  
  if (is.null(dades)){  # tria interactivament el data.frame
    objects<-ls(envir=.GlobalEnv)
    class<-sapply(objects,function(x) class(get(x,envir=.GlobalEnv)))
    objects<-objects[class=="data.frame"]
    object<-select.list(objects,title="Tria el data.frame",multiple=FALSE)
    dades<-get(object,envir=.GlobalEnv)
  }
  

	triat<-character(0)

	if (find==1){
		part<-winDialogString(message="escriu part del nom", default="")
		if (agrep) res<-agrep(part,names(dades),ignore.case = TRUE,value=FALSE,max.distance=max.distance)
		if (!agrep) res<-grep(part,names(dades),ignore.case = TRUE,value=FALSE)
	}

	labels<-unlist(lapply(dades,function(x){
                                  aux<-attr(x,"vari.label")
                                  return(ifelse(is.null(aux),"",aux))
                                }
              ))
              
              
	if (!is.null(labels) & find==2){
		part<-winDialogString(message="escriu part de la etiqueta", default="")
		if (agrep) res<-agrep(part,labels,ignore.case = TRUE,value=FALSE,max.distance=max.distance)
		if (!agrep) res<-grep(part,labels,ignore.case = TRUE,value=FALSE)
	}

	if (length(res)>0 ){
		triat<-menu(paste(names(labels),labels,sep=":   ")[res],graphics=TRUE)	
		return(names(dades)[res[triat]])
	}
	if (length(res)==0) return(paste("No s'ha trobat cap variable amb el patró \"",part,"\"",sep=""))

}




