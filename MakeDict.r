MakeDict <- function(data){
  xdat <- data  
  value.labels<-NULL
    for (i in 1:ncol(xdat)){
      temp<-attr(xdat[,i],"value.labels")
      if (!is.null(temp)){
        temp<-sort(temp)
        temp<-paste(paste(names(temp),"=",temp,sep=""),collapse=";")
      } else {
        temp<-""
      }
      value.labels<-c(value.labels,temp)
    }
    vari.label<-as.character(lapply(xdat,function(x) attr(x,"vari.label")))
    dict<-data.frame(varname=names(xdat),label=vari.label,valuelabels=value.labels)
    dict$label<-as.character(dict$label)
    dict$label<-ifelse(dict$label=="NULL", "", dict$label)
    return(dict)
}


