if (!"carrega.llibreria"%in%ls()) source(file.path(RutinesLocals,"carrega.llibreria.r"))
carrega.llibreria(c("car")) # per a la funci? replace




## funci? que reempla?a els miss.values de una variable per NA, i assigna TRUE a un nou atribut anomenat miss.values.arreglat.

arregla.miss.values<-function(x){

  if (is.list(x)){ res<-lapply(x,arregla.miss.values)
  } else {
    miss.values<-attr(x,"miss.values")
    if (!is.null(miss.values)){   ## no fa res.
      expr<-paste(paste(miss.values,"=NA",sep=""),collapse="; ")
      attr(x,"miss.values.arreglat")=TRUE
      res<-car::recode(x,expr)
    } else 
      res<-x
    
  }
  
  return(res)

}
