


import.ms3<-function(carpeta){

  # carpeta: carpeta on estan els arxius ms3
  # result: arxiu .sav on van a parar els rsultats.

  # llegeix nom dels arxius
  arxius<-list.files(carpeta)
  list.caso<-as.integer(substr(arxius,1,nchar(arxius)-4))
  keep<-!is.na(list.caso)
  arxius<-arxius[keep]
  list.caso<-list.caso[keep]
  N<-length(arxius)

  ## inicialització
  i<-1
  l<-nchar(arxius[i])
  caso<-list.caso[i]
  n<-as.integer(read.table(file.path(carpeta,arxius[i]),nrows=1))
  aux<-read.table(file.path(carpeta,arxius[i]),skip=1,nrows=n,sep=",")
  var1<-paste("a",aux$V1,"_",aux$V2,sep="")
  var2<-aux$V3
  taula<-as.data.frame(aggregate(var2,list(var1),sum))
  names(taula)<-c("alimento",caso)

  ## bucle
  for (i in 2:N){
    l<-nchar(arxius[i])
    caso<-list.caso[i]
    n<-as.integer(read.table(file.path(carpeta,arxius[i]),nrows=1))
    aux<-read.table(file.path(carpeta,arxius[i]),skip=1,nrows=n,sep=",")
    var1<-paste("a",aux$V1,"_",aux$V2,sep="")
    var2<-aux$V3
    temp<-as.data.frame(aggregate(var2,list(var1),sum))
    names(temp)<-c("alimento",caso)
  	taula<-merge(taula,temp,all=TRUE,by.x="alimento")
  }

  #transposem les dades
  caso<-names(taula)[2:N+1]
  taula2<-t(as.matrix(taula[,1:N+1]))

  # suposem que els NA són zeros.
  taula2<-ifelse(is.na(taula2),0,taula2)

  # ho convertim amb data.frame amb la primera variable que serà el 'caso'
  taula2<-as.data.frame(taula2)
  names(taula2)<-taula$alimento
  taula2<-cbind(row.names(taula2),taula2)
  taula2<-data.frame(taula2,row.names=NULL)
  names(taula2)<-c("caso",as.character(taula$alimento))

  return(taula2)

}









