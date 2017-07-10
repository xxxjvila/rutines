format2<-function(x,digits=NULL,...){

  if (!is.null(digits)){             
    res<-format(round(x,digits),trim=TRUE,nsmall=digits,...)
    res<-ifelse(res==paste("0.",paste(rep(0,digits),collapse=""),sep=""),paste("<0.",paste(rep(0,digits-1),collapse=""),"1",sep=""),res)
    res<-ifelse(res==paste("0,",paste(rep(0,digits),collapse=""),sep=""),paste("<0,",paste(rep(0,digits-1),collapse=""),"1",sep=""),res)    
    res<-ifelse(x==0,format(round(x,digits),trim=TRUE,nsmall=digits,...),res)
    return(res)
  } else {
    return(ifelse(x<10,format(round(x,2),trim=TRUE,nsmall=2,...),
            ifelse(x<100,format(round(x,1),trim=TRUE,nsmall=1,...),format(round(x,0),trim=TRUE,nsmall=0,...))))
  }
  
}
