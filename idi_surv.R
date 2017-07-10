idi<-function(dat,rr1,rr2,outcome){ 
  dadestemp<-dat
  r1<-rr1
  r2<-rr2
  casco<-dadestemp[, outcome]
  cl<-match.call()
  dd<-by(r2-r1,casco,mean,na.rm=TRUE)
  ans<-list()
  ans$res<-100*(dd[2]-dd[1])
  ans$call<-cl
  return(ans)
}


# To calculate CI IDI
######################################################################
ciidi<-function(dat,rr1,rr2,outcome, B=1000, alpha=0.05, seed, digits=2){
  set.seed(seed)
  dadestemp<-dat
  z<-qnorm(1-alpha/2)
  res<-idi(dat,rr1,rr2,outcome)[[1]]
  se<-sd(replicate(B,idi(dadestemp[sample(1:nrow(dadestemp),nrow(dadestemp),replace=TRUE), ],rr1,rr2,outcome)[[1]]))
  ci <- res+c(-1,1)*z*se
  ci<-format2(c(res,ci), digits)
  pvalidi<-(1-pnorm(abs(res/se)))*2
  pvalidi<-ifelse(pvalidi < 0.001, "<0.001", round(pvalidi, 3))
  return(paste(ci[1]," [",ci[2],"; ",ci[3],"]",", p-value = ", pvalidi, sep=""))
}