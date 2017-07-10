#función delta AUC
deltaAUC.surv<-function(dat,ref,new,times,outcome){
  temps<-dat[,times]
  cens<-dat[,outcome]
  res <- rcorrp.cens(ref,new, Surv(temps,as.integer(cens==1)), method=2)
  tst <- abs(res[1]/res[2])
  pvalue <- (1-pnorm(tst))*2
  ifelse(pvalue<0.001,"<0.001",round(pvalue,3))}