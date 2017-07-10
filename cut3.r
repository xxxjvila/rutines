### FA QUANTILS COM HO FA EL SPSS ###

cut3<-function(x,g){
  # x: variable numerica de la que es volen fer els quantils.
  # g: nombre de grups.
  x<-na.omit(x)
  if (g>length(unique(x))) stop("Hi ha mes grups que dades diferents")
  perc<-rank(x,ties.method="average")/length(x)
  cum.perc<-cumsum(table(perc))
  breaks.perc<-sapply((1:g)/g,function(x) max(names(cum.perc)[which(names(cum.perc)<=x)]))
  cut(perc,c(0,breaks.perc))
}
