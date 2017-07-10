rm(list=ls()) ## borra tot el que pugui haver-hi en la memoria  del R
dades<-as.data.frame(cbind(c(1,1,2,3,4,5), c(11,11,12,13,14,15))) ## crea una base de dades que es diu "dades"
library(xlsx)  ## carrega la llibreria 
write.xlsx(dades, file= "/home/ars/ESTUDIS/L01_DEMCOM/Analisi/dat/dades.xlsx", sheetName="Sheet1")  ## exporta a XLSX
 