library(XLConnect)

#xxx <- "c:/programs/Dropbox/rutines/VanillaBatch/dat/BL-146.xlsx"
xxx<-commandArgs(trailingOnly = T)

filename<-sub(".xlsx", "", basename(xxx))
chn<-loadWorkbook(xxx)
dat<-readWorksheet(chn,sheet="Hoja1", startRow=1)
save(dat, file=paste(dirname(xxx), "/dat.rda", sep=""))

