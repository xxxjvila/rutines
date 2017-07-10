library(XLConnect)
chn<-loadWorkbook("c:/programs/Dropbox/rutines/VanillaBatch/dat/BL-168.xlsx")
ODiaz1<-readWorksheet(chn,sheet="Hoja1", startRow=1)
save(ODiaz1, file="c:/programs/Dropbox/rutines/VanillaBatch/dat/ODiaz1.rda")
