#.libPaths(c(.libPaths(), lib= "/home/jvila/R/i486-pc-linux-gnu-library/3.1/lib"))
rm(list=ls())

library(XLConnect)

#setwd("/home/jvila/Dropbox/rutines/VanillaBatch")
setwd("c:/programs/Dropbox/rutines/VanillaBatch")

## method 1
system.time({
xlcMemoryReport()  
system("R CMD BATCH b.R")
xlcMemoryReport()  
load("./dat/ODiaz1.rda")
})

## method2
system.time({
xlcMemoryReport()  
source("b.R")
xlcMemoryReport()  
})


## method3
system.time({
  xfiles<-c("BL-146.xlsx", "BL-168.xlsx")
  xwd<-"c:/programs/Dropbox/rutines/VanillaBatch/"
  xlcMemoryReport()  
  for(i in 1:length(xfiles)){
  xarg<-paste(xwd, "dat/", xfiles[i], sep="")  
  system(paste("C:/programs/R/R-3.0.1/bin/R --vanilla --slave --args ", xarg, " < ", xwd, "c.R",  sep=""))
  load(paste(dirname(xarg), "/dat.rda", sep=""))
  filename<-sub(".xlsx", "", xfiles[i])
  assign(sub("-", "_", filename), dat)
  save(dat, file = paste(xwd, '/dat/', filename, ".rda", sep=""))  
  }
  xlcMemoryReport()  
}
)
