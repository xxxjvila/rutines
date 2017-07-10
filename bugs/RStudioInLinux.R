rm(list=ls())
.libPaths(c(.libPaths(), lib= "/home/jvila/R/i486-pc-linux-gnu-library/3.1/lib"))
setwd("/home/jvila/Dropbox/rutines/bugs")
RutinesLocals<-"/home/jvila/Dropbox/rutines"
#RutinesLocals<-"C:/programs/Dropbox/rutines" 
#setwd("C:/programs/Dropbox/rutines/xxx")

library(compareGroups)

varis2<-c("me_b1", "me_b2", "me_b3", "me_b4", "me_b5", "me_b6", "me_b7", 
          "me_b8", "me_b9")
varis3<-c("me_c1", "me_c2", "me_c3", "me_c4", "me_c5", "me_c6")
varis<-c( varis2, varis3)

load("xdat.rda")
xdat <- xdat[1:235,] ## error
#xdat <- xdat[1:236,] ## NO erro

cap<-c("Durante su trabajo", "Ha padecido")

x1 <- createTable(compareGroups(~ . , Q1=0, Q3=1,
                      data= xdat, simplify=FALSE),
            show.p.overall=FALSE, show.all=TRUE, hide.no='No', 
            show.n =FALSE)

rbind(x1[varis2], x1[varis3], caption=cap)

xxx <- rbind(x1[varis2], x1[varis3], caption=cap)
