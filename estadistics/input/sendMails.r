rm(list=ls())
setwd("/home/jvila/Dropbox/rutines/estadistics/input")


## reading users
library(gdata)
dat<-read.xls("../users/estadistics.xls")
mails<-c(c("jvila@imim.es", "xxxjvila@gmail.com"), as.character(dat$email))
mails<-unique(mails)

## making e-mail
subject<-"Sessions d'estadística"
      #attached<-' -a "../docs/xxx.doc'
message<-'" -o message-content-type=html message-file="../html/message.html"'

part1<-'/usr/local/bin/sendEmail -f jvila@imim.es -t '
part3<-' -s mail3.imim.es '

for (i in 1:nrow(partners)){
  system(
    paste(part1, mails[i]," -u ", subject, part3, message, sep='')
    )
}


