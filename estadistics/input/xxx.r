part1<-'/usr/local/bin/sendEmail -f eurhobop@imim.es -t '
part2<-' -u EURHOBOP recruitment status -s mail3.imim.es -a "/DATA/Estudis/eurhobop/'
part3<-'" -o message-content-type=html message-file="/DATA/Estudis/eurhobop/message.html"'

for (i in 1:nrow(partners)){
  system(paste(part1, partners$partner[i],part2 , partners$file[i],part3, sep=''))
   }
   