convert.dates<-function(day,month,year){
  ans  <-  apply(cbind(day,month,year),1,paste,collapse="-")
  ans  <-  chron(ans,format="d-m-Y",out.format="d-mon-Y")
  return(ans)
}