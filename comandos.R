
## per treballar amb factors

xxx$sex <- as.factor(with(xxx, ifelse(sexe_b==1, "Male", "Female")))
xxx$sex <- relevel(xxx$sex, ref= "Male")
intervals(lm(lnimta_CCA ~ C(as.factor(sexe_b),base= 1), data = xxx))
intervals(lm(lnimta_CCA ~ sex, data = xxx))


## per llegir excel
dat <- readWorksheetFromFile( "./dat/Basedades2filtradasensedades4.xlsx", sheet = "Full 1", header = T,dateTimeFormat = "%d-%m-%Y", startRow=1, endRow = 386)

redcap
ronald multistage

## per installar RODBC
http://superuser.com/questions/283272/problem-with-rodbc-installation-in-ubuntu
rm(list=ls())
#RutinesLocals<-"C:/Users/jvila/Dropbox//rutines"
RutinesLocals<-"/home/jvila/Dropbox/rutines"
RutinesLocals <- "/Users/jvila/Dropbox/rutines"
install.packages("readr")
date_names_langs()
parse_date("1 enero 2015", "%d %B %Y", locale = locale("es"))
install.packages("haven")

source(file.path(RutinesLocals,"table2.r"))
source(file.path(RutinesLocals,"subset2.r"))
source(file.path(RutinesLocals,"carrega.llibreria.r"))
source(file.path(RutinesLocals,"calculadora.risc.r"))
source(file.path(RutinesLocals,"merge2.r"))
source(file.path(RutinesLocals,"intervals.r"))
source(file.path(RutinesLocals,"prepare.r"))
source(file.path(RutinesLocals,"export.SPSS.r"))

source(file.path(RutinesLocals,"arregla.formats.r"))

source(file.path(RutinesLocals,"import.ACCESS2.r"))
source(file.path(RutinesLocals,"merge2.r"))
source(file.path(RutinesLocals,"add.cases.r"))
source(file.path(RutinesLocals,"format2.r"))
source(file.path(RutinesLocals,"order2.r"))

source(file.path(RutinesLocals,"print2.r"))
source(file.path(RutinesLocals,"read.spss4.r"))
source(file.path(RutinesLocals,"spss_varlist.r"))


####packages 
install.packages("shiny")
install.packages("compareGroups")
install.packages("gam")
install.packages("png")
install.packages("epitools")
install.packages("pROC")
install.packages("psych")
install.packages("plotrix")
install.packages("knitr")
install.packages("chron")
## pgirmess
## primer he hagut d'instalar "gdal"
## gdal-config  em mostra que no existeix
## sudo apt-get install libgdal-dev
## sudo apt-get install libgdal1-dev libproj-dev
## sudo apt-get update
install.packages("rgdal")
install.packages("pgirmess")
install.packages("stringr")
install.packages("MASS")
install.packages("nnet")
install.packages("car")
install.packages("RODBC")
install.packages("survival")
install.packages("lattice")
install.packages("cluster")
install.packages("Hmisc")
install.packages("xtable")
install.packages("gdata")
install.packages("oce")
install.packages("tcltk2")
##install.packages("odfWeave")
install.packages("Rcmdr")
install.packages("extrafont")
###############################################################################
##############     rJava  20/10/2015   ########################################
###############################################################################

## veure: http://tecadmin.net/install-oracle-java-8-jdk-8-ubuntu-via-ppa/
## per veure on es el JAVA
whereis java

## s'ha d'executar: 
sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update
sudo apt-get install oracle-java8-installer

## comprovar la versio instalada
java -version 

## si el resultat no ?s 1.8 
sudo update-alternatives --config java # i seleccionar 1.8

## un cop haguem comprovat que es la 1.8
sudo apt-get install oracle-java8-set-default

## par tal de que el R l'incorpori
R CMD javareconf
###############################################################################
###############################################################################
###############################################################################
install.packages("rJava")
install.packages("xlsx")


#deb 
#http://cran.rstudio.com/bin/linux/ubuntu 
#lucid/

  
R.Version()

rm(list=ls(mgcv))
http://cran.rstudio.com/

library(frailtypack) # la llibreria de Juan Ramon de Supervivencia

## png
install.packages("png")
library(png)

## lme4
install.packages("epitools")


## pROC
install.packages("pROC")


## psych
install.packages("psych")
library(psych)

## plotrix
install.packages("plotrix")
library(plotrix)

install.packages("knitr")
library(knitr)

##chron
install.packages("chron")

## pgirmess
## primer he hagut d'instalar "gdal"
## gdal-config  em mostra que no existeix
## sudo apt-get install libgdal-dev
## sudo apt-get install libgdal1-dev libproj-dev
## sudo apt-get update
install.packages("rgdal")
install.packages("pgirmess")


install.packages("rgdal")
install.packages("stringr")
install.packages('stringr', repos='http://cran.us.r-project.org')

## per instal.lar "car" a linux
install.packages("MASS")
install.packages("nnet")
install.packages("car")
library(car)

## per instal.lar "RODBC"
## sudo aptitude install unixodbc-dev
install.packages("RODBC")
library(RODBC)

install.packages("survival")
library(survival)

install.packages("gam")
library(gam)

## per instal.lar "Hmisc"
install.packages("lattice")
install.packages("cluster")
install.packages("Hmisc")
library(Hmisc)

install.packages("xtable", dependencies=TRUE)
library(xtable)


install.packages("gdata", dependencies=TRUE)
library(gdata)

install.packages("oce", dependencies=TRUE)
library(oce)

install.packages("tcltk2", dependencies=TRUE)
library(tcltk2)


install.packages("odfWeave", dependencies=TRUE)
library(odfWeave)

install.packages("compareGroups")
library(compareGroups)

install.packages("Rcmdr", dependencies=TRUE)
library(Rcmdr)

install.packages("extrafont")
library(extrafont)
font_import()
fonts()

## rjava / xlsx / XLConnect
## des del promt:
sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update
sudo apt-get install oracle-java7-installer
sudo apt-get update
sudo R CMD javareconf
## des de R
install.packages("rJava", dependencies=TRUE)
install.packages("XLConnect", dependencies=TRUE)
install.packages("XLConnectJars", dependencies=TRUE)


################################################################################
####################   r Java     ##############################################
################################################################################
## veure si es la versio de 32 o 64 bits amb 
## sessionInfo()
## baixar-se la versi? de 64-bits de:
## http://java.com/en/download/manual.jsp
## ho he instal.lat a C:/Programs/Java64/
## he posat aquesta adre?a al path d'inici de windows
library(rJava)
################################################################################
################################################################################
################################################################################
Sys.setenv(JAVA_HOME='C:/Programs/Java64') # for 64-bit version
Sys.setenv(JAVA_HOME='/usr/lib/jvm/java-7-oracle/jre')


## .libPaths()
## .libPaths(c("/home/ars/R/x86_64-pc-linux-gnu-library/2.15","/usr/local/lib/R/site-library","/usr/lib/R/site-library","/usr/lib/R/library"))
## veure:
## http://www.r-statistics.com/2012/08/how-to-load-the-rjava-package-after-the-error-java_home-cannot-be-determined-from-the-registry/
## Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre7') # for 32-bit version
Sys.setenv(JAVA_HOME=': /usr/lib/jvm/java-7-openjdk-i386/jre')

Sys.setenv(JAVA_HOME='C:/Programs/Java/bin')
Sys.getenv("JAVA_HOME")
Sys.setenv(JAVA_HOME='C:/ProgramData/Oracle/Java')
if(Sys.getenv("JAVA_HOME")!="") Sys.setenv(JAVA_HOME="")
install.packages("rJava")
library(rJava)

install.packages("xlsx", dependencies=TRUE)
library(xlsx)


## exemple d'escriure un xlsx
dades<-as.data.frame(cbind(c(1,1,2,3,4,5), c(11,11,12,13,14,15)))
write.xlsx(dades, file= "xxx.xlsx", sheetName="Sheet1")  ## exporta a XLSX


## llegir fitxer de EXCEL
xfile<-"U:/ULEC/Exemples_estadistica/register/dat/tasques.xls"
channel<- odbcConnectExcel(xfile)
sqlTables(channel)
dat<-sqlFetch(channel, sqtable="Hoja1$")
close(channel)

## guardar fitxer de EXCEL
xxx<-as.data.frame(cbind(c(1,2,3,4,5,6), c(11,12,13,14,15,16)))
setwd("/home/jvila/xxx")
channel<- odbcConnectExcel("xxx.xls", readOnly=FALSE)
sqlSave(channel, catok, tablename="Participants",append = FALSE,safer= FALSE,rownames=FALSE,colnames=FALSE)
close(channel)

## library(sos)
findFn("open office")

save(clin, file ="xxx.RData")

## per triar el fitxer on es vol savel
save(exemple, file= file.choose())

# quan dos numeros no son iguals pero afecta a un decimal a prendre pel cul
rp17<-ifelse(isTRUE(all.equal(sup17a, (sup17ok*100))), 1, 
      ifelse(isTRUE(all.equal(sup17b, (sup17ok*100))), 2,
      ifelse(isTRUE(all.equal(sup17c, (sup17ok*100))), 3, 999)))
rp17

format(sup17c,digits=16,nsmall=16)

# una altre opci? es definir la funci? "==" perque faci aix?:  

"==" <- function(x,y) isTRUE(all.equal(x, y)) 



# per desactivar un grafic
graphics.off()


# per borrar una lliberia
search() # miro a quina posicio es, p.e. la 2
detach(2)

a<--2.9841282
b<-sqrt(0.4656142)
c<-qnorm(0.1,a,b)
d<-round(1/(1+exp(-c)),4)


#posar ordre
## per mes d'una variable:
problems <- problems[order(problems$centreid, problems$paccentreid, -problems$type), ]
xxx <- subdat[order(subdat[,"centro"],subdat[,"paci"]),]
xxx$ordre  <- seq(1, nrow(subdat))
subdat <- merge2(subdat, xxx[, c("idrepe", "ordre")], by.id=c("idrepe"),all.x=TRUE, sort= FALSE)
subdat <- order2(subdat, c("ordre"))
subdat <- remove.vars(subdat, "ordre")


head(ictus[order(ictus[,"id"],order(ictus$ancestor, decreasing = TRUE)),])

tots2<-tots2[order(tots2[,"parella"],-tots2[,"cascon"]),]
xxx<-xxx[order(xxx[,"id"]),]

x<-c( 1, 2, 3, 4, 5, 6, 7, 1, 3, 4, 5,8)
y<-c(22,23,23,24,25,26,27,28,24,22,20,21)
z<-cbind(x,y)
t(z[order(z[,1],-z[,2]),])

pred<-order2(pred, c("id"))
xxx[order(c(xxx$font,xxx$lp)),] 



vari<-scan(what="character", sep="\n")
idpaci
estudi
idepisodi
nom
ape1
ape2
f_ing
iamseg
diamseg
iam2a
toiam


xxx<-tot[!is.na(tot$iam2a) & tot$iam2a==1, vari]
xxx[order(xxx[,"estudi"],-xxx[,"idepisodi"]),]



packageDescription("gnm")
example(rpanel)



library()
search()
ls(4)
help(solve)
?solve
help("[[")
help.start()
example("hclust")
source("c:\\jvila\\r\\comandos.R")
sink("resultado.txt")
sink()


# punto de corte
qt(.975, df = 24)

# calcular "p" para un valors de "t", bilateral
(1-pt(2.063899,df=24))*2

## generar una random variable and testing normality
library(MASS)
x<-rt(300,df=5)
fitdistr(x,"t")
qqnorm(x); qqline(x)
qqplot(qt(ppoints(length(x)),df=5.55),x)
qqline(x)


# exemple de plots amb distribucio normal
x<-seq(-6,6,by=0.1)
plot(x,dnorm(x),type="l",xlim=c(-6,6),ylim=c(0,0.9))
lines(x,dnorm(x,mean=0,sd=2),col="red")

x<-seq(0,40,by=0.01)
curve(dgamma(x,shape=2,scale=3),from=0,to=40)
abline(v=2*3,lty=2)

x<-0:20
plot(x,dpois(x,lambda=4),type="h")

plot(x,ppois(x,lambda=4),type="s")

# calular mitjanes per linia
muestras<-matrix(rnorm(1000),nrow=100,byrow=T)
medias<-apply(muestras,1,mean)

# calcular el nombre de missings per linia
answer$na<-apply(t(apply(answer[,8:57],1,is.na)), 1, sum)

answer$na<-apply(is.na(answer[,8:57]),1,sum))

## crea una variable que indica si hi ha missing o no
lpa$keep<-apply(!is.na(lpa),1,all)

# calcula mitjana i t-Student
with(pred,by(edad,sexo,function(x) c(mean(x,na.rm=TRUE),sd(x,na.rm=TRUE))))
t.test(edad ~ sexo, data = subset2(pred, "sexo<999 & edad <999"),var.equal = TRUE)

# generar numeros de una binomial
x<-rbinom(20,size=1,prob= 0.2)


## seleccionar
x<-sample(c("A","B","C"),200,replace=T,prob=c(0.5,0.4,0.1))
wom<-wom[sample(1:nrow(wom),16),]

#exemple de buscar una variable
agrep("diarev",names(mcr),value = TRUE)


keep.var<-c("aparece","atencion","fllega","primeringr","fcoro1","ptcaprim","ptcaresca", "ptcaelec","ptcafarma","ptcasincla","frevas")
keep.var[!keep.var%in%names(mcr)]



# per fer moltes taules
xxx<-names(pred)
for(i in 12:length(pred))print(cbind(table2(pred[,xxx[i]],pred$sexo)))

cbind(table2(pred$hta,pred$sexo))

sink(file="xxx.doc")
for(i in 3:length(xxx))print(cbind(table2(pred[,xxx[i]])))
sink()
file.show("xxx.doc")
shell.exec("xxx.doc")


# per borrar variables 
dades<-remove.vars(dades,"resucoro")


## localitzar registres i variables
which(names(pred)=="hta")

pred$numcase<-1:nrow(pred)

rownames(pred)   



# per fer un excel dels resultats
write.table(datos, file = "c:/jvila/r/r.xls",append=FALSE,sep="\t",col.names=TRUE,row.names=FALSE)
write.table(jsanchez, file = paste(treball,"jsanchez.xls", sep=""),append=FALSE,sep="\t",col.names=TRUE,row.names=FALSE, na="")
shell.exec("c:/jvila/r/r.xls")


#exemple de recode, rename, attrib 
x1$colmed<-car::recode(x1$colmed,"2=0;1=1;else=NA")

casos<-rename.vars(casos, from="hipolip6", to="colmed")

attr(ic.coef,"vari.label")<-c("Identificador", "x2", "ss")
attr(x3$colmed,"vari.label")<-"Hipolipemiantes (en casos a 6 meses)"
attr(x3$colmed,"value.labels")<-c("No"=0, "Si" =1)


#seleccionar pacients i variables
vari<-scan(what="character", sep="\n")
id
nodo
edad

xxx<-subset2(pred, "nodo ==1 & edad >70")[,vari]

fix2(clin[is.na(clin$edad), c("fechini","fechnac","xxx1","edad")])


# salvar atributs
atri.ahtam<-attributes(clin$ahtam)
attributes(clin$ahtam)<-atri.ahtam

                  
# subset
clin<-subset2(clin,"clin$lugartto==1 | clin$lugartto==6")
vari<-c("idepisodi","estudi", "nombrepa", "hsintmon", "msintmon", "infosinmon","fechini", "sint")
subset2(dat, "infosinmon ==1 & hsintmon >24")[,vari] 


#merge
clin<-merge2(clin,fili,by.id=c("estudi","idepisodi"),all.x=TRUE, sort= FALSE)


# dates
library(chron)
seg6m$xxx1<-paste(as.character(seg6m$adhospdtdy),"-", as.character(seg6m$adhospdtmo),"-", as.character(seg6m$adhospdtyr),sep="")
seg6m$recruitdat<-chron(seg6m$xxx1,format=c(dates="d-m-y"),out.format=c(dates="day-mon-year"))
tot$f_ing<-chron(tot$f_ing,format=c(dates="d-m-y"),out.format=c(dates="day-mon-year"))
min(tot[tot$origen==3,]$f_ing)

tot$f_ing<-chron(tot$f_ing,out.format=c(date="d-mon-Y"))

xx<-chron(paste("31","-","12","-","2002",sep=""),format=c(dates="d-m-y"),out.format=c(dates="day-mon-year"))


xxx<-c("06/01/2012 20:36:25" "06/01/2012 20:36:25" "12/01/2012 01:38:33" "10/01/2012 11:23:16" "08/01/2012 22:14:22" "08/01/2012 22:14:22")
dts<-substr(xxx, 1, 10)
tms<-substr(xxx, 12, 20)
x1<-chron(dates=dts,format=c("d/m/Y"),out.format=c("d-mon-y"))
x2<-chron(times=tms,format=c("h:m:s"),out.format=c("h:m:s"))
answer$moment<-chron(dates = x1, times = x2,format=c(dates="d/m/Y", times = "h:m:s"),out.format=c(dates="day-mon-year", times = "h:m:s"))





ini<-chron(c("4/6/2004","8/12/1995","1/1/2004"),format=c("d/m/Y"),out.format=c("d-mon-y"))
fi<-chron(c("1/11/2003","31/12/1997","31/12/2007"),format=c("d/m/Y"),out.format=c("d-mon-y"))
df<-data.frame(ini,fi)
df$res<-rep(NA,nrow(df))
for (i in 1:nrow(df)){
 df$res[i]<-trunc(runif(1,df$ini[i],df$fi[i]))
}
df$res<-chron(df$res,out.format=c("d-mon-y"))
df





#funcio
f1=function (a,b) {
v=a*2
w=b*2
return (v,w)
}
x<-f1(3,5)



f2=function (a,b) {
a*b
}
xxx<-f2(2,9)

## escriure una taula
write.table(datos, file = "c:/jvila/r/r.xls",append=FALSE,sep="\t",col.names=TRUE,row.names=FALSE)

shell.exec("c:/jvila/r/r.xls")


########################################################################
##################  importo SPSS i exporto acces ########
########################################################################
vari<-tolower(scan(what="character"))
rescate
n_h
HOSP1
NUM_PACIENTE
caso
ape1
ape2
nom
edad
sex
RTRSIMO
admi
ahtai
acoli

fitxer<-"U:\\Estudis\\Epidemiologia\\REGICOR\\POBLACIONAL\\dades\\regi78_actual\\original\\bases de dades procedencia fusio\\78-95 procedeix de investigats.sav"
hola<-read.spss4(fitxer,keep.var=vari)

acces<-paste(treball, "problemes.mdb", sep="")
export.ACCESS(taula=gedaps, file.mdb=acces, table.name="gedaps", table.dict = "dicgedaps")
shell.exec(acces)

#### importar acces
import.ACCESS2(
  file.mbd="U:\\Estudis\\Clinic\\BASICMAR\\dades\\DEA Jordi\\JJimenez.mdb",
  nom.taula=c("basic","m3","gen"),
  nom.variables=list(c("ALL"), 
                     c("ALL"), 
                     c("partic", "K406", "K1444", "K375", "K246","K201")),
  nom.dicc="Dic",
  file.spss="",
  var.dicc=c("nombre","etiqueta_variable","etiqueta_valor","tabla2"),
  noms.taules=c("basic","m3","gen"),
  fix.formats=TRUE)




# per buscar repetits

(repes <- with(stud,table(dni)))[repes>1]


repes<-with(check1,table(id))
repes<-as.double(names(repes)[repes>1])
check1$exclu<-with(check1, ifelse(check1$id%in%repes, 74, exclu))
    

sum(with(cascon,table(idfortiam))>1)

t<-with(fortiam,table(colest))
sum(t>1)
t[t>1]
valors.repes<-as.double(names(t)[t>1])


fortiam$num_paci[fortiam$colest%in%valors.repes]
xxx<-subset(fortiam,colest%in%valors.repes)[,c("num_paci","colest")]
fix2(xxx[order(xxx$colest),])


#  correlacions
vari<-scan(what="character")
nkg2a_cd3_
nkg2c_cd3_
x2a_2c_cd3_
nkg2c_en_cd3__cd56_
nkg2a_en_cd3__cd56_
nkg2c_en_cd56__cd3_
nkg2a_en_cd56__cd3_
nkg2c_en_cd3__cd56__1
nkg2a_en_cd3__cd56__1
x2a_2c_cd3__cd56_
x2a_2c_cd3__cd56__1
ilt2_cd3__cd56_
ilt2_cd3__cd56__1
ilt2_cd3__cd56__2
ilt2_cd3_
nkg2c_en_nk
nkg2a_en_nk
ilt2_en_nk
x2a_2c_en_nk


xxx<-dades[,vari]

res<-NULL
for (i in 2:ncol(xxx)){
  for (j in 1:(i-1)){
    x<-xxx[,i]
    y<-xxx[,j]
    ct<-cor.test(x,y,method = "spearm")
    r<-ct$estimate
    pvalor<-ct$p.value
    n<-sum(!is.na(x) & !is.na(y))
    label.x<-attr(x,"vari.label")
    label.y<-attr(y,"vari.label")
    label<-paste(label.x,label.y,sep=" vs. ")
    res<-rbind(res,c(label, r,pvalor,n))
  }
}

colnames(res)<-c("Variables2","rho","pvalor","n")


write.table(res, 
   file = "U:\\Estudis\\Externs\\NKG2C M Lopez Botet\\Dades\\cor.xls",append=FALSE,sep="\t",col.names=TRUE,row.names=FALSE)
    
# per fer LR univariades
vari<-scan(what="character")
edad
C(as.factor(sexo),base=1)
C(as.factor(period),base=1)

write.table("Univariat", file = paste(treball,"LRuni.xls",sep=""),col.names=FALSE,row.names=FALSE)
write.table(rbind(c("Variable", "OR", "95%CI inf", "95%CI sup", "p-value")), sep="\t",file = paste(treball,"LRuni.xls",sep=""),append= TRUE, col.names=FALSE,row.names=FALSE)

for (i in 1:length(vari)){
formul<-paste("def"," ~ ", noquote(vari[i]), sep="")
mod<-glm(
  formula=formul,
  family="binomial",
  data=dat,
  na.action=na.exclude
  )
write.table(intervals(mod)[2,,drop=FALSE], file = paste(treball,"LRuni.xls",sep=""),append=TRUE,sep="\t",col.names=FALSE,row.names=TRUE)
}

shell.exec(paste(treball,"LRuni.xls",sep=""))



## per fer moltes tab
for (i in 2:length(vari)){
eval(parse(text=paste("with(clin,table2(",noquote(vari[i]),"))",sep="")))
}

for (i in 2:length(vari)){
  cat("\n_______",vari[i],"_________\n")
  table2(clin[,vari[i]])
  cat("\n\n\n")
}

for (i in 2:length(vari)){
  clin[,vari[i]]<-car::recode(clin[,vari[i]],"NA=999")
}

# per imprimir molts resultats
sink(file = "c:\\jvila\\xxx.txt")
for (i in 1:length(vari)){
  cat("\n_______",vari[i],"_________\n")
  print(table(clin[,vari[i]],clin$a?oini))
  cat("\n\n\n")
}
sink()
shell.exec("c:\\jvila\\xxx.doc")


# per comprovar linealitat
####################################
# tria explicativa, outcome i les dades
explicativa<-"imc"
outcome<-"itb_cutrec"
nom.dades<-"hermesok"


# aqui fa el model
temp<-eval(parse(text=paste("subset(",nom.dades,",!is.na(",outcome,") & !is.na(",explicativa,"))",sep="")))
formul<-paste(noquote(outcome), "~ s(", noquote(explicativa),")",sep="")

mod.lin<-gam(
  formula=as.formula(noquote(formul)),
  family="binomial",
  data=temp,
  #subset =sexe==1,
  na.action=na.exclude
  )
# grafic
res.mod<-preplot.gam(mod.lin,type="terms",terms=paste("s(",noquote(explicativa),")",sep=""),se.fit=TRUE)[[1]]
ci<-cbind(res.mod$y,res.mod$y-qnorm(1-0.05/2)*res.mod$se.y,res.mod$y+qnorm(1-0.05/2)*res.mod$se.y)
orden<-order(res.mod$x)
ci<-ci[orden,]
matplot(sort(res.mod$x),ci,type="l",lty=c(1,2,2),col="black",xlab=explicativa,ylab="logit smooth estimate")
title("gam logistica")
rug(jitter(res.mod$x))
#####################################




### sumar per columnes
x1<-colSums(with(fusio,table(smoker,font)))
x2<-with(fusio,apply(table(smoker,font),2,sum))



# taules bivariades
var.taula<-"VARIABLE\tKEEP\tDIGITS\tMETHOD\tELIM\tTIPUS\tLOGPTREND
hours\tNULL\t1\t2\tNULL\tNULL\tFALSE"

write(var.taula,file="C:\\xxx.doc")
file.show("C:\\xxx.doc")



taules.bivariades(file.input = NULL, var.taula = var.taula, nom.col = "group", 
    dades = oren, nom.arxiu = "C:\\jvila\\oren\\resu", dec.car = ",", plot.norm = TRUE, 
    lim.p.value = 0.05) 


##genera noms del tipus xp01, xp02, etc.
grep("^xp[0-9]+$",names(notes),value=TRUE)

toupper(letters[1:8])

## per omplir de 0
xxx<-tr05lab$id
xxx<-c(99999, xxx)
xxx<-format(xxx)
xxx<-gsub(" ", "0", xxx)
xxx<-xxx[-1]
tr05lab$xxx<-xxx


## pastes varis

xxx<-rbind(paste(rep("p", 8), as.character(seq(1,8, 1)), sep=""))
lettercode<-cbind(paste(rep(toupper(letters[1:8]), 12), rep(as.character(seq(1,12, 1)),each= 8), sep=""))
numbercode<-cbind(seq(1,length(lettercode), 1))
convert<-cbind(lettercode, numbercode)

# genera cadenes del tipu an01, an02, etc.

cbind(paste(rep("an", 50), num.pract<-gsub(" ","0",format(1:50)), sep=""))
c(paste(rep("r", 20), gsub(" ","0",format(1:20)), sep=""))

result<-54
paste("La respuesta es",result,sep=": ")


x<-c(1,3,4)

paste(x,collapse="/")

paste(x,sep="/")


x<-c(1,2,3)
y<-c(4,5,6)
z<-c(7,8,9)
paste(x,y,z,sep="+")


paste(paste("Pregunta",1:5,sep=""),collapse="\t")

toupper(letters[1:8])

paste(paste("Pregunta",letters[1:5],sep=" "),collapse="\n")

paste(paste("Pregunta",LETTERS[1:5],sep=" "),collapse="\n")
write(rbind(paste(paste("Pregunta",1:npreg,sep=""),collapse="\t")),file="xxx")
file.show("xxx")

## legir un fitxer EXCEL
regiair<-read.xls( paste(treball,"alea.xls", sep =""),colNames = FALSE,sheet = 1)

# replicates
numok$xxx<-rep(1:19, each= 40)
rep(c("a","b","c"),c(10,20,5))


save(dat,file = file.path(treball,"dat.Rdata"))


# per llegir un excel
jsanchez<-xlsReadWrite::read.xls( paste(treball, "Muestras empleadas para pools.xls", sep=""),
          colNames = TRUE,
          sheet = 1,
          type = "data.frame",
          from = 1,
          rowNames = NA, colClasses = NA, checkNames = TRUE,
          dateTimeAs = "numeric",
          stringsAsFactors = default.stringsAsFactors())


# per salvar com etiquetes els valors d'una variable de cadena
xxx<-levels(flow$situ2)
flow$situ2<-as.integer(as.factor(flow$situ))
attr(flow$situ2,"value.labels")<-structure(1:length(xxx), names=xxx)



### per buscar alguna sintaxis (p.e. casos.RData) feta mab R

xxx<-list.files("/home/jvila/gdrivelars/d449/MU/MUAnalysis/MuscEsque/empresa", pattern= ".R$", recursive=TRUE, full.names = TRUE)

for (i in 1:length(xxx)){
  contingut<-scan(xxx[i],what="character",sep="\n")
  if (length(grep("loc<-",contingut))) print(xxx[i])
}


### per veure les caracter?stiques de les variables
lapply(jm, class)


### per exportar a SPSS 
export.SPSS (m4, file.save = NULL, var.keep = "ALL", run.spss = FALSE)

export.SPSS (par1a1, file.dict = NULL, file.save = "U:/Estudis/Clinic/FORTIAM - RESCATE II/FORTIAM/analisi/MG?mez/Article 2/par1a1.sav"
      , var.keep = "ALL", file.runsyntax = "C:/Archivos de programa/SPSS Evaluation/runsyntx.exe") 


## per que no sorti en format cient?fic
format((prec/100)^2,scientific = FALSE)


# Data per imputar
##############################################

          #data aleatoria entre inici i final de l'estudi
n<-nrow(segok)
segok$temp<-with(segok,chron(iam_ind + round(runif(nrow(segok),0,d_ult2-iam_ind),0),out.format="d-mon-Y"))

## calcular la data maxima
surv11$timemax<-with(surv11, ifelse(event>=1, apply(surv11[,c("datiam", "dataltraci", "datavc", "datdef")], 1, min), apply(surv11[,c("datiam", "dataltraci", "datavc", "datdef")], 1, max)))



# 4 dimensional plot

m<-matrix(unlist(with(countries,by(event,eventq,function(x) c(min(x,na.rm=TRUE),max(x,na.rm=TRUE))))),
    ncol=2,byrow=TRUE)
m<-format(round(m,3))
m<-apply(m,1,function(x) paste("[",x[1],";",x[2],"]",sep=""))

colors<-c("blue", "green", "yellow", "red")

plot(countries$gross,countries$cvdeath
    ,cex=sqrt(countries$n/100)
    ,col=colors[countries$eventq]
    ,xlab="Yearly gross national income per capita ($)"
    ,ylab="Age-standardized mortality rate for cardiovascular diseases",pch=19)

points(countries$gross,countries$cvdeath,cex=sqrt(countries$n/100))
legend("topright",legend=paste("Q",1:4,": ",m,sep=""),
        fill=colors,title="in-hospital mortality")

par(xpd=NA)
identify(countries$gross,countries$cvdeath,countries$name,cex=0.8,col="black",font=2)


# nova finestra gr?fica
win.graph()


## funcions i classess
> print.isaac<-function(x) cat("hola qu? tal",x,"\n")
> x<-3
> class(x)<-"isaac"
> x
hola qu? tal 3 
> print(x)
hola qu? tal 3 
> unclass(x)
[1] 3
> class(x)
[1] "isaac"
> class(unclass(x))
[1] "numeric"
> print.default
function (x, digits = NULL, quote = TRUE, na.print = NULL, print.gap = NULL, 
    right = FALSE, max = NULL, useSource = TRUE, ...) 
{
    noOpt <- missing(digits) && missing(quote) && missing(na.print) && 
        missing(print.gap) && missing(right) && missing(max) && 
        missing(useSource) && length(list(...)) == 0
    .Internal(print.default(x, digits, quote, na.print, print.gap, 
        right, max, useSource, noOpt))
}
<environment: namespace:base>
> methods(class="isaac")
[1] print.isaac
> methods(class="cox.zph")
[1] [.cox.zph*     plot.cox.zph*  print.cox.zph*

   Non-visible functions are asterisked
> methods(class="glm")
 [1] add1.glm*               anova.glm               Anova.glm*             
 [4] av.plot.glm*            ceres.plot.glm*         confidence.ellipse.glm*
 [7] confint.glm*            cooks.distance.glm*     cr.plot.glm*           
[10] deviance.glm            drop1.glm*              effects.glm*           
[13] extractAIC.glm*         family.glm*             formula.glm*           
[16] influence.glm*          intervals.glm           leverage.plot.glm*     
[19] linear.hypothesis.glm*  logLik.glm*             model.frame.glm        
[22] ncv.test.glm*           outlier.test.glm*       predict.glm            
[25] print.glm               qq.plot.glm*            residuals.glm          
[28] rstandard.glm           rstudent.glm            summary.glm            
[31] Var.glm*                Varcov.glm              vcov.glm*              
[34] weights.glm*           

   Non-visible functions are asterisked
> add1.glm
Error: objeto "add1.glm" no encontrado
> ?add1.glm
 
> getAnywhere(add1.glm) # i surt tota la funcio add1.glm


#### per treure espais en blanc
ibespss$poblaci_<-with(ibespss, sub(" +$","", poblaci_))
albaspss<-subset2(ibespss, "poblaci_=='ALBACETE'")

### per truere el punt al final de un carcater
alldat$tropo_peak<- with(alldat, sub("\\.+$", "", tropo_peak, fixed = FALSE ))

## per saber els valors que no es poden convertir a numeric
x<-c("2.1","2,2",NA)
x<-trim(x)
x<-ifelse(x=='',NA,x)
ww1<-which(is.na(x))
x2<-as.double(x)
ww2<-which(is.na(x2))
ww<-ww2[!ww2%in%ww1]
x[ww]


### per calcular or, rr, etc.
library(epicalc)
help(package="epicalc")
example(cs)

## la data del sistema

Sys.Date()


## attributs
cbind(lapply(euphoric3, function(x) attr(x,"vari.label")))
cbind(unlist(lapply(dexa, function(x) attr(x, "vari.label"))))

## per treure els espais en blanc
dades$xxx <- ifelse(sub(" +$", "", dades$comentario)=="tercera generaci?n",1,0)

## taules varies
install.packages("Epi")
install.packages("catspec")
install.packages("gmodels")
install.packages("epitools")

library("Epi")
library("catspec")
library("gmodels")
library("epitools")

example(stat.table)
example(ctab)
example(CrossTable)
example(riskratio)


### per treure els missing
macrowom<-macrowom[apply(t(apply(macrowom,1,is.na)), 1, sum) == 0, ]


### per dibuixar un grafic de barres
par(las=1, mar=c(5, 6, 4, 2), xpd=FALSE)
mehta<-48.2
lohta<-47.4
uphta<-49.1
hta<-c(42.8, 46.6, 48.3, 51.2, 50.2, 43.7, 51.2, 52.6, 43.1)
centers<-c("REGICOR", "HERMEX", "TALAVERA", "CDC", "RIVANA", "RECCyL", "CORSAIB", "DINO", "DRECA")
htac<-hta-mehta
color<-ifelse(hta<lohta, "green", ifelse(hta>uphta, "red", "blue"))
xxx<-barplot(htac,horiz=TRUE,axes=F,col=color, xlim= c(-6,5),
    main="Age-standardized Hypertension prevalence: MEN")
axis(1,pretty(range(htac)),(pretty(range(htac))+ mehta))
axis(2,xxx, centers)
abline(v=c(lohta, mehta, uphta)-mehta, lty=c(2,1,2))
par(xpd=NA)
legend(mean(par()$usr[1:2]),par()$usr[3]-diff(par()$usr[3:4])*0.1,c("Overall","95%CI"),xjust=0.5,lty=1:2,bty="n")


## per veure el que fa un package
help(package="survival")

OR<-c(1.13,3.75,4.32,5.54,5.01)
selogOR<-c(0.2,0.3,0.25,0.12,0.2)
meta.DSL(OR,selogOR)
meta.DSL(OR[-1],selogOR[-1])


### per buscar un tros de sintaxis en tots el tinn-R d'una carpeta
carpeta<-"U:/Estudis/Colaboracions/2009 DARIOS Prevalencia FRCV Espa?a siglo XXI/Analisis"
arxius<-list.files(carpeta, pattern=".r$", full.names=T, recursive=T)

for (i in 1:length(arxius) ){
xxx<-scan(file=arxius[i], what="character",  sep="\n")
print(grep("Comparaci?n de resultados",xxx))
}


## per calcular mitjanes  per fila
offv01$dbp<-with(offv01,apply(cbind(a56,a58),1, mean, na.rm=TRUE))

## Per fer taules amb totals
xxx<-as.matrix(with( fusio, table2(flow2, font, margin=0)))
cbind(xxx,apply(with(fusio, table (flow2, font)), 1, function(x) sum(x)))


## per definir l'amplada de la consola
options(width = 60)
seq(1, 100, 1)
options(width = 32)
seq(1, 100, 1)

## compare groups
library(foreign)
library(compareGroups)
setwd("C:/cursR/data")
datos<-read.spss("partoFin.sav",
    use.value.labels = FALSE,
    to.data.frame = TRUE)
datos$naci_ca<-factor(datos$naci_ca,labels= names(attr(datos$naci_ca,"value.labels")))
datos$sexo<-factor(datos$sexo,labels= names(attr(datos$sexo,"value.labels")))
res <- compareGroups(tx ~ edad + peso + sexo + naci_ca, data = datos,
       selec = c(peso = "datos$edad < 40"),
       method = c(peso=2))
restab <- createTable(res, show.n = TRUE,
                      hide = c(sexo =1),
                      digits = c(edad=3))
export2latex(restab, file = "C:/xxx/table1", dec = ",")
export2csv(restab, file = "C:/xxx/table1", sep = ";")

# un altres exemple
# primer fer un scan . . . .
dat<-fusio[, vari]
dat<-prepare(dat)

res <- compareGroups(font ~ .,  data = dat, subset  = fusio$st3c==0 | fusio$st3c==1)
restab <- createTable(res, show.n = TRUE, hide = c(sexo= 1,ant_dm= 1,ant_tab= 1,ant_col= 1,ant_hta= 1,ant_iam=1 ,ant_rev= 1,onda_q= 1,loc_ar= 1,ucc_exit= 1,mort28= 1,mort6= 1,hemodin= 1))
export2csv(restab, file = "C:/xxx/xxx", sep = ";")
shell.exec("c:/xxx/xxx.csv")


## update
res<-update(res,  font ~ . -hemodin, subset  = fusio$st3c==0)
restab <- createTable(res, show.n = TRUE, hide = c(sexo= 1,ant_dm= 1,ant_tab= 1,ant_col= 1,ant_hta= 1,ant_iam=1 ,ant_rev= 1,onda_q= 1,loc_ar= 1,ucc_exit= 1,mort28= 1,mort6= 1,hemodin= 1), show.p.trend=TRUE)
# restab  <-  update(restab,  show.all  =  FALSE)
export2csv(restab, file = "C:/xxx/xxx", sep = ";")
shell.exec("c:/xxx/xxx.csv")




## per saber les etiquetes de les variables
varnames<-NULL
for (i in 1:ncol(fusio) ) {
varnames<-rbind(varnames, trim(paste(paste(i, ") ", names(fusio[i]), sep=""), attributes(fusio[ , i])$vari.label, sep=": ")))
}

## per esborrar packages
remove.packages("compareGroups")

## per instal?lar un tar.gz
install.packages("C:/CursR/menorca/packages/tar.gz/compareGroups_0.1-5.tar.gz", repos=NULL, type="source")

install.packages("/xxx/compareGroups_2.0.3.tar.gz", repos=NULL, type="source")

install.packages("SNPassoc")
install.packages("XLConnect")
install.packages("shiny")
install.packages("HardyWeinberg")
install.packages("/home/jvila/Dropbox/CompareGroups/package/compareGroups_without_odfWeave/compareGroups_2.1.tar.gz", 
                 repos=NULL, type="source")

## ajuda sobre un package
help(package=oce)

## exemple de if else
alpha <- 0
if (alpha > 1) {x <- 88} else {x <- -88}
x

## per fer comparacions m?ltiples
p.adjust(c(0.004, 0.0003, 0.005), "BH")


## exemple de factors
gender<-rbinom(10,1,0.5)
gender<-c(gender,9)
table(gender)
gender<-factor(gender,levels=c(0,1),labels=c('home','dona'))
table(gender)


## per saber les dades que hi ha al R
data()

########### spss.get2 ############
source(file.path(RutinesLocals,"spss_varlist.r"))
source(file.path(RutinesLocals,"prepare.r"))
source(file.path(RutinesLocals,"arregla.formats.r"))
library(Hmisc)
xfile<-"./dat/cancer_incidente_npnm_enviado.sav"
dict<-spss_varlist(xfile)
xdates<-dict[grep("^DATE",dict[,2]),"longname"]
dat<-spss.get(xfile,allow="_",use.value.labels=FALSE,datevars=xdates)
dat[,xdates]<-arregla.formats(dat[,xdates])
for (i in 1:ncol(dat)) attr(dat[,i],"vari.label")<-label(dat[,i])
##################################


## per guardar els factors com etiquetes

x1$abo<-as.factor(x1$abo)
ll<-levels(x1$abo)
x1$abo<-as.integer(x1$abo)
attr(x1$abo,"value.labels")<-structure(1:length(ll),names=ll)
attr(x1$abo,"vari.label")<-"ABO"


## per substituir els valor d'edat < 40
sapply(age, function(x) if (x<40) runif(1,40,45) else x)


## per calcular el temps que triga a fer-se una cosa
 system.time({ qnorm(0.05/2)}) 
 
 
## per posar numero d'ordre
xalib$count<-NA
xalib$count[1]<-1
xnum<-1

for (i in 1:(nrow(xalib)-1)){
    x1<-xalib$id[i]
    xnum<-ifelse(xalib$id[i+1]==x1, xnum+1, 1)
    xalib$count[i+1]<-xnum
    }
    


# per buscar una funcio, especialment les que estan amagades (son les que tenen un asterix)
getAnywhere(mean)
getAnywhere(print.coxph.penal)

# per buscar en els packages intal?lats
help.search("ancova")

# per buscar a la p?gina web del CRAN
RSiteSearch("ancova") 

# utilitzant el paquet SOS
library(sos)
findFn("ancova")


## regular expressions
######################

## busca exactament "36." al comen??ament
x <- c("736.0", "36.", "366.1", "366.")
x[grep("^36\\.", x)]

# busca la primera vegada (^) que surt un numero [0-9] i el substitueix per xxx
sub("^[0-9]","xxx","0124hola") 
[1] "xxx124hola"

# busca la primera vegada que surt una sequencia de numeros [0-9]+ i aquesta sequencia la substitueix per xxx
sub("[0-9]+","xxx","0124hola123") 
[1] "xxxhola123"

# busca qualsevol (gsub) numero [0-9] i el substitueix per xxx
gsub("[0-9]","xxx","0124hola04")
[1] "xxxxxxxxxxxxholaxxxxxx"

# busca qualsevol (gsub) sequencia de numeros [0-9]+ i la substitueix per xxx
> gsub("[0-9]+","xxx","0124hola04")
[1] "xxxholaxxx"


# busca la primera (sub) sequencia de numeros [0-9]+ i la substitueix per xxx
sub("[0-9]+","xxx","aaaaa0124hola04")
[1] "aaaaaxxxhola04"

# busca la primera (sub) sequencia de numeros [0-9]+ que esta a comen??ament, pero no n'hi ha cap
sub("^[0-9]+","xxx","aaaaa0124hola04")
[1] "aaaaa0124hola04"

sub(" $","","apoefhawpehf ")
[1] "apoefhawpehf"

sub(" $","","apoefhawpehf  ")
[1] "apoefhawpehf "

sub("[ ]+$","","apoefhawpehf  ")
[1] "apoefhawpehf"

> sub("[ ]+","","apo  efhawpe hf")

[1] "apoefhawpe hf"

> sub("[ ]","","apo  efhawpe hf")

[1] "apo efhawpe hf"

> sub("[ ]","","apo  efhawpe hf")

[1] "apo efhawpe hf"

> sub("[  ]","","apo  efhawpe hf")

[1] "apo efhawpe hf"

> sub("[ ]2","","apo  efhawpe hf")

[1] "apo  efhawpe hf"

> sub("^[ ]+","","    wapoeufhapuwef")

[1] "wapoeufhapuwef"

> sub("^[ ]+","","    wapoeufhapu   wef")

[1] "wapoeufhapu   wef"

> gsub(" ","","    wapoeufhapu   wef   ")

[1] "wapoeufhapuwef"

gsub("^[0-9]+","","10987561023asdof?341525iwhapfohe")

[1] "asdof?341525iwhapfohe"

> sub("^[0-9]+","","10987561023asdof?341525iwhapfohe")

[1] "asdof?341525iwhapfohe"

> gsub("[0-9]+","","10987561023asdof?341525iwhapfohe")

[1] "asdof?iwhapfohe"

> gsub("[0-9]","","10987561023asdof?341525iwhapfohe")

[1] "asdof?iwhapfohe"

> grep("[0-9]",c("asd?ofih","askoufh21938"))

[1] 2

> grep("^[0-9]",c("asd?ofih","askoufh21938"))

integer(0)

> grep("[0-9]$",c("asd?ofih","askoufh21938"))

[1] 2

> grep("[0-9]",c("asd?ofih","askoufh21938"))

[1] 2

> grep("[0-9]",c("asd?ofih","askoufh21938","a?sdlfh039465aposdf"))

[1] 2 3

> grep(".[0-9]+.",c("asd?ofih","askoufh21938","a?sdlfh039465aposdf"))

[1] 2 3

> grep(".[0-9].",c("asd?ofih","askoufh21938","a?sdlfh039465aposdf"))

[1] 2 3

> grep(".[0-9].$",c("asd?ofih","askoufh21938","a?sdlfh039465aposdf"))

[1] 2

> grep(".[0-9]+.$",c("asd?ofih","askoufh21938","a?sdlfh039465aposdf"))

[1] 2

> grep(".[0-9]$",c("asd?ofih","askoufh21938","a?sdlfh039465aposdf"))

[1] 2

> grep(".[0-9].$",c("asd?ofih","askoufh21938","a?sdlfh039465aposdf"))

[1] 2

> grep("^.[0-9].$",c("asd?ofih","askoufh21938","a?sdlfh039465aposdf"))

integer(0)

> grep(".",c("apofh","apesoh.apoeh"))

[1] 1 2

> grep("\\.",c("apofh","apesoh.apoeh"))

[1] 2

> sub("\\.","[","apesoh.apoeh")

[1] "apesoh[apoeh"

> grep("[",c("apofh","apesoh[apoeh"))

Error in grep("[", c("apofh", "apesoh[apoeh")) : 

  invalid regular expression '[', reason 'Missing ']''

> grep("\\[",c("apofh","apesoh[apoeh"))

[1] 2

#### apply i  sapply
####################
N<-100000
donant<-as.data.frame(1:N)
names(donant)<-"parti"
donant$aliq<-rpois(N,3)
 
## repeteix una fila varies vegades
system.time({
x<-NULL
for (i in 1:nrow(donant)){
  x <- c(x, rep(donant$parti[i],donant$aliq[i]))
}
})
  
system.time(
x2 <- sapply(1:nrow(donant), function(i) rep(donant$parti[i],donant$aliq[i]))
)
x2<-unlist(x2)

## enumera les vegades que surt un individu
x2<-sort(x2)
tt<-table(x2)
system.time(
ordre <- sapply(1:length(tt), function(i) 1:tt[i])
)
ordre<-unlist(ordre)
cbind(x2,ordre)[1:100,]



## per indicar quin es el registre ultim 
id <- c(rep(1,4), rep(2, 2), rep(3, 5))
sequ <- c(1,2,3,4,1,2,1,2,3,4,5)
dat <- data.frame(id,sequ)

tt<-table(dat$id)
dat2<-data.frame(id=names(tt),freq=as.integer(tt))

dat<-merge(dat,dat2,by="id",all.x=TRUE)
dat$ultimo<-as.numeric(with(dat,freq==sequ))

##################################################################
###########   selccionar ultima entrada   ########################
##################################################################

## partim d'una base de dades: els individus = id_unic; estan enurants com "id"
## vull quedar-me l'ultim "id" de cada "id_unic"
id_unic  <- c(rep("AAA", 3), rep("BBB", 4), rep("CCC",1), rep("DDD", 2))
id <- sample(seq(1:length(id_unic)))
xdat <- as.data.frame(cbind(id, id_unic))
xdat$id <- as.numeric(as.character(xdat$id))
xdat$id_unic <- as.character(xdat$id_unic)

## la poso per ordre
xdat <- xdat[order(xdat$id_unic, xdat$id), ]

## li poso la variable "orden"
kk <- table(sort(xdat$id_unic))
orden <- sapply(1:length(kk), function(i) 1:kk[i])
xdat$orden <- unlist(orden)

## calculo les vegades que surt cada id_unic
tt <- table(xdat$id_unic)
dat2<-data.frame(id_unic=names(tt),freq=as.integer(tt))

## afageixo la informacio de les vegades que surt cada id_unic
xdat<-merge(xdat,dat2,by="id_unic",all.x=TRUE)

## els que orden==freq es el ultim
xdat$ultimo<-as.numeric(with(xdat,freq==orden))

##################################################################
##################################################################
##################################################################


## per posar una data a cadena
(fecha <- chron("15-05-2016", format="d-m-Y", out.format=c(dates="day-mon-year")))
class(fecha)
(fecha2 <- format(as.Date(fecha), "%d-%m-%Y"))
class(fecha2)

## per saber quin es converteix a missing a transformar a numero
x<-c("2.1","2,2",NA)
x<-trim(x)
x<-ifelse(x=='',NA,x)
ww1<-which(is.na(x))
x2<-as.double(x)
ww2<-which(is.na(x2))
ww<-ww2[!ww2%in%ww1]
x[ww]


## per guardar amb cadena les estiquetes de les variables
xxx<-NULL
x2<-wik$flow
for (i in 1:length(x2)){
  x1<-names(attr(wik$flow,"value.labels")[attr(wik$flow,"value.labels")==x2[i]])
  xxx<-rbind(xxx,x1)
}

wik$flow2<-as.vector(xxx)

## per cambiar l'rodre dels levels d'un factor
dat$bmicat<-factor(dat$bmicat, c("<24", "[24-30)", "30+"))


## la data del systema
Sys.Date()


## per llegir dades d'un servidor
setwd("/run/user/jvila/gvfs/sftp:host=134.0.8.34,user=ars/home/ars/ESTUDIS/ALTRES/jvila/mgil/partners/")
dat<-read.csv("partners.csv", sep=";", header = TRUE, allowEscapes=FALSE)


## per llegir MySQL
install.packages("DBI")
install.packages("RMySQL",lib= "/home/jvila/R/i486-pc-linux-gnu-library/3.1/lib")
## he anat a UBUNTU Software centre i he installat
## libmysqlclient-dev

## he instal.lat el package linux (previament ho'havia baixat el tar.gz
## R CMD INSTALL /home/jvila/Downloads/RMySQL_0.9-3.tar.gz

library(RMySQL)
con2 <- dbConnect(MySQL(), user="web", password="ieT6io9z", dbname="web", host="134.0.8.34")
con2 <- dbConnect(MySQL(), user="userdbcr", password="7437fgs78", dbname="iCRDvas", host="crd.ivascular.es")

dbGetQuery(con2, "SET NAMES utf8")

con2 <- dbConnect(MySQL(), user="root", password="xxx127", 
                  dbname="Modul1", host="localhost")
dbListTables(con2) 
dbListFields(con2, "congelador")

mydata <- dbReadTable(con2, "congelador")
dbWriteTable(con2, "mmar", subtr9500)

dbDisconnect(con2)

## per trobar un caracter en una cadena
regexpr("a", "bcvgdhdbbfassss")[[1]]

##
install.packages("png",lib= "/home/jvila/R/i486-pc-linux-gnu-library/3.1/lib")
library(png)

## per instalar un tar.gz
install.packages("C:/programs/Dropbox/JVila/compareGroups_2.1.tar.gz", repos= NULL,
                 type= "source")
cGroupsWUI()

## per retardar l'execucio
?Sys.sleep

## per treure els warning
options(warn=-1)

## per carregar una base de dades de la web
setwd("/run/user/jvila/gvfs/sftp:host=134.0.8.34,user=ars/home/ars/ESTUDIS/L02_MUTUA/Analisi/screening/")
load("./dat/2013-11-13.RData")


## per ordenar un factor
value.lab<-c("<35"=1, "35-44"=2, "45-54"=3, "55+"=4)
dat$agegr<-factor(dat$agegr,levels=sort(value.lab),labels=names(sort(value.lab)))


## per buscar una cadena entre fitxers
ff<-list.files("U:/Estudis/Tancats/A37_GEDAPS",pattern=".r$",recursive=TRUE,full=TRUE)

for (i in ff){
  temp<-scan(what="character",file=i,sep="\n",quiet=TRUE)
  if(length(ww<-grep(">8",temp))>0){
    cat("---------",i,"-----------\n")
    print(temp[ww])  
    cat("\n")
  }
}


## per saber la versi??
sessionInfo()

## per fer vanilla
/usr/bin/R --vanilla --slave --args "Hospital de la Monta??a", "67676767678" < /home/ars/ESTUDIS/L02_MUTUA/Analisi/Cardiovascular/empresa/Maker.R
/usr/bin/R --vanilla --slave  < /home/ars/ESTUDIS/L01_DEMCOM/Analisi/queries/maker.R


## per codis ascci i utf8
library(oce)
integerToAscii(126L)
paste(rep("??", 10), collapse="")

paste(rep(integerToAscii(175L), 10), collapse="")
cat(integerToAscii(194L), integerToAscii(175L), sep="" )


today<-chron(as.character(Sys.Date()), format="Y-m-d", out.format="d-mon-Y")

sessionInfo()

## Per a calcular la memoria
library(memuse)
howbig(10000, 500)

## retraasar un segons l'execucio
?Sys.sleep()

################################################################################
############  EXEMPLE Inserir dades a MySQL   ##################################
################################################################################
## insert bd sql
library(RMySQL)
library(chron)

# db connect
con<- dbConnect(MySQL(), user="web", password="ieT6io9z",dbname="web", host="localhost")

taula<-"salut_laboral_tabac"
ndatasql<-dbListFields(con,taula)
dat<-smk
ndatar<-names(dat)

xxx<-ndatar[ndatar%in%ndatasql]
yyy<-ndatasql[ndatasql%nin%ndatar]

dat$idu<-""
dat$time<-format(Sys.time(), "%Y-%m-%d  %H:%M:%S")

# ordena y elige las variables.
varilab<-scan(what="character", sep="\n")
idu
id
cigar
fuma
inifum
puros
pipas
minutes
dificul
whatcigar
smkmorning
smkill
hasta1
morning
cigar2
ncigar
fager
fagercat
situ
time

dat<-dat[, varilab]

# insert taula 
cadena<-paste("INSERT INTO ", taula," VALUES('",paste(dat[1,],collapse=","),"')",sep ="")
cadena<-gsub(",","','",cadena)

#dbGetQuery(con,cadena)


## llegir, dins de un  path, la part del nom del fitxer
indiv<-basename(xfile)

## la part inicial i la part final
indiv<-sub("^indiv_","",indiv)
indiv<-sub("\\.csv$","",indiv)



#############################################################################
### afegir casos a un ACCESS
#############################################################################
setwd("c:/xxx")
a <- c(1,2,3,4,5)
b <- c("a", "b", "c", "d", "e")
dat <- as.data.frame(cbind(a, b))
names(dat) <- c("numero", "caracter")
dat$numero <- as.numeric(dat$numero)
dat$caracter <- as.character(dat$caracter)
dat2 <- dat
dat2$numero <- dat2$numero*10
export.ACCESS (dat, "xxx.mdb", table.name= "mitabla")

con <- odbcConnectAccess("xxx.mdb")
sqlSave(con, dat=dat2, tablename = "mitabla", append = TRUE, 
  rownames = FALSE, safer = FALSE)


###
## per netajar la consola
cat("\014")

###############################################################################
## per saber el que pesen els objectes
.ls.objects <- function (pos = 1, pattern, order.by = "Size", decreasing=TRUE, head = TRUE, n = 10) {
  # based on postings by Petr Pikal and David Hinds to the r-help list in 2004
  # modified by: Dirk Eddelbuettel (http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session) 
  # I then gave it a few tweaks (show size as megabytes and use defaults that I like)
  # a data frame of the objects and their associated storage needs.
  napply <- function(names, fn) sapply(names, function(x)
          fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size) / 10^6 # megabytes
  obj.dim <- t(napply(names, function(x)
            as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}
.ls.objects()


################################################################################
## per canviar el codi a UTF-8
Encoding(attr(dat$pesfuer, "vari.label")) <- "latin1"
attr(dat$pesfuer, "vari.label") <- iconv(attr(dat$pesfuer, "vari.label"), "latin1", "UTF-8")
Encoding(names(attr(dat$pesfuer, "value.labels"))) <- "latin1"
names(attr(dat$pesfuer, "value.labels"))<- iconv(names(attr(dat$pesfuer, "value.labels")), "latin1", "UTF-8")


####packages 
install.packages("shiny")
install.packages("compareGroups")
install.packages("gam")
install.packages("png")
install.packages("epitools")
install.packages("pROC")
install.packages("psych")
install.packages("plotrix")
install.packages("knitr")
install.packages("chron")
install.packages("rgdal")
install.packages("pgirmess")
install.packages("stringr")
install.packages("MASS")
install.packages("nnet")
install.packages("car")
install.packages("RODBC")
install.packages("survival")
install.packages("lattice")
install.packages("cluster")
install.packages("Hmisc")
install.packages("xtable")
install.packages("gdata")
install.packages("oce")
install.packages("tcltk2")
install.packages("odfWeave")
install.packages("Rcmdr")
install.packages("extrafont")
install.packages("xlsx")

## per saber la versi?? d'un paquest
packageDescription("shiny")

## fer una taula d'un table2
<<echo=FALSE, results='hide', warning=FALSE, message=FALSE>>=
xdat <- prepare(dat[, c("id", "idcentro")])
x <- table2(xdat$idcentro)
yy <- cbind(unlist(attr(x, "dimnames")[1]), x[1:length(x)])
xtable(yy)
@
\begin{table}[H]
\centering
\caption{Recruited participants by center}
\
\\
\begin{tabular}{lr}
\hline
&\\
& N (\%)\\
&\\
<<echo=FALSE, results='asis', warning=FALSE, message=FALSE>>=
print.xtable(xtable(yy), only.contents=TRUE, include.rownames = FALSE,
             include.colnames=FALSE, hline.after=FALSE)
@
\hline
\end{tabular}
\end{table}
  
## per que no surti un output
{sink("/dev/null"); x <- table2(dat$avulsio, margin=0); sink()}

## per treballar contra el servidor
setwd("/run/user/1000/gvfs/sftp:host=134.0.8.34,user=ars/home/ars")
list.files()


################################################################################
################################################################################
## per posar el s??mbol major o igual
plot(0, 0) 
title(main=
      eval(parse(text='expression(phantom("")<=phantom(""))'))
      )

aaa <- "xxx"
plot(0, 0) 
title(main=
       eval(substitute(expression( a + phantom("")<=phantom("")), list(a = aaa)))
      ) 

aaa <- "xxx"
bbb <- "yyy"
plot(0, 0) 
title(main=
       eval(substitute(expression(paste(a, phantom("")<=phantom(""), b)), 
        list(a = aaa, b= bbb)))
      ) 

bbb <- "750 Kcal/sem."
plot(0, 0) 
title(main=
       eval(substitute(expression(paste(phantom("")>=phantom(""), b)), 
        list(b= bbb)))
      ) 

bbb <- "750 Kcal/sem."
ccc <- " = 80%"
plot(0, 0) 
text(0,-0.2,  eval(substitute(expression(paste(phantom("")>=phantom(""), b, c)), 
        list(b= bbb, c=ccc)))
     )
################################################################################
################################################################################
## per canviar el code
Encoding(dat$puesto) <- "latin1"
dat$puesto <- iconv(dat$puesto, "latin1", "UTF-8")


################################################################################
################################################################################
## per modificar celles de un EXCEL
rm(list=ls())
setwd("/DATA/scratch_isaac/EUROTRACS")
options(java.parameters = "-Xmx4g")
library(XLConnect)
library(xlsx)


file.remove("suma2.xlsx")
file.copy("suma.xlsx", "suma2.xlsx",overwrite=TRUE)

# read input and ouput
wb <- XLConnect::loadWorkbook("suma2.xlsx")
XLConnect::readWorksheet(wb, sheet = "Hoja1",header=FALSE,startRow=1,startCol=1,endRow=8,endCol=4)
#xlsx::read.xlsx(file="suma2.xlsx", sheetName="Hoja1", rowIndex=1:3,colIndex=1,header=FALSE)

# modify cells
writeNamedRegionToFile("suma2.xlsx",2, name="yyy",formula = "Hoja1!$A$1",header=FALSE,rownames=NULL)
wb <- XLConnect::loadWorkbook("suma2.xlsx")
XLConnect::setForceFormulaRecalculation(wb, sheet = "Hoja1", TRUE)
XLConnect::readWorksheet(wb, sheet = "Hoja1",header=FALSE,startRow=1,startCol=1,endRow=4,endCol=1)

## per augmentar la memoria
options(java.parameters = "-Xmx4000m")


## per llegir xlsx
installXLSXsupport()

## per llegir dates des de EXCEL que entren numeros
chron(as.numeric(as.character(dat$fecha))-365.5*70+16, out.format = "d/m/yy")


## Per fer un bucle (la funcio Recall())
mydata <- function() {
n1<-round(runif(1, 180, 190), 0)
mcguill1<-SimCon(n1,29.8,11.9,10,100,0)
n0<-round(runif(1, 180, 190), 0)
mcguill0<-SimCon(n0,29.8,11.9,10,100,0)
group<-c(rep(1,n1), rep(0,n0))
dat<-as.data.frame(cbind(c(mcguill1, mcguill0), group))
names(dat)<-c("mcguill", "group")
m1<-format(signif(mean(subset(dat, group==1)$mcguill), digits=3), scientific=FALSE)
m0<-format(signif(mean(subset(dat, group==0)$mcguill), digits=3), scientific=FALSE)
tval<-signif(with(dat, t.test(mcguill~group, var.equal=TRUE))$statistic, 4)
pval<-with(dat, t.test(mcguill~group, var.equal=TRUE))$p.value
        if (pval > 0.2) return(dat)
        Recall()
 }   

dat <- mydata()   


