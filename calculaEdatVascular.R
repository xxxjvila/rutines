rm(list=ls())
RutinesLocals<-"C:/Programs/Dropbox/rutines"
source(file.path(RutinesLocals,"calculadora.risc.r"))

################################################################################
################ Valors de una persona   #######################################
################################################################################

sex <- 0
age <- 35
coltot <- 210
hdl <- 52
tas <- 130
tad <- 70
diab <- 0
smoke <- 1

################################################################################
################   Calculs   ##################################################
################################################################################
persona <- calculadora.risc(sex,age,coltot,hdl,tas,tad,diab,smoke
                ,calibrated=TRUE,age.range=c(35,74))*100

## Jaume diu que posi millor combinacio
perfectHDL <- ifelse(sex==1, 49, 59)
perfect <- calculadora.risc(sex,age, coltot= 190, hdl=perfectHDL,tas=129,tad=84,
                            diab=0,smoke=0,calibrated=TRUE,age.range=c(35,74))*100


## aqui calculo el Risc Relatiu
RR <- round(persona/perfect, 1)

## calculo el risc per a totes les edats (data.frame "resu")
resu <- NULL
for ( i in 35:74){
  x <- calculadora.risc(sex,age= i,
                        coltot= 190,hdl=perfectHDL,tas=129,tad=84,
                        diab=0,smoke=0,calibrated=TRUE,age.range=c(35,74))*100
  resu <- rbind(resu, c(i, x))
}
resu <- as.data.frame(resu)
names(resu) <- c("age", "risk")

## a les dones com que tenen una tendencia quadratrica, 
## poso el risk mes alt a les edats altes (aprox a partir de 63a)
xage <- subset(resu, risk == max(resu$risk))$age[1]
resu$risk <- with(resu, ifelse(sex==0 & age >=xage, max(risk), risk))

## aqui calculo la edat vascular
ageCV <- max(subset(resu, risk < persona)$age)


