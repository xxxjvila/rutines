causa.exitus <- function(cie, status=9, fexit=NA, cexit=NA, sexitus=1, sobran=0) {

#cie<-cie9 or cie10
#status<-Por defecto "1='muerto'"
#fexit<-'diaexit' or 'mesexit' or 'anyoexit'
#sobran<-si es '0', se devuelve la causa de exitus, si es '>0' se devuelve 'xcieIsobran' (si es CIE que sobra o no)
#(los CIEs que sobran)
#sexitus<-Aqui se pondra el valor que tienen los muertos en la variable 'status'


####  CEXIT 1   (MUERTE POR CARDIOPATIA ISQUÉMICA)
cie_1_9<-c(413,seq(4130,4139,by=1),
           seq(410,412,by=1),seq(4100,4129,by=1),
           414,seq(4140,4149,by=1),           
           7981,
           7982,
           4275,
           7989)

cie_1_10<-c('I200','I201','I208','I209',paste('I',210:214,sep=""),paste('I',219:221,sep=""),'I228','I229',
           'I236','I240','I241','I248','I249',paste('I',250:256,sep=""),'I258','I259','I461','R96','R960','R961','R98',
           'I469','I20','I21')


####  CEXIT 2 (MUERTE POR ENFER. CEREBROVASCULAR)
cie_2_9<-c(seq(430,434,by=1),seq(4300,4349,by=1),
           seq(436,438,by=1),seq(4360,4373,by=1),
           seq(4375,4377,by=1),
           seq(4379,4389,by=1),
           435,seq(4350,4359,by=1))

cie_2_10<-c(paste('I',600:616,sep=""),paste('I',618:621,sep=""),paste('I',629:636,sep=""),'I638','I639','I64',paste('I',670:679,sep=""),
            'I688',paste('I',690:694,sep=""),'I698',paste('I',650:669,sep=""),paste('G',450:452,sep=""),'G454',paste('G',458:463,sep=""),'G468',
            'I64X','I67','F011','F019')
                                
####  CEXIT 3 (MUERTE POR OTRAS CAUSAS CV)
cie_3_9<-c(4438,4439,
           401,seq(4010,4019,by=1),
           402,seq(4020,4029,by=1),
           403,seq(4030,4039,by=1),
           404,seq(4040,4049,by=1),
           405,seq(4050,4059,by=1),
           426,seq(4260,4269,by=1),
           427,seq(4270,4274,by=1), seq(4276,4279,by=1),           
           428,seq(4280,4289,by=1),
           seq(4291,4299,by=1),
           557,5579,785,746,5184,5571)

cie_3_10<-c('I738','I739','I792','I10','I110','I119','I29',
            paste('I',30:32,sep=""),'I39',paste('I',50:52,sep=""),'I58','I59',paste('I',440:455,sep=""),
            'I458','I459','I498','I460',paste('I',470:472,sep=""),'I479','I48',paste('I',490:495,sep=""),
            'I499','R0014','R008','R012','I500','I501','I509','I515','I516','K55','K550','K551','K559','J81','J81X','R570','Q213','Q245')

####  CEXIT 4 (MUERTE POR OTRA CAUSA NO CV)
cie_4_9<-c('418','459','4590')
cie_4_10<-c('I504','I647','I648','I725','I780','I120')


####  CIE 3 SOBRAN (Todas las: 390-445, 447-459) menos las demas CIEs 9
x_cie_3_9_sobran<-c(seq(390,445),seq(3900,4459,by=1),
          seq(447,459),seq(4470,4599,by=1))                  
cie_3_9_sobran<-x_cie_3_9_sobran[x_cie_3_9_sobran %nin%c(cie_1_9,cie_2_9,cie_3_9,cie_4_9)]


##############################  Asigno la causa segun el CIE  ##################
### Exitus con CIE = 4, 
### Exitus sin CIE = 9
cie_f<-ifelse(!is.na(cie),4,ifelse(!is.na(cexit),cexit,ifelse((status==sexitus & !is.na(status)) | !is.na(fexit),9,NA)))


#CIE 1
cie_f<-ifelse(cie%in%cie_1_9 & (cie_f==4|cie_f==9) , 1 , cie_f)
cie_f<-ifelse(cie%in%cie_1_10 & (cie_f==4|cie_f==9) , 1 , cie_f)

#CIE 2
cie_f<-ifelse(cie%in%cie_2_9 & (cie_f==4|cie_f==9) , 2 , cie_f)
cie_f<-ifelse(cie%in%cie_2_10 & (cie_f==4|cie_f==9) , 2 , cie_f)

#CIE 3
cie_f<-ifelse(cie%in%cie_3_9 & (cie_f==4|cie_f==9) , 3 , cie_f)
cie_f<-ifelse(cie%in%cie_3_9_sobran & (cie_f==4|cie_f==9) , 3 , cie_f)
cie_f<-ifelse(cie%in%cie_3_10 & (cie_f==4|cie_f==9) , 3 , cie_f)


#CIE 4
cie_f<-ifelse(cie%in%cie_4_9 & (cie_f==4|cie_f==9) , 4 , cie_f)
cie_f<-ifelse(cie%in%cie_4_10 & (cie_f==4|cie_f==9) , 4 , cie_f)


#CIE 3 (las 'I' que sobran)
todasI<-grep("^I",cie,value=TRUE)
Isobran<-todasI[todasI %nin% c(cie_1_10, cie_2_10, cie_3_10, cie_4_10)]
xcieIsobran<-ifelse(cie%in%Isobran , 1 , 0)  #Informacion de las 'I' que sobran
#cie_f<-ifelse(cie%in%Isobran & (cie_f==4|cie_f==9) , 3 , cie_f)  #Asigno un '3' a las 'I' q sobran
cie_f<-ifelse(cie%in%Isobran , 3 , cie_f)  #Asigno un '3' a las 'I' q sobran



################################################################################

if (sobran==0)
  {
    return(cie_f)   
  }
else
  {
#    return(Isobran)
    return(xcieIsobran)
  }


}


# x1<-data.frame(cie=cie_1_9,tipo="cie_1_9")
# x2<-data.frame(cie=cie_2_9,tipo='cie_2_9')
# x3<-data.frame(cie=cie_3_9,tipo='cie_3_9')
# x4<-data.frame(cie=cie_4_9,tipo='cie_4_9')
# x5<-data.frame(cie=cie_1_10,tipo='cie_1_10')
# x6<-data.frame(cie=cie_2_10,tipo='cie_2_10')
# x7<-data.frame(cie=cie_3_10,tipo='cie_3_10')
# x8<-data.frame(cie=cie_4_10,tipo='cie_4_10')
# x9<-data.frame(cie=cie_3_9_sobran,tipo='cie_3_9_sobran')
 
# x<-as.data.frame(rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9))
# export.ACCESS(x,paste(carpeta,"\\","query.mdb",sep=""),"cie")
                    