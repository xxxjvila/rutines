RutinesLocals<-"u:\\ULEC\\Software\\R cran project\\rutines"

source(file.path(RutinesLocals,"carrega.llibreria.r"))
source(file.path(RutinesLocals,"table2.r"))
carrega.llibreria(c("Hmisc"))
                                           

IMT.bmode <- function(bmode) {
  
#en este orden-->estudi, parti, seg, imt_segment, imt_mean, imt_max, proceden
  
bmode$id<-paste(bmode$parti,bmode$imt_segment)
repes<-table(bmode$id)
repes<-names(repes)[repes>1]
repes

ids<-unique(bmode$parti[bmode$id%in%repes])
bmode<-bmode[!is.na(bmode$imt_segment),]
dim(bmode)

bmode<-bmode[bmode$parti%nin%ids,]

names(bmode)
bmode<-remove.vars(bmode,"id")

imt<-as.data.frame(unique(bmode[, c("estudi","parti","seg","proceden")]))

imt$imta_lbul<-NA
imt$imta_lcca<-NA
imt$imta_lica<-NA
imt$imta_rbul<-NA
imt$imta_rcca<-NA
imt$imta_rica<-NA

xbmode<-bmode
xbmode$imt_mean<-round(xbmode$imt_mean,3)
xbmode$imt_segment<-car::recode(bmode$imt_segment,"'LBUL'='imta_lbul';'LCCA'='imta_lcca';'LICA'='imta_lica';'RBUL'='imta_rbul';'RCCA'='imta_rcca';'RICA'='imta_rica'")

for (i in 1:nrow(xbmode))
{
  imt[imt$parti==xbmode[i,2], xbmode[i,4]]<-xbmode[i,5]
}

vars<-c("imta_lcca","imta_rcca")
imt$imta_lrcca<-apply(imt[,c(vars)],1,mean, na.rm=FALSE)

attr(imt$imta_lbul,"vari.label")<-"Averaged IMT - LBUL (Left bulbus)"
attr(imt$imta_lcca,"vari.label")<-"Averaged IMT - LCCA (Left common carotid artery)"
attr(imt$imta_lica,"vari.label")<-"Averaged IMT - LICA (Left internal carotid artery)"
attr(imt$imta_rbul,"vari.label")<-"Averaged IMT - RBUL (Right bulbus)"
attr(imt$imta_rcca,"vari.label")<-"Averaged IMT - RCCA (Right common carotid artery)"
attr(imt$imta_rica,"vari.label")<-"Averaged IMT - RICA (Right internal carotid artery)"
attr(imt$imta_lrcca,"vari.label")<-"Averaged IMT - (LCCA-RCCA)"   

imt<-remove.vars(imt,c("estudi","seg"))

################################################
imt2<-as.data.frame(unique(bmode[, c("estudi","parti","seg")]))

imt2$imtm_lbul<-NA
imt2$imtm_lcca<-NA
imt2$imtm_lica<-NA
imt2$imtm_rbul<-NA
imt2$imtm_rcca<-NA
imt2$imtm_rica<-NA

xbmode2<-bmode
xbmode2$imt_max<-round(xbmode2$imt_max,3)
xbmode2$imt_segment<-car::recode(bmode$imt_segment,"'LBUL'='imtm_lbul';'LCCA'='imtm_lcca';'LICA'='imtm_lica';'RBUL'='imtm_rbul';'RCCA'='imtm_rcca';'RICA'='imtm_rica'")

for (i in 1:nrow(xbmode2))
{
  imt2[imt2$parti==xbmode2[i,2], xbmode2[i,4]]<-xbmode2[i,6]
}

vars<-c("imtm_lcca","imtm_rcca")
imt2$imtm_lrcca<-apply(imt2[,c(vars)],1,max, na.rm=FALSE)

attr(imt2$imtm_lbul,"vari.label")<-"Maximum IMT - LBUL (Left bulbus)"
attr(imt2$imtm_lcca,"vari.label")<-"Maximum IMT - LCCA (Left common carotid artery)"
attr(imt2$imtm_lica,"vari.label")<-"Maximum IMT - LICA (Left internal carotid artery)"
attr(imt2$imtm_rbul,"vari.label")<-"Maximum IMT - RBUL (Right bulbus)"
attr(imt2$imtm_rcca,"vari.label")<-"Maximum IMT - RCCA (Right common carotid artery)"
attr(imt2$imtm_rica,"vari.label")<-"Maximum IMT - RICA (Right internal carotid artery)"
attr(imt2$imtm_lrcca,"vari.label")<-"Maximum IMT - (LCCA-RCCA)"   

imt2<-remove.vars(imt2,c("estudi","seg"))


imt<-merge2(imt,imt2,by.id="parti",all=TRUE,sort=FALSE)




return(imt) 


}


