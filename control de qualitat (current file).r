library(foreign)


f<-function(dades,missings,max.valors=32,desc=NULL,dates=NULL,max.rare.values=3,max.factor=TRUE){


#dades=res$dades;desc=NULL;dates=res$dates.var;missings=res$dict[,"missings"]


	file.res<-paste(dirname(file),"/control qualitat (",gsub(".sav","",basename(file)),").xls",sep="")

	var.names<-attributes(dades)$names
	vari.labels<-attributes(dades)$variable.labels
	if (is.null(vari.labels)) vari.labels<-rep(" ",length(var.names))  # no té etiquetes
	if (!is.null(vari.labels)) vari.labels<-cbind(var.names,vari.labels)
	row.names(vari.labels)<-NULL

	write.table(paste("************ CONTROL DE QUALITAT:",gsub(".sav","",basename(file)),"************",sep=" "),file.res,row.names=FALSE,col.names=FALSE)


	write.table(paste("*** EL NÚMERO TOTAL DE FILES DE LA BASE DE DADES ÉS ",nrow(dades),sep=""),file.res,row.names=FALSE,col.names=FALSE,append=TRUE)


	write.table(" ",file.res,row.names=FALSE,col.names=FALSE,append=TRUE)


	if (is.null(desc)) mirar=1:ncol(dades)
	if (!is.null(desc)) mirar=which(!names(dades)%in%desc)


	for (i in mirar){


	##i<-which(names(dades)=="LP")

	##print(names(dades)[i])

		write.table(" ",file.res,row.names=FALSE,col.names=FALSE,append=TRUE)
		write.table(cbind("*********",vari.labels[i,1],"*********"),file.res,sep="\t",row.names=FALSE,col.names=FALSE,append=TRUE)
		write.table(cbind("-. Etiqueta: ",vari.labels[i,2]),file.res,sep="\t",row.names=FALSE,col.names=FALSE,append=TRUE)

		var<-dades[,i]

		if (!missings[i]=="NOMISS"){
			write.table("-. Valors declarats missings",file.res,row.names=FALSE,col.names=FALSE,append=TRUE)
			write.table(missings[i],file.res,sep="\t",row.names=FALSE,col.names=FALSE,append=TRUE)
		}

		## VARIABLES BUIDES
		if (all(is.na(var) | gsub(" ","",as.character(var))==""))
			write.table("la variable està buida",file.res,row.names=FALSE,col.names=FALSE,append=TRUE)

	
		## VARIABLES NO BUIDES
		if (!(all(is.na(var) | gsub(" ","",as.character(var))==""))){

			t=data.frame(table(var))

			names(t)[1]="values"

			t$values<-sort(unique(var))

			if (names(dades)[i]%in%dates){
				t$values<-ISOdate(1582,10,14)+t$values
				t$values<-substr(as.character(t$values),1,10)
			}


			## VARIABLES CATEGÒRIQUES
			if (nrow(t)<max.valors){

				value.labels=attributes(var)$value.labels
		
				if (!is.null(value.labels)){
					values=as.integer(value.labels)
					labels=names(value.labels)
					value.labels=data.frame(list(labels=labels[order(values)],values=sort(values)))
					value.labels$labels=paste("'",value.labels$labels,sep="")
					taula=merge(value.labels,t,all=TRUE,by="values")
					taula$Freq=ifelse(is.na(taula$Freq),0,taula$Freq)
					write.table("-. Taula de freqüències",file.res,row.names=FALSE,col.names=FALSE,append=TRUE)
					write.table(taula,file.res,sep="\t",row.names=FALSE,append=TRUE)
					num.miss<-sum(is.na(dades[,i]))
					perc.miss<-paste(round(num.miss/nrow(dades)*100,2),"%",sep="")
					write.table(cbind("-. Missings (%): ",paste(num.miss," (",perc.miss,")",sep="")),file.res,row.names=FALSE,col.names=FALSE,sep="\t",append=TRUE)

					##@@@ SELECCIONAR ELS INDIVIDUS AMB VALORS NO ETIQUETATS. var.identificadora[var==taula[is.na(taula$labels),"values"]]

				}

				if (is.null(value.labels)){
					write.table("-. Taula de freqüències",file.res,row.names=FALSE,col.names=FALSE,append=TRUE)
					write.table(t,file.res,sep="\t",row.names=FALSE,append=TRUE)
					num.miss<-sum(is.na(dades[,i]))
					perc.miss<-paste(round(num.miss/nrow(dades)*100,2),"%",sep="")
					write.table(cbind("-. Missings (%): ",paste(num.miss," (",perc.miss,")",sep="")),file.res,row.names=FALSE,col.names=FALSE,sep="\t",append=TRUE)
				}

			}

			if (nrow(t)>=max.valors){

				## VARIABLES CONTÍNUES 
				if (!is.character(var) & !is.factor(var)){

					n.valid=sum(!is.na(var))  # número de valors vàlids (comptant els possibles rars)

					value.labels=attributes(var)$value.labels

					rare.values=NULL
					
					if (!is.null(value.labels)){

						## VALORS ETIQUETATS (ES CONSIDEREN TAMBÉ RARS)
						rare.values=as.integer(value.labels)
						values=as.integer(value.labels)
						labels=names(value.labels)	
						value.labels=data.frame(list(labels=labels[order(values)],values=sort(values)))
						if (names(dades)[i]%in%dates){
							value.labels$values<-ISOdate(1582,10,14)+as.integer(value.labels$values)
							value.labels$values<-substr(as.character(value.labels$values),1,10)
						}
						value.labels$labels=paste("'",value.labels$labels,sep="")
						write.table("-. Valors etiquetats",file.res,row.names=FALSE,col.names=FALSE,append=TRUE)
						write.table(value.labels,file.res,sep="\t",dec=",",row.names=FALSE,col.names=TRUE,append=TRUE)
						
					}

					## VALORS RARS 

					valors<-as.double(names(table(var)))
					valors<-as.double(scale(valors,scale=FALSE))
					valors=valors^2
		
					d=dist(valors)
					cut.tree<-cutree(hclust(d),2)

					freq<-as.integer(table(cut.tree))
					grup.min=as.integer(names(table(cut.tree))[which(as.integer(table(cut.tree))==min(freq))])

					rare.values<-c(rare.values,as.double(names(table(var))[cut.tree==grup.min]))
					rare.values=unique(rare.values)

					if (length(rare.values)>max.rare.values) rare.values=NULL

					if (!is.null(rare.values) & length(rare.values)>0){
						rare.values<- var[round(var,10)%in%round(rare.values,10)]
						var=var[!round(var,10)%in%round(rare.values,10)]
					}

					if (names(dades)[i]%in%dates & !is.null(rare.values) & length(rare.values)>0){
						rare.values<-ISOdate(1582,10,14)+rare.values
						rare.values<-substr(as.character(rare.values),1,10)
					}
						
					if (!is.null(rare.values) & length(rare.values)>0){
						write.table("-. Valors rars (allunyats de la resta)",file.res,row.names=FALSE,col.names=FALSE,append=TRUE)
						table.rare.values<-cbind(unique(rare.values),table(rare.values))
						colnames(table.rare.values)=c("value","freq")
						write.table(table.rare.values,file.res,sep="\t",dec=",",row.names=FALSE,col.names=TRUE,append=TRUE)
					}
					
					cont<-c(mean(var,na.rm=TRUE),sd(var,na.rm=TRUE),min(var,na.rm=TRUE),max(var,na.rm=TRUE),quantile(var,c(0.05,0.95),type=6,na.rm=TRUE))
					cont<-structure(cont,names=c("mitjana","sd","min","max","perc.5","perc.95"))

					if (names(dades)[i]%in%dates){
						cont<-ISOdate(1582,10,14)+cont
						cont<-substr(as.character(cont),1,10)
						cont<-cont[-(1:2)]
						cont<-structure(cont,names=c("min","max","perc.5","perc.95"))
					}
					write.table(rbind(c("-. N válida = ",n.valid)),file.res,sep="\t",row.names=FALSE,col.names=FALSE,append=TRUE)
					write.table("-. Descriptius",file.res,row.names=FALSE,col.names=FALSE,append=TRUE)
					write.table(t(cont),file.res,sep="\t",dec=",",row.names=FALSE,col.names=TRUE,append=TRUE)
				}

				## VARIABLES CATEGORIQUES
				if (is.character(var) | is.factor(var)){
					if (max.factor){ ## si hi ha moltes categories no ho llista (p.e. marca de tabac).
						if (nrow(t)<max.valors){
							write.table("-. Taula de freqüències",file.res,row.names=FALSE,col.names=FALSE,append=TRUE)
							write.table(t,file.res,sep="\t",row.names=FALSE,append=TRUE)			
							num.miss<-sum(is.na(dades[,i]))
							perc.miss<-paste(round(num.miss/nrow(dades)*100,2),"%",sep="")
							write.table(cbind("-. Missings (%): ",paste(num.miss," (",perc.miss,")",sep="")),file.res,row.names=FALSE,col.names=FALSE,sep="\t",append=TRUE)
						}
						if (nrow(t)>=max.valors)
							write.table(paste("-. OBSERVACIÓ: No es dona la taula de freqüències per tenir més de ",max.valors," categories",sep=""),file.res,row.names=FALSE,col.names=FALSE,append=TRUE)

					}
				}

			}
		

		}

	}

}

#############



source("read.spss2.r")
source("spss_var.rpk")


################################################################
################################################################
########### FI FUNCIONS PRÈVIES ##############################
################################################################




#file=choose.files(default = "*.sav", caption = "Selecciona BD de SPSS")
res<-read.spss2(file)
names(res$dades)<-spss_varlist(file)$longname   	# poso els noms de l'SPSS.
desc=select.list(names(res$dades),multiple=TRUE,title="Variables descartades")
f(dades=res$dades,desc=desc,dates=res$dates.var,missings=res$dict[,"missings"])


