##########################################################
##########################################################
###    ANÀLISI D'HAPLOTIPS AMB R: VERSIÓ XXX    ##########
##########################################################
##########################################################

### AUTOR: ISAAC SUBIRANA (isubirana@imim.es)

rm(list=ls())
options(warn=-1)  ## no surten en pantalla de R els missatges warning
options(show.error.messages = TRUE)   ## surten en pantalla de R els missatges d'error

################################################################
################### funcions prèvies  ##########################
################################################################


llibreries.disponibles<-.packages(all.available = TRUE)
path.llibreries<-"U:/ULEC/Software/R cran project/llibreries de R (versio 2.3)"
llibreries.locals<-list.files(path.llibreries)



####### carrega les llibreries necessàries #######

carrega.llibreria<-function(llibreria,path.llibreries="U:/ULEC/Software/R cran project/llibreries de R (versio 2.3)"){
# path.llibreries: carpeta on estan les llibreries en .zip
# llibreria: vector de character amb el nom de les llibreries a descarregar.

  if (sum(paste("package:",llibreria,sep="")%in%search())<length(llibreria)){ # si no no cal fer res: ja estan totes carregades

    llibreries.disponibles<-.packages(all.available = TRUE)
    
    llibreries.locals<-list.files(path.llibreries)
    llibreries.locals.curt<-as.character(sapply(llibreries.locals,function(x) unlist(strsplit(x,"_"))[1]))

    for (i in 1:length(llibreria)){
      if (!llibreria[i]%in%llibreries.disponibles){
        nom.llibreria.local<-llibreries.locals[which(llibreria[i]==llibreries.locals.curt)]
        cat("----","Downloading local package ",nom.llibreria.local," from ",path.llibreries,"----\n")
        install.packages(paste(path.llibreries,"/",nom.llibreria.local,sep=""),repos=NULL,destdir=.Library)
      }
      if (!paste("package:",llibreria[i],sep="")%in%search()){
        eval(parse(text=paste("require(",llibreria[i],")",sep="")))
      }
    }

  }

}

####### dona les coordenades reals del gràfic segons les coordenades unitàries (x,y) #######
convert<-function(x,y){
	screen(1)
	if (is.na(par()$xpd)) plt=c(0,1,0,1)
	if (!is.na(par()$xpd)) plt=par()$plt
	A<-matrix(rbind(c(1-plt[1],plt[1],0,0),c(1-plt[2],plt[2],0,0),
		c(0,0,1-plt[3],plt[3]),c(0,0,1-plt[4],plt[4])),4,4)
	b<-par()$usr
	max.coord<-solve(A,b)
	return(c(max.coord[1]+(max.coord[2]-max.coord[1])*x,max.coord[3]+(max.coord[4]-max.coord[3])*y))
}

####### dibuixa els intervals de la regressió ######
dibuixa.IC<-function(beta,se,OR,names){
	OR<-OR[1]
	par(las=1)
	K<-length(beta)
	OR<-rep(OR,K)
	estim<-ifelse(OR,exp(beta),beta)
	sup<-ifelse(OR,exp(beta+1.96*se),beta+1.96*se)
	inf<-ifelse(OR,exp(beta-1.96*se),beta-1.96*se)
	pvalor<-2*pnorm(beta/se,lower.tail=FALSE)
	if (OR[1]) ylim<-c(0,max(sup)+(max(sup)-min(inf))*0.2)
	if (!OR[1]) ylim<-c(min(inf)-(max(sup)-min(inf))*0.2,max(sup)+(max(sup)-min(inf))*0.2)
	x<-1:K
	colors<-c("black","red")
	plot(rep(x,2),c(inf,sup),type="n",xlab="",ylab=ifelse(OR[1],"Odds ratio","Effect"),xlim=c(1-0.5,K+0.5),axes=F,ylim=ylim)
	points(x,estim,pch=22,bg=colors[as.integer(pvalor<0.05)+1],col=colors[as.integer(pvalor<0.05)+1])
	arrows(x,inf,x,sup,length=0.08,angle=90,code=3,col=colors[as.integer(pvalor<0.05)+1])
	axis(2,pretty(seq(ylim[1],ylim[2],len=10)))
	abline(h=ifelse(OR[1],1,0),lty=3)
	axis(1,x,tick=F,labels=names,line=-0.5)
	title(y.label)
}


####### dibuixa els gràfics de barres de les frequències haplotips per grups #########

bar.plot<-function(type,title=grup.label,cex.legend=1,cex.table=1,cex.screen=0,dens=20,
		color=gray(0:(nrow(freq)-1)/nrow(freq)),legend.coord=NULL,bar.space=1,cex.values=0.9,
		legend.intersp=1,table.intersp=1,xtext=NULL,coord.xtext=NULL,table.title.w=-3,table.title.cex=1,
		primera=TRUE,canvi.cex.screen=FALSE){
	
	#gràfic en colors
	if (is.null(color)){
		windows(4,4,xpos=0,ypos=0)
		par(mar=c(0,0,0,0))
		plot(rep(1:25,25),rep(1:25,each=25),col=colours()[1:(25*25)],bg=colours()[1:(25*25)],pch=22,cex=2.3)
		windows<-dev.list()
		color<-rep(NA,nrow(freq))
		for (i in 1:nrow(freq)){
			coord<-locator(1)
			coord<-trunc(c(coord$x,coord$y)+0.5)
			color[i]<-colours()[(coord[1]+25*(coord[2]-1))]
		}
		dev.off(windows[length(windows)])
	}
	
	op <- par(no.readonly = TRUE)

	if (!primera) plot.new()
		
	if (type==3 ){
		taula.plot<-taula.freq[,-(N+1)]
		taula.plot[,(N+1):length(names(taula.plot))]<-round(taula.plot[,(N+1):length(names(taula.plot))]*100,1)
		row.names(taula.plot)<-paste("haplo",1:nrow(taula.plot),sep=".")
		names(taula.plot)[(N+1):(length(grup.value.labels)+N)]<-grup.value.labels		
	}

	if (type==4){
		taula.plot<-taula.freq[,1:N]
		row.names(taula.plot)<-paste("haplo",1:nrow(taula.plot),sep=".")
	}

	angle=rep(c(45,45+90),trunc(nrow(freq)/2))
	if (length(angle)<nrow(freq)) angle<-angle[-1]

	par(tcl=-0.4)

	# amb la taula com a llegenda a sota
	if (type==3 | type==4){
		#if (canvi.cex.screen) close.screen(all.screens=TRUE)
		close.screen(all.screens=TRUE)		
		if (cex.screen==0) cex.screen<-0.5
		split.screen(rbind(c(0,1,cex.screen,1),c(0,1,0,cex.screen)))
		screen(1)
		par(mgp=c(3,0.7,0),mar=c(0,4,3,4),xpd=FALSE)
	}

	mp<-barplot(freq*100,legend = FALSE,space=bar.space,plot=FALSE,ylim=c(0,110))
	try(barplot(freq*100,legend = FALSE,space=bar.space,ylab="haplotype frequencies (%)",main=title,las=1,plot=TRUE,cex.main=1,
		density=dens,angle=angle,col=color,axes=TRUE,xlim=c(mp[1]-(mp[2]-mp[1])/2,max(mp)+(mp[2]-mp[1])/2),ylim=c(0,110)))
	abline(h=0)

	if (type>1){
		altures.text<-freq/2
		for (i in 2:nrow(freq)) altures.text[i,]<-altures.text[i-1,]+(freq[i-1,]+freq[i,])/2
	}

	# amb els valors en les barres
	if (type==2 | type==4){
		points(matrix(rep(mp,nrow(freq)),nrow(freq),ncol(freq),byrow=TRUE),altures.text*100,type="p",cex=3*cex.values,pch=22,col=gray(1),bg=gray(1))
		points(matrix(rep(mp,nrow(freq)),nrow(freq),ncol(freq),byrow=TRUE)-min(mp)*0.08*cex.values,altures.text*100,type="p",cex=3*cex.values,pch=22,col=gray(1),bg=gray(1))
		points(matrix(rep(mp,nrow(freq)),nrow(freq),ncol(freq),byrow=TRUE)+min(mp)*0.08*cex.values,altures.text*100,type="p",cex=3*cex.values,pch=22,col=gray(1),bg=gray(1))					
		text(matrix(rep(mp,nrow(freq)),nrow(freq),ncol(freq),byrow=TRUE),altures.text*100,format(round(freq*100,1)),cex=cex.values)
	}

	# amb la taula com a llegenda a sota
	if (type==3 | type==4){
		#llegenda al costat
		par(xpd=NA)
		screen(1)
		if (is.null(legend.coord)) legend.coord<-c(max(mp)+min(mp)/2,100)
		try(legend(legend.coord[1],legend.coord[2],rev(row.names(taula.plot)),bty="n",cex=cex.legend,fill=rev(color),density=rev(dens),angle=rev(angle),y.intersp=legend.intersp))
		#llegenda de sota (taula)
		screen(2)
		par(mar=c(2,4,0,1),xpd=TRUE)
		if (type==4){
			try(textplot(taula.plot,show.rownames=TRUE,cex=cex.table,rmar=table.intersp))
			try(title("Haplotype codes",line=table.title.w,cex.main=table.title.cex))
		}
		if (type==3){
			if (cex.table==1) cex.table=0.8
			try(textplot(taula.plot,show.rownames=TRUE,cex=cex.table,rmar=table.intersp))
			try(title("Haplotype codes and frequencies",line=table.title.w,cex.main=table.title.cex))
		}				
	}
	if (!is.null(xtext)){	
		par(xpd=NA)
		text(coord.xtext[1],coord.xtext[2],xtext,adj=c(0,0),cex=0.9)
		par(xpd=TRUE)
	}

	par(op)
	
	return(invisible(list(tipus=tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,
		dens=dens,color=color,legend.coord=legend.coord,bar.space=bar.space,cex.values=cex.values,xtext=xtext,
		legend.intersp=legend.intersp,table.intersp=table.intersp,coord.xtext=coord.xtext,
		table.title.w=table.title.w,table.title.cex=table.title.cex,primera=primera)))

}


######### haplo.glm ############

haplo.model<-function(haplo.base,primera.model=TRUE,y=y,descriptius=FALSE){

haplo.base=NULL;primera.model=TRUE;y=y;descriptius=FALSE


	if (length(table(y))>2){
		family<-"gaussian"
		y<-as.double(y)	
	}
	if (length(table(y))==2){
		family<-"binomial"
		y<-as.integer(y)
		if (max(y,na.rm=TRUE)>1) y<-as.integer(y==1)
	}
	y.value.labels<-names(attributes(dades[,y.label])$value.labels)

	haplo.glm.control=haplo.glm.control(haplo.freq.min=min.freq,haplo.effect=haplo.effect,haplo.base=haplo.base,
				sum.rare.min=haplo.glm.min, haplo.min.info=haplo.glm.min)

	#sense factors ambientals d'ajust
	if (!amb.true | descriptius){ 
		fit<-haplo.glm(y ~ geno,family = family,na.action=na.action,allele.lev=alleles.lev, 
     	      		locus.label=locus.labels,control=haplo.glm.control,x=TRUE)
		form.model=paste("MODEL: ",y.label,"~",paste(locus.labels,collapse="+"),sep="")
	}
	#amb factors ambientals d'ajust
	if (amb.true & !descriptius){
		confusores<-ambientals.labels[1]
		if (length(ambientals.labels)>1){
			for (i in 2:K) confusores<-paste(confusores,"+",ambientals.labels[i],sep="")
		}
		if (ambxgen.true){
			ambxgen<-ambxgen.labels[1]
			if (length(ambxgen.labels)>1){
				for (i in 2:length(ambxgen.labels)) ambxgen<-paste(ambxgen,"+",ambxgen.labels[i],sep="")
			}
			confusores<-paste(confusores,"+",ambxgen,sep="")
		}
		if (ambxamb.true){
			ambxamb<-ambxamb.labels[1]
			if (length(ambxamb.labels)>1){
				for (i in 2:length(ambxamb.labels)) ambxamb<-paste(ambxamb,"+",ambxamb.labels[i],sep="")
			}
			confusores<-paste(confusores,"+",ambxamb,sep="")
		}
		fit<-haplo.glm(formula=as.formula(paste("y~geno+",confusores,sep="")),family = family,na.action=na.action,allele.lev=alleles.lev, 
	      	      locus.label=locus.labels,control=haplo.glm.control,x=TRUE,data=dades[keep,])
		form.model=paste("MODEL: ",y.label,"~",paste(locus.labels,collapse="+"),"+",confusores,sep="")
	}
	
	noms<-c(fit$haplo.names,"geno.base")
	if (sum(noms=="geno.rare")>0) noms<-c(fit$haplo.names[-length(fit$haplo.names)],"geno.base")
	pos<-c(fit$haplo.common,fit$haplo.base)
	haplos<-data.frame(codi=noms,fit$haplo.unique[pos,],freq=round(fit$haplo.freq[pos],5),row.names=1:length(pos))
		if (min(fit$haplo.freq)<min.freq){
		pos.raros<-fit$haplo.rare	
		if (length(pos.raros)==1) haplos.raros<-t(as.matrix(fit$haplo.unique[pos.raros,]))
		if (length(pos.raros)>1) 
			haplos.raros<-data.frame(fit$haplo.unique[pos.raros,],freq=round(fit$haplo.freq[pos.raros],5),row.names=1:length(pos.raros))
	}
	

	if (!descriptius){

		## CANVI DE NOMS DELS COEFICIENTS DE LA REGRESSIÓ PER A LES CATEGÒRIQUES ##

		names.beta<-names(coef(fit))
		names.beta<-gsub("\\(Intercept\\)","Constant",names.beta)

		inter2cat<-grep(":",names.beta)
		inter1cat<-grep("C\\(as.factor",if(length(inter2cat)==0) names.beta else names.beta[-inter2cat])

		if (length(inter1cat)>0){
			for (i in 1:length(inter1cat)){
				name.categ<-unlist(strsplit(unlist(strsplit(names.beta[inter1cat[i]], "\\)")),"\\("))[3]
				value.labels<-attributes(dades[,name.categ])$value.labels
				if (!is.null(value.labels)) value.labels<-value.labels[order(as.double(value.labels))]
				if (is.null(value.labels)){
					value.labels<-as.double(names(table(dades[,categ[i]])))
					names(value.labels)<-value.labels
				}
				xxx<-unlist(strsplit(names.beta[inter1cat[i]], "\\)"))
				xxx<-gsub(" ","",xxx)
				xxx<-unlist(strsplit(xxx, "="))
				xxx[!is.na(as.double(xxx))]<-names(value.labels)[as.double(xxx)[!is.na(as.double(xxx))]]
				xxx<-paste(c(paste(c(xxx[1],xxx[2]),collapse=" ("),paste(c(xxx[3],xxx[4]),collapse=") ")),collapse=" = ")
				xxx<-gsub("C\\(as.factor\\(","",xxx)
				xxx<-gsub(","," ",xxx)
				xxx<-gsub("base ","ref. ",xxx)
				names.beta[inter1cat[i]]<-xxx
			}
		}

		if (length(inter2cat)>0){
			for (j in 1:length(inter2cat)){
				yyy<-unlist(strsplit(names.beta[inter2cat[j]], ":"))
				indexos<-grep("C\\(as.factor",yyy)
				res<-NULL
				for (i in indexos){
					name.categ<-unlist(strsplit(unlist(strsplit(yyy[i], "\\)")),"\\("))[3]
					value.labels<-attributes(dades[,name.categ])$value.labels
					if (!is.null(value.labels)) value.labels<-value.labels[order(as.double(value.labels))]
					if (is.null(value.labels)){
						value.labels<-as.double(names(table(dades[,categ[i]])))
						names(value.labels)<-value.labels
					}
					xxx<-unlist(strsplit(yyy[i], "\\)"))
					xxx<-gsub(" ","",xxx)
					xxx<-unlist(strsplit(xxx, "="))
					xxx[!is.na(as.double(xxx))]<-names(value.labels)[as.double(xxx)[!is.na(as.double(xxx))]]
					xxx<-paste(c(paste(c(xxx[1],xxx[2]),collapse=" ("),paste(c(xxx[3],xxx[4]),collapse=") ")),collapse=" = ")
					xxx<-gsub("C\\(as.factor\\(","",xxx)
					xxx<-gsub(","," ",xxx)
					xxx<-gsub("base ","ref. ",xxx)
					yyy[i]<-xxx
				}
				names.beta[inter2cat[j]]<-paste(yyy,collapse=" : ")
			}
		}

		p<-length(coef(fit))
		coeff<-as.double(coef(fit))
		se<-sqrt(diag(fit$var.mat[1:p,1:p]))
		z<-coeff/se
		p.value<-2*(1-pnorm(abs(z)))
		if (family=="gaussian") {
			coefficients<-data.frame(list(variables=names.beta,coeff=round(coeff,4),se=round(se,4),
				z=round(z,3),pvalue=round(p.value,3)))
			coefficients2<-data.frame(list(variables=names.beta,coeff=coeff,se=se,z=z,pvalue=p.value))
			if (!ambxgen.true){
				windows()
				error.IC<-try(dibuixa.IC(beta=coefficients2$coeff[2:(1+length(fit$haplo.names))],se=coefficients2$se[2:(1+length(fit$haplo.names))],OR=FALSE,names=fit$haplo.names),silent=TRUE)
				if (is.character(error.IC)) winDialog("ok","Algun error ha succeït en el gràfic dels efectes de la regressió")
				if (!is.character(error.IC)){
					winDialog("ok","Arregla la mida i quan estigui bé prem sobre el gràfic")
					locator(1)
					icplot.type<-select.list(c("wmf", "png", "jpeg", "jpg", "bmp", "ps", "pdf"),multiple=FALSE,title="En quin format vols guardar el gràfic?")
					savePlot(filename=paste(path,"/IC plot_",iter.ICplot,sep=""),type=icplot.type)
					dev.off()
				}
			}
		}
		if (family=="binomial"){
			OR.inf<-exp(coeff-1.96*se)
			OR.sup<-exp(coeff+1.96*se)
			coefficients<-data.frame(list(variables=names.beta,coeff=round(coeff,4),se=round(se,4),
				z=round(z,3),pvalue=round(p.value,3)),OR=round(exp(coeff),2),OR.inf=round(OR.inf,2),OR.sup=round(OR.sup,2))
			coefficients[1,c("OR","OR.inf","OR.sup")]<-rep("*",3)
			coefficients2<-data.frame(list(variables=names.beta,coeff=coeff,se=se,z=z,pvalue=p.value,OR=exp(coeff),
				OR.inf=OR.inf,OR.sup=OR.sup))
			coefficients2[1,c("OR","OR.inf","OR.sup")]<-rep(NA,3)
			if (!ambxgen.true){
				windows()
				error.IC<-try(dibuixa.IC(beta=coefficients2$coeff[2:(1+length(fit$haplo.names))],se=coefficients2$se[2:(1+length(fit$haplo.names))],OR=TRUE,names=fit$haplo.names),silent=TRUE)
				if (is.character(error.IC)) winDialog("ok","Algun error ha succeït en el gràfic dels efectes de la regressió")
				if (!is.character(error.IC)){
					winDialog("ok","Arregla la mida i quan estigui bé prem sobre el gràfic")
					locator(1)
					icplot.type<-select.list(c("wmf", "png", "jpeg", "jpg", "bmp", "ps", "pdf"),multiple=FALSE,title="En quin format vols guardar el gràfic?")
					savePlot(filename=paste(path,"/IC plot_",iter.ICplot,sep=""),type=icplot.type)
					dev.off()
				}
			}
		}
		if (family=="binomial") printBanner(paste("Regressió logística amb variable resposta: ",y.label,sep=""))	
		if (family=="gaussian") printBanner(paste("Regressió lineal amb variable resposta: ",y.label,sep=""))
		printBanner("Coefficients de la regressió", banner.width=40, char.perline=80, border="-")
		print(coefficients)
		printBanner("Haplotips no rars", banner.width=40, char.perline=150, border="-")
		print(haplos)
		if (min(fit$haplo.freq)<min.freq){
			printBanner("Haplotips rars", banner.width=40, char.perline=150, border="-")
			print(haplos.raros)
		}
	}

	if (descriptius | !ambxgen.true){
		#estadístic de Wald per veure si tots els coeficients dels haplotips són simultàniament zero <--> no hi ha efecte dels haplotips
		coef.haplo<-as.double(coef(fit)[2:(1+length(fit$haplo.names))])
		var.haplo<-fit$var.mat[2:(1+length(fit$haplo.names)),2:(1+length(fit$haplo.names))]
		if (is.matrix(try(solve(var.haplo),silent=TRUE))){
			W<-coef.haplo%*%solve(var.haplo)%*%coef.haplo
			p.haplo<-1-pchisq(W,length(coef.haplo))
			Wald<-paste("Wald=",round(W,2),", df=",length(coef.haplo),", p-value=",round(p.haplo,5),sep="")
		}
		if (!is.matrix(try(solve(var.haplo),silent=TRUE))) Wald<-"Matriu variàncies numèricament singular"
		if (!descriptius){
			printBanner("test Ho: no hi ha efecte haplotips", banner.width=40, char.perline=150, border="-")
			print(Wald)
			printBanner("",border="")
		}
	}


	if (!descriptius){

		if (winDialog("yesno","Vols validar el model?")=="YES"){

			######  VALIDACIÓ DEL MODEL #######

			carrega.llibreria(c("ROCR","Hmisc","gplots"))
			library(ROCR);library(Hmisc);library(gplots)

			#valors predits i residus (en mitjana per als possibles haplotips) per individu
			fitted<-tapply(fit$fitted*fit$haplo.post.info$post,fit$haplo.post.info$indx,sum)
			resid<-tapply(fit$residuals*fit$haplo.post.info$post,fit$haplo.post.info$indx,sum)  #working residuals: (y-p)/(p*(1-p)), y-mu
			resp<-tapply(fit$y,fit$haplo.post.info$indx,min)
			X<-matrix(NA,length(unique(fit$haplo.post.info$indx)),dim(fit$x)[2])
			for (i in 1:dim(fit$x)[2]) X[,i]<-tapply(fit$x[,i] * fit$haplo.post.info$post, fit$haplo.post.info$indx, sum)
			windows()
			plot(fitted,resid,xlab="fitted values",ylab="working residuals",pch=19,cex=0.1)
			sel<-winDialog("yesno","Vols identificar característiques d'individus?")
			if (sel=="YES") identifica(fitted,resid,as.matrix(cbind(resp,X[,-1])),c(y.label,names.beta[-1]))
			if (family=="binomial")	ROC.plot(resp,fitted)
			printBanner("",border="")		
		}

		if (primera.model) write.table(form.model,file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=FALSE)
		if (!primera.model) write.table(form.model,file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		write.table(paste("Freq. mínima: ",min.freq,sep=""),file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		write.table(paste("Efecte de l'haplotip: ",haplo.effect,sep=""),file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		if (na.action=="na.exclude") max.missings=0
		write.table(paste("Na.action: ",na.action," (màxims de missings en els SNIP's: ",max.missings,")",sep=""),file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		write.table(paste("Filtre: ",sub("filtre<-","",filtre),sep=""),file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		write.table(" ",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		write.table(coefficients2,file=paste(path,"/resultats model",".xls",sep=""),sep="\t",dec=dec.option,na="*",row.names=FALSE,append=TRUE)
		write.table(" ",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		if (!ambxgen.true){
			write.table("test Ho: no hi ha efecte haplotips",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
			write.table(Wald,file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
			write.table(" ",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		}		
		write.table("Haplotips del model",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		write.table(" ",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		write.table(haplos,file=paste(path,"/resultats model",".xls",sep=""),sep="\t",dec=dec.option,row.names=FALSE,append=TRUE)
		write.table(" ",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		if (min(fit$haplo.freq)<min.freq){
			write.table(paste("Haplotips rars: freq. < ",min.freq*100,"%",sep=""),file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
			write.table(" ",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
			write.table(haplos.raros,file=paste(path,"/resultats model",".xls",sep=""),sep="\t",dec=dec.option,row.names=FALSE,append=TRUE)
			write.table(" ",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		}
		write.table(paste("nombre individus analitzats =",max(fit$haplo.post.info[,"indx"])),file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		write.table(" ",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		write.table("Matriu de covariàncies de les estimacions dels coeficients de la regressió:",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		write.table(" ",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		beta<-as.double(coef(fit))
		var<-fit$var.mat[1:length(beta),1:length(beta)]
		rownames(var)<-colnames(var)<-names.beta
		write.table(var,file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=TRUE,append=TRUE,sep="\t",dec=dec.option)
		write.table(" ",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		write.table(" ",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)


		### contrastos lineals sobre els coeficients de la regressió ###

		if (winDialog("yesno","Vols fer contrastos lineals sobre els coeficients de la regressió?")=="YES"){

			tornar=TRUE
			while (tornar){

				tipus.contrast<-menu(choices=c("Coeficients iguals a zero","Altres (combinacions lineals)"),graphics=TRUE,title="Tipus de contrastos")

				if (tipus.contrast==1){
					beta.cont<-select.list(list=names.beta,preselect=NULL,multiple=TRUE,title="Sobre quins coeficients vols fer el constrast?")
					indexos<-which(names.beta%in%beta.cont)
					var.cont<-var[indexos,indexos]
					coef.cont<-beta[indexos]
					W<-coef.cont%*%solve(var.cont)%*%coef.cont
					pvalue.cont=pchisq(W,df=length(coef.cont),lower.tail=FALSE)
					write.table(" ",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
					write.table("Contrastos sobre els coeficients (Ho: són tots simultaniament zero):",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
					write.table(t(as.matrix(beta.cont)),file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE,sep="\t")
					write.table(paste("p.valor global=",pvalue.cont),file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
					write.table(" ",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
					write.table(" ",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
				}

				if (tipus.contrast==2){
					nombre.cont<-as.integer(winDialogString(message="Quants contrastos vols fer?",default="1"))
					K<-matrix(0,nombre.cont,length(beta))
					beta.cont<-list()
					c<-formula.cont<-rep(NA,nombre.cont)
					for (i in 1:nombre.cont){
						beta.cont[[i]]<-select.list(list=names.beta,preselect=NULL,multiple=TRUE,title=paste("Coeficients constrast",i))
						indexos<-which(names.beta%in%beta.cont[[i]])
						formula.cont[i]<-""
						for (j in 1:length(indexos)){
							K[i,indexos[j]]<-as.double(winDialogString(message=paste("Pes per a",names.beta[indexos[j]]),default="1"))
							if (j<length(indexos)) formula.cont[i]<-paste(formula.cont[i],K[i,indexos[j]],"*",names.beta[indexos[j]],"+",sep="")
							if (j==length(indexos)) formula.cont[i]<-paste(formula.cont[i],K[i,indexos[j]],"*",names.beta[indexos[j]],sep="")
						}
						c[i]<-as.integer(winDialogString(message="Quina és la constant del constrast?",default="0"))
						formula.cont[i]<-paste(formula.cont[i],"+",-c,sep="")
					}
	
					Kbeta.c<-K%*%beta-c
					KvarK<-K%*%var%*%t(K)
					W<-t(Kbeta.c)%*%solve(KvarK)%*%Kbeta.c
					pvalue.cont=pchisq(W,df=nombre.cont,lower.tail=FALSE)
					pvalue.glob.t<-data.frame(list(text="pvalor global=",valor=pvalue.cont))

					beta.inf=Kbeta.c-1.96*sqrt(diag(KvarK))
					beta.sup=Kbeta.c+1.96*sqrt(diag(KvarK))
					if (family=="binomial") OR.inf=exp(beta.inf); if (!family=="binomial") OR.inf=rep("*",length(beta.inf))
					if (family=="binomial") OR.sup=exp(beta.sup); if (!family=="binomial") OR.sup=rep("*",length(beta.v))
					if (family=="binomial") OR=exp(Kbeta.c); if (!family=="binomial") OR=rep("*",length(Kbeta.c))
					pvalue.ind=2*pnorm(abs(Kbeta.c/sqrt(diag(KvarK))),lower.tail=FALSE)
					
					IC.cont<-data.frame(list(beta=as.vector(Kbeta.c),sd=sqrt(diag(KvarK)),beta.inf=beta.inf,beta.sup=beta.sup,OR=OR,OR.inf=OR.inf,OR.sup=OR.sup,pvalor=pvalue.ind),row.names=formula.cont)

					write.table(" ",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
					write.table("Contrastos sobre combinacions lineals de coeficients",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
					write.table(" ",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
					write.table(t(as.matrix(c("contrast",names(IC.cont)))),file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE,sep="\t")
					write.table(IC.cont,file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=TRUE,append=TRUE,sep="\t",dec=dec.option)
					write.table(" ",file=paste(path,"/resultats model",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
					write.table(pvalue.glob.t,file=paste(path,"/resultats model",".xls",sep=""),sep="\t",dec=dec.option,col.names=FALSE,row.names=FALSE,append=TRUE)
				}
				tornar=winDialog("yesno","Vols fer més constrastos?")=="YES"
			}
		}
	}

	if (descriptius){
		if (is.matrix(try(solve(var.haplo),silent=TRUE)))
			return(invisible(list(model=fit,W=W,p.haplo=p.haplo,df.haplo=length(coef.haplo))))
		if (!is.matrix(try(solve(var.haplo),silent=TRUE)))
			return(invisible("Error: no s'ha pogut calcular l'estadístic de Wald"))
	}
	if (!descriptius) return(invisible(list(noms=noms,model=fit,formula=form.model)))

}


##### CORBA ROC #####

ROC.plot<-function(y,p){

	y=as.integer(y)
	p=as.double(p)

	#sensibilitat #{>=p i y==1} / # {y==1}
	#especificiatat #{<p i y==0} / # {y==0}
	
	windows()
	par(las=1)
	pred<-prediction(p,y)
	perf<-performance(pred,"tpr","fpr")
	plot(perf,type="l",ylim=c(0,1),xlim=c(0,1),ylab="Sensibility",xlab="1-Especificity",main="ROC curve",xaxs="i",yaxs="i")
	abline(0,1,lty=2)

	aux<-rcorr.cens(p,y)
	AUC<-aux["C Index"]
	SE=aux["S.D."]
	AUC.inf<-AUC-qnorm(0.975)*SE/2
	AUC.sup<-AUC+qnorm(0.975)*SE/2
	text(0.6,0.25,paste("AUC = ",round(AUC,3)," [",round(AUC.inf,3)," ; ",round(AUC.sup,3),"]",sep=""))
	sens<-attributes(perf)$y.values[[1]]
	esp<-1-attributes(perf)$x.values[[1]]
	while (winDialog("yesno","Vols identificar riscos en la curva ROC?")=="YES"){
		coord<-locator(1)
		sens.aprox<-coord$y
		esp.aprox<-1-coord$x
		dif<-(sens-sens.aprox)^2+(esp-esp.aprox)^2
		index<-which(dif==min(dif))  #identifca el punt de la corba més pròxima al punt del pla seleccionat.
		#torna a dibuixar la corba.
		plot(1-esp,sens,type="l",ylim=c(0,1),xlim=c(0,1),axes=FALSE,ylab="Sensibility",xlab="1-Especificity",main="ROC curve",xaxs="i",yaxs="i")
		abline(0,1,lty=2)
		box()
		axis(1,1-esp[index],labels=round(1-esp[index],3))
		axis(2,sens[index],labels=round(sens[index],3))
		text(0.6,0.25,paste("AUC = ",round(AUC,3)," [",round(AUC.inf,3)," ; ",round(AUC.sup,3),"]",sep=""))	
		abline(h=sens[index],v=1-esp[index],col="red",lty=4)
		winDialog("ok",paste("El risc associat és: ",round(100*p[index],2),"%",sep=""))
	}
	plot(1-esp,sens,type="l",ylim=c(0,1),xlim=c(0,1),ylab="Sensibility",xlab="1-Especificity",main="ROC curve",xaxs="i",yaxs="i")
	abline(0,1,lty=2)
	text(0.6,0.25,paste("AUC = ",round(AUC,3)," [",round(AUC.inf,3)," ; ",round(AUC.sup,3),"]",sep=""))	
}

## IDENTIFICA CARACTERÍSTIQUES D'INDIVIDUS SELECCIONATS EN UN GRÀFIC ##

identifica<-function(x,y,X,nom.variables){
	tornar="YES"
	k<-1
	while (tornar=="YES"){
		coord<-locator(1)
		y.aprox<-coord$y
		x.aprox<-coord$x
		rang.x<-(max(x)-min(x))
		rang.y<-(max(y)-min(y))
		dif<-(x/rang.x-x.aprox/rang.x)^2+(y/rang.y-y.aprox/rang.y)^2
		index<-which(dif==min(dif))
		text(x[index],y[index],col="red",k)
		n<-length(index)
		if (n==1) cat("Característiques de l'individu associat al punt",k,"\n\n")
		if (n>1) cat(paste("Característiques dels",n,"individus associats al punt",k,"\n"))
		res<-data.frame(t(X[index[1],]))
		names(res)<-nom.variables
		print(res)
		cat("\n\n\n")
		k<-k+1
		tornar<-winDialog("yesno","Vols identificar un altre punt?")
	}
}


###########################################################################################################################
###########################################################################################################################
################################################ FI FUNCIONS PREVIES ######################################################
###########################################################################################################################
###########################################################################################################################


#################################################################
###################  INICIALITZACIÓ  ############################
#################################################################



## instal·la les llibreries de CRAN-project necessàries per a carregar les altres llibreries ##

carrega.llibreria(c("combinat","gdata","gtools","mvtnorm","gplots"))  ## no sé perquè recaraissos em demana aquestes llibreries....
library(combinat);library(gdata);library(gtools);library(mvtnorm);library(gplots)

## presentació ##

intro.text<-
"Benvingut al programa d'anàlisi d'haplotips.\n\n
Recorda que:\n
    (1) Els missings de les variables excepte els SNP's han de ser SYSMIS ó MISSING\n
    (2) Els SNP's han d'estar codificats de la següent manera:\n
        (2.1) 1 L'homozigot més freqüent\n
        (2.2) 2 L'heterozigot\n
        (2.3) 3 L'homozigot menys freqüent\n
    qualsevol altre valor serà considerat missing\n
    (3) Els SNP's han d'estar etiquetats amb els corresponents al·lels\n
    (4) La variable resposta ha de ser d'un dels següents dos tipus\n
        (4.1) binària (regressió logística), amb codis 1:2 o bé 0:1 ('èxit'=1)\n
        (4.2) contínua (regressió lineal)\n
    (5) Cada anàlisi es fa en UNA base de dades en SPSS:\n
per canviar de base de dades caldrà finalitzar el programa i començar de nou.\n\n
Si ho tens tot correcte, clica sobre el text per a començar.\n
Si no, encara estàs a temps d'arreglar la base de dades.\n\n
    BONA SORT!\n\n"

carrega.llibreria("gplots")
library(gplots)

windows(width=20,height=20,pointsize=10,xpos=10,ypos=0)
textplot(intro.text,halign="center",valign="center",font=2,col="dark red",cex=0.8)

invisible(locator(1))
invisible(dev.off())

#### lectura de dades ####

## obre una finestra per anar a buscar la base de dades de spss
file<-choose.files("*.sav",caption="Escull l'arxiu de dades en SPSS",multi=FALSE)

carrega.llibreria("foreign")
library(foreign)

## carrega la rutina read.spss4.r ##
source(file.path(RutinesLocals,"read.spss4.r"))

dades<-read.spss4(file)
names(dades)<-toupper(names(dades))

attach(dades,warn.conflicts=FALSE)


tornar=TRUE
primera.model=TRUE
primera.freq=TRUE
primera.descriptives=TRUE
iter.barplot=1
iter.linkplot=1
iter.ICplot=1

canvi.resposta=TRUE
canvi.polimorfismes=TRUE
canvi.grup=TRUE
canvi.ambientals=TRUE
canvi.na.action=TRUE
canvi.haplo.effect=TRUE
canvi.min.freq=TRUE
canvi.em.control=TRUE
canvi.haplo.glm.min=TRUE
canvi.descriptius=TRUE
canvi.filtre=TRUE

correct.path=TRUE
path=dirname(file)
path=paste(path,"/resultats haplotips ",sub(".sav","",basename(file)),sep="")
path=winDialogString("On vols guardar els resultats dels model/s?",path)
if (nchar(path)>250-21)
	if (winDialog("yesno",paste("Advertència: el directori té",nchar(path),"caràcters\nVols canviar la carpeta?"))=="YES")
		path=winDialogString("Nova carpeta dels resultats",path)
dir.create(path)  # crea una carpeta on van a parar els resultats que es diu "resultats haplotips"
dec.option=winDialogString("Quin separador vols pels decimals en Excell?",",")

resp.true=FALSE
grup.true=FALSE
amb.true=FALSE
categ.true=FALSE
ambxamb.true=FALSE
ambxgen.true=FALSE

filtre<-NULL

################################################################################################################################
################################################################################################################################
#################################################### MAIN PROGRAM ##############################################################
################################################################################################################################
################################################################################################################################


while (tornar){


	##### ENTRADA INTERACTIVA DE LES VARIABLES (I TIPUS) #####


	### filtre ###

	if (canvi.filtre){  # keep.filtre
		filtre<-winDialogString("Escriu el filtre",ifelse(is.null(filtre) || filtre=="" || length(filtre)==0,"None",filtre))
		if (filtre=="None" | is.null(filtre) | filtre==""){
			keep.filtre=rep(TRUE,nrow(dades))   # els seleccionem a priori a tots.
			filtre<-"None"
		}
		if (!filtre=="None"){
			filtre<-toupper(filtre)		
			filtre<-sub("AND","&",filtre)
			filtre<-sub("OR","|",filtre)
			filtre<-sub("=","==",filtre)   ##@
			write(paste("keep.filtre<-",filtre,sep=""),"xxx.r")
			#file.show("xxx.r")
			source("xxx.r")
			file.remove("xxx.r")
		}
		keep.filtre<-ifelse(is.na(keep.filtre),FALSE,keep.filtre)
	}	
	

	### polimorfismes ###

	if (canvi.polimorfismes){
		nom.variables<-names(dades)
		polis.correctes=TRUE
		N=1
		while (N<2){
			locus.labels<-select.list(nom.variables,title="Polimorfismes",multiple=TRUE)
			N<-length(locus.labels)
			if (N<2) winDialog(type = "ok", "Ei!, que com a mínim ha d'haver dos polimorfismes!")
		}
		alleles.lev<-list()
		for (i in 1:N){
			aux<-names(attributes(dades[,locus.labels[i]])$value.labels)
			alleles.lev[[i]]<-c(substr(aux[as.integer(attributes(dades[,locus.labels[i]])$value.labels)==1],1,1),
						substr(aux[as.integer(attributes(dades[,locus.labels[i]])$value.labels)==3],1,1))
		}
	}
	if (canvi.na.action | canvi.filtre){
		
		### individus a seleccionar ###  IMPORTANT: ENTÉN QUE ELS 9 SÓN MISSINGS.
		if (canvi.na.action) na.action<-select.list(c("na.exclude","na.geno.keep"),title="NA action")
		if (na.action=="na.geno.keep"){
			max.missings<-menu(as.character(1:(N-1)),graphics=TRUE,"Màx missings en SNIP's")
			keep.geno<-as.integer(apply(is.na(dades[,locus.labels]) | dades[,locus.labels]==9,1,sum))<=max.missings
		}
		if (!na.action=="na.geno.keep") keep.geno<-!apply(is.na(dades[,locus.labels]) | (!dades[,locus.labels]==1 & !dades[,locus.labels]==2 & !dades[,locus.labels]==3),1,any)

		keep=keep.filtre & keep.geno  ## redifinició de keep

		polis<-dades[keep,locus.labels]

		## etiquetes dels al.lels
		
		#codificació dels polimorfismes en al.lels
		geno<-matrix(NA,nrow(polis),(2*N))
		for (i in 1:N){  # bucle per a cada polimorfisme
			geno[polis[,i]==1,(2*i-1)]<-1
			geno[polis[,i]==2,(2*i-1)]<-1
			geno[polis[,i]==3,(2*i-1)]<-2
			geno[polis[,i]==1,(2*i)]<-1
			geno[polis[,i]==2,(2*i)]<-2
			geno[polis[,i]==3,(2*i)]<-2
		}
		
		carrega.llibreria("haplo.stats")
		library(haplo.stats)
		geno <- setupGeno(geno, miss.val=NA)
		
	}

	### variables resposta ###
		# la variable resposta només pot ser, 
		#
		#	(a)	contínua (normal), 
		#	(b)	dicotòmica 
		#		amb dues possibilitats o codificacions:
		#		1-sí, 0-no
		#		1-sí, 2-no

	if (canvi.resposta){
		nom.variables<-nom.variables[!nom.variables%in%locus.labels]
		y.label<-select.list(c("None",nom.variables),"None",title="Escull la variable resposta")
		y.label<-ifelse(y.label=="None","",y.label)		
		resp.true=!y.label==""
		if (resp.true) y<-dades[keep,y.label]
	}
	if (!canvi.resposta & resp.true & (canvi.na.action | canvi.filtre)) y<-dades[keep,y.label]


	### variable grup ###

	if (canvi.grup){
		grup.label<-select.list(c("None",nom.variables),"None",title="Var. grup")
		grup.label<-ifelse(grup.label=="None","",grup.label)
		grup.true=!grup.label==""
		if (grup.true){
			grup<-dades[,grup.label]
			if (!is.null(attributes(grup))){
				grup.value.labels<-names(attributes(grup)$value.labels)
				grup.values<-as.double(attributes(grup)$value.labels)
			}
			if (is.null(attributes(grup))){
				grup.value.labels<-names(table(grup))
				grup.values<-as.integer(grup.value.labels)
			}
			order<-order(grup.values)
			grup.values<-sort(grup.values)
			grup.value.labels<-grup.value.labels[order]
			grup<-dades[keep,grup.label]
		}
	}
	if (!canvi.grup & grup.true & (canvi.na.action | canvi.filtre)) grup<-dades[keep,grup.label]

	
	### variables ambientals ###

	if (canvi.ambientals){
	
		if (resp.true){
			ambientals.labels<-select.list(c("None",nom.variables[!nom.variables%in%y.label]),"None",title="ambientals",multiple = TRUE)
			ambientals.labels<-ifelse(ambientals.labels=="None","",ambientals.labels)
			ambientals.labels.aux<-ambientals.labels  ## sense el as.factor.
			amb.true=!ambientals.labels[1]==""
			if (amb.true){
				categ<-select.list(c("None",ambientals.labels),"None",title="amb categor",multiple = TRUE)
				categ<-ifelse(categ=="None","",categ)
				categ.true=!categ[1]==""
				if (categ.true){
					for (i in 1:length(categ)){

						value.labels<-attributes(dades[,categ[i]])$value.labels		
						t<-as.data.frame(table(dades[,categ[i]]))
						names(t)<-c("values","labels")	

						if (!is.null(value.labels)){
							value.labels<-data.frame(list(labels=names(value.labels),values=as.integer(value.labels)))
							value.labels<-value.labels[order(value.labels$values),]
						}

						if (is.null(value.labels)){
							value.labels<-t
							t[,2]<-as.character(t[,1])
						}

						base=menu(as.character(value.labels[,1]),graphics=TRUE, title=paste("Ref",categ[i]))
						if (base==0) base=1
						index<-categ[i]==ambientals.labels
						ambientals.labels[index]<-paste("C(as.factor(",ambientals.labels[index],"),base=",base,")",sep="")
					}
				}
			}
		}

		K<-0
		if (amb.true){
		
			#interaccions variables ambientals amb haplotips

			ambxgen.labels<-select.list(c("None",ambientals.labels),"None",title="amb x gen",multiple = TRUE)
			ambxgen.labels<-ifelse(ambxgen.labels=="None","",ambxgen.labels)
			ambxgen.true=!ambxgen.labels[1]==""
			if (ambxgen.true){
				for (i in 1:length(ambxgen.labels)) ambxgen.labels[i]<-paste(ambxgen.labels[i],":geno",sep="")
			}

			#interaccions variables ambientals entre elles, d'ordre dos.

			K<-length(ambientals.labels)
			if (K>1){
				ambxamb.labels.list<-rep(NA,(K*(K-1)/2))
				comptador<-1
				for (i in 1:(K-1)){
					for (j in (i+1):K){		
						ambxamb.labels.list[comptador]<-paste(ambientals.labels[i],":",ambientals.labels[j],sep="")
						comptador<-comptador+1
					}
				}	
				ambxamb.labels<-select.list(c("None",ambxamb.labels.list),"None",title="amb x amb",multiple = TRUE)
				ambxamb.labels<-ifelse(ambxamb.labels=="None","",ambxamb.labels)		
				ambxamb.true=!ambxamb.labels[1]==""
			}
		}
	}


	### variables de control ###

	if (canvi.haplo.effect) haplo.effect<-select.list(c("add","dominant","recessive"),title="Haplo. effect")
	if (canvi.min.freq){
		min.freq<-winDialogString(message="Introdueix freq. mínima dels haplotips (en tant per 1: ex. 0.02 si es vol el 2%)", default="")
		min.freq<-ifelse(is.null(min.freq) | min.freq=="",0.01,as.double(min.freq))
	}
	if (canvi.em.control){
		em.control<-select.list(c("defecte","EM"),"defecte",title="EM control")
		if (em.control=="") em.control="defecte"
	}
	if (canvi.haplo.glm.min) haplo.glm.min=as.double(winDialogString(message="Introdueix freq. mínima dels haplotips en el model", default="0.001"))



	#############################################
	#########  LINKAGE DESEQUILIBRIUM  ##########
	#############################################

	if (canvi.polimorfismes | canvi.filtre){ # no efecte el canvi del na.action, només els canvis en els polimorfismes
	

		carrega.llibreria("genetics")
		library(genetics)
	
		polis.LD<-polis

		for (i in 1:N){
			polis.LD[,i]<-ifelse(polis.LD[,i]==1,paste(alleles.lev[[i]][1],"/",alleles.lev[[i]][1],sep=""),
				ifelse(polis.LD[,i]==2,paste(alleles.lev[[i]][1],"/",alleles.lev[[i]][2],sep=""),
				ifelse(polis.LD[,i]==3,paste(alleles.lev[[i]][2],"/",alleles.lev[[i]][2],sep=""),NA
				)))
			polis.LD[,i]<-genotype(polis.LD[,i])
		}
	
		polis.LD<-makeGenotypes(polis.LD)

		LD<-LD(polis.LD)

		if (N>2){
			windows(width = 14, height = 7)
			par(mfrow=c(1,2),las=1,cex.axis=0.8,cex.lab=1,mgp=c(3,0.6,0))
			LDplot(LD)
			dist<-LD$"D'"
			dist[is.na(dist)]<-0
			dist<-dist+t(dist)
			diag(dist)<-1
			dist<-1-dist
			poli.pos<-try(isoMDS(dist,tol=1e-7),silent=TRUE)
			if (!is.character(poli.pos)){
				plot(poli.pos$points, type = "n",xlab="",ylab="",asp=1)
				text(poli.pos$points, labels = row.names(poli.pos$points),col=1:length(row.names(poli.pos$points)))
				title("Representació gràfica dels\npolimorfismes segons la distància D'")
			}
			barplot.type<-select.list(c("wmf", "png", "jpeg", "jpg", "bmp", "ps", "pdf"),multiple=FALSE,title="En quin format vols guardar el gràfic?")
			savePlot(filename=paste(path,"/bar plot_",iter.barplot,sep=""),type=barplot.type)
			iter.linkplot=iter.linkplot+1
			dev.off()
		}

		printBanner("Linkage Desequilibrium")
		printBanner(paste("Filtre: ",sub("filtre<-","",filtre),sep=""), banner.width=40, char.perline=30, border="-")
		printBanner("N", banner.width=40, char.perline=30, border="-")
		print(LD$"n")
		printBanner("D prima", banner.width=40, char.perline=30, border="-")
		print(round(LD$"D'",3))
		printBanner("X^2", banner.width=40, char.perline=30, border="-")
		print(round(LD$"X^2",))
		printBanner("p-valor", banner.width=40, char.perline=30, border="-")
		print(round(LD$"P-value",5))
		if (N>2) if (is.character(poli.pos)) print(paste("No es poden representar els polimorfismes en el pla: ",poli.pos[[1]])) 
		printBanner("",border="")

		if ("package:genetics"%in%search()) detach("package:genetics") # si no es solapa amb la funció locus i dóna problemes en en setupGeno.

	}



	########################################################################
	### freqüencies haplotípiques sense necessitat de construir cap model ##
	######### amb l'algorisme EM: veure apunts Víctor Moreno ###############
	########################################################################

	if (canvi.grup | canvi.polimorfismes | canvi.em.control | canvi.na.action | canvi.filtre){
	
		if (em.control=="EM") control<-haplo.em.control(insert.batch.size=N,min.posterior=0)
		if (!em.control=="EM") control<-haplo.em.control()

		if (!grup.true) fit.em<-haplo.em(geno=geno, locus.label=locus.labels, miss.val=NA,control=control)

		if (grup.true){

			grup.em<-grup[!is.na(grup)]
			geno.em<-geno[!is.na(grup),]
			#per poder comparar hem de treure els individus amb missing en el grup
			fit.em<-haplo.em(geno=geno.em, locus.label=locus.labels, miss.val=NA,control=control) 

			fit.em.grup<-list()
			lnlike<-rep(NA,length(grup.values))
			for (g in 1:length(grup.values)){
				fit.em.grup[[g]]<-haplo.em(geno=geno.em[grup.em==grup.values[g],],locus.label=locus.labels,miss.val=NA,control=control)
				lnlike[g]<-fit.em.grup[[g]]$lnlike
			}
			LRT.obs<- sum(lnlike)-fit.em$lnlike

			if (winDialog("yesno","Vols testar igualtat de frequències?")=="YES"){
				
				#############################
				##### EMPIRICAL P-VALUE #####
				#############################

				tornar.sim=TRUE
				while(tornar.sim){
					set.seed(492)
					max.iter<-as.integer(winDialogString("Número iteracions", "1000"))
					LRT.sim<-rep(NA,max.iter)
					for (iter in 1:max.iter){
						fit.em.grup.sim<-list()
						lnlike.sim<-rep(NA,length(grup.values))
						grup.sim<-sample(grup.em)
						for (g in 1:length(grup.values)){
							fit.em.grup.sim[[g]]<-haplo.em(geno.em[grup.sim==grup.values[g],],locus.label=locus.labels,miss.val=NA,control=control)
							lnlike.sim[g]<-fit.em.grup.sim[[g]]$lnlike
						}
						LRT.sim[iter]<-sum(lnlike.sim)-fit.em$lnlike
					}
					plot(density(LRT.sim),main=paste("Empirical LRT Distribution for ",max.iter," Permutation",sep=""),xlab="log-Likelihood Ratio Values",ylab="density",cex.axis=0.8,xlim=c(0,max(LRT.sim)*1.2),xaxs="i",yaxs="i")
					polygon(c(density(LRT.sim)$x[density(LRT.sim)$x>=LRT.obs][1],density(LRT.sim)$x[density(LRT.sim)$x>=LRT.obs]),c(density(LRT.sim)$y[1],density(LRT.sim)$y[density(LRT.sim)$x>=LRT.obs]),col="grey",border=NA)
					legend("topright",paste("p-value = ",mean(LRT.obs<=LRT.sim),sep=""),inset=0.1,bty="n")
					par(font=3,xpd=NA)
					legend("center","recorda que p-value és l'área que està\n en gris i que té el MATEIX significat\nque el p-valor de tota la vida",bty="n",text.col="green",cex=1.5)
					par(font=1,xpd=TRUE)
					tornar.sim<-winDialog("yesno","Vols tornar-hi (canviant número iteracions)?")=="YES"
				}	
			}
		}
		
		hap.freq<-list()
		printBanner("Freqüències haplotípiques (algorisme EM)")
		printBanner(paste("Filtre: ",sub("filtre<-","",filtre)),banner.width=40, char.perline=30, border="-")
		nom.hap<-fit.em$haplotype
		for (i in 1:N) nom.hap[,i]<-ifelse(nom.hap[,i]==1,alleles.lev[[i]][1],alleles.lev[[i]][2])
		ordre<-order(fit.em$hap.prob)
		hap.freq[[1]]<-data.frame(nom.hap[rev(ordre),],freq=sort(fit.em$hap.prob,decreasing = TRUE),row.names=1:length(ordre))
		taula.freq<-hap.freq[[1]]
		names(taula.freq)<-c(locus.labels,"all")
		if (grup.true){
			for (g in 1:length(grup.values)){
				nom.hap<-fit.em.grup[[g]]$haplotype
				for (i in 1:N) nom.hap[,i]<-ifelse(nom.hap[,i]==1,alleles.lev[[i]][1],alleles.lev[[i]][2])
				ordre<-order(fit.em.grup[[g]]$hap.prob)
				hap.freq[[g+1]]<-data.frame(nom.hap[rev(ordre),],freq=sort(fit.em.grup[[g]]$hap.prob,decreasing = TRUE),row.names=1:length(ordre))
				taula.freq<-merge(taula.freq,hap.freq[[g+1]],by=locus.labels,all=TRUE)
			}
			names(taula.freq)<-c(locus.labels,"all",grup.value.labels)
		}
		
		taula.freq<-taula.freq[order(taula.freq$all,decreasing=TRUE),]
		
		taula.freq2<-taula.freq
		freq<-taula.freq[,(length(locus.labels)+1):dim(taula.freq2)[2]]
		freq<-as.matrix(freq)
		if (is.vector(freq)) freq<-ifelse(is.na(freq),0,freq)
		if (grup.true) for (i in 1:dim(freq)[1]) for (j in 1:(length(grup.value.labels)+1)) if (is.na(freq[i,j])) freq[i,j]<-0
		taula.freq[,(length(locus.labels)+1):dim(taula.freq)[2]]<-freq
		taula.freq2[,(length(locus.labels)+1):dim(taula.freq2)[2]]<-round(freq,4)
		print(taula.freq2)
		printBanner("",border="")
		if (primera.freq) write.table(ifelse(grup.true,grup.label,"all"),file=paste(path,"/resultats frequencies",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=FALSE)
		if (!primera.freq) write.table(ifelse(grup.true,grup.label,"all"),file=paste(path,"/resultats frequencies",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		write.table(" ",file=paste(path,"/resultats frequencies",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		write.table(paste("Número d'individus en total = ",sum(keep),sep=""),file=paste(path,"/resultats frequencies",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		if (na.action=="na.exclude") max.missings=0
		write.table(paste("Na.action: ",na.action," (màxims de missings en els SNIP's: ",max.missings,")",sep=""),file=paste(path,"/resultats frequencies",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		write.table(paste("Filtre: ",sub("filtre<-","",filtre),sep=""),file=paste(path,"/resultats frequencies",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		write.table(" ",file=paste(path,"/resultats frequencies",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		write.table(taula.freq,file=paste(path,"/resultats frequencies",".xls",sep=""),col.names=TRUE,row.names=FALSE,append=TRUE,dec=dec.option,sep="\t")
		write.table(" ",file=paste(path,"/resultats frequencies",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		write.table(" ",file=paste(path,"/resultats frequencies",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
		primera.freq=FALSE
	
		
		if (grup.true){

			###########################################################
			#### gràfic de barres de les freqüències haplotípiques ####
			###########################################################

			if (winDialog("yesno","Vols un gràfic de barres de les freqüencies dels haplotips?")=="YES"){

				carrega.llibreria("gplots")
				library(gplots)

				freq<-as.matrix(freq)
				freq<-freq[,2:ncol(freq)]
				colnames(freq)<-grup.value.labels
		
				k=0
				windows(600,600)
				triat=FALSE
				while(!triat){
					tipus<-menu(paste("tipus",1:4),graphics=TRUE,"Quin tipus vols (OK per veure'l)?")
					if (tipus>0){
						if (k>0) dev.off()
						bar.plot(tipus)
						k=k+1					
						triat<-winDialog("yesno","Et va bé aquest?")=="YES"
					}
					if (tipus==0) triat=TRUE
				}
				if (tipus>0){
					plot.acabat=!winDialog("yesno","Vols canviar alguna cosa del gràfic?")=="YES"
					#param per defecte de bar.plot
					title=grup.label
					cex.legend=1
					cex.table=1
					cex.screen=0
					dens=20
					color=gray(0:(nrow(freq)-1)/nrow(freq))
					legend.coord=NULL
					table.intersp=1
					legend.intersp=1
					bar.space=1
					cex.values=0.9
					xtext=NULL
					coord.xtext=NULL
					primera=FALSE
					table.title.w=-3
					table.title.cex=1

					while (!plot.acabat){

						if (tipus==1) sel<-select.list(c("Colors","Títol","Títol eix x","Densitat","Separació de les barres"),"Què vols canviar?")
						if (tipus==2) sel<-select.list(c("Colors","Títol","Títol eix x","Densitat","Separació de les barres","Mida dels valors"),"Què vols canviar?")
						if (tipus==3) sel<-select.list(c("Colors","Títol","Títol eix x","Mida llegenda","Posició llegenda","Mida taula","Llargada taula","Densitat","Separació de les barres","Separació files taula","Separació files llegenda","Alçada títol taula","Mida títol taula"),"Què vols canviar?")
						if (tipus==4) sel<-select.list(c("Colors","Títol","Títol eix x","Mida llegenda","Posició llegenda","Mida taula","Llargada taula","Densitat","Separació de les barres","Separació files taula","Separació files llegenda","Mida dels valors","Alçada títol taula","Mida títol taula"),"Què vols canviar?")


						if (sel=="Títol"){
							aux<-bar.plot(tipus,title=winDialogString("Escriu el títol",""),cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,bar.space=bar.space,cex.values=cex.values,xtext=xtext,table.intersp=table.intersp,legend.intersp=legend.intersp,coord.xtext=coord.xtext,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
							title=aux$title
						}
						if (sel=="Colors"){
							dens=NULL
							aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=NULL,bar.space=bar.space,cex.values=cex.values,xtext=xtext,table.intersp=table.intersp,legend.intersp=legend.intersp,coord.xtext=coord.xtext,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
							color=aux$color
						}
						if (sel=="Posició llegenda"){
							aux.cex.screen<-ifelse(cex.screen==0,0.5,cex.screen)
							A<-matrix(c(1,aux.cex.screen,1,1),2,2,byrow=TRUE)
							b<-c(0,1)
							coef<-solve(A,b)
							mousemove <- function(buttons, x, y){
									y.prima<-coef[1]+coef[2]*y
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,bar.space=bar.space,cex.values=cex.values,xtext=xtext,coord.xtext=coord.xtext,table.intersp=table.intersp,legend.intersp=legend.intersp,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=convert(x,y.prima),primera=primera)
									return(NULL)
							}
							mousedown <- function(buttons, x, y){
									y.prima<-coef[1]+coef[2]*y
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,bar.space=bar.space,cex.values=cex.values,xtext=xtext,coord.xtext=coord.xtext,table.intersp=table.intersp,legend.intersp=legend.intersp,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=convert(x,y.prima),primera=primera)
									return(aux$legend.coord)
							}
							legend.coord=getGraphicsEvent("Click on left button to quit",onMouseMove=mousemove,onMouseDown=mousedown)
							#bar.plot(3,legend.coord=legend.coord)
						}
						if (sel=="Títol eix x"){
							aux.cex.screen<-ifelse(cex.screen==0,0.5,cex.screen)
							A<-matrix(c(1,aux.cex.screen,1,1),2,2,byrow=TRUE)
							b<-c(0,1)
							coef<-solve(A,b)
							xtext<-winDialogString("Escriu el títol","")
							mousemove <- function(buttons, x, y){
									y.prima<-coef[1]+coef[2]*y
									#y.prima<-ifelse(y.prima<0,0,y.prima)
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,bar.space=bar.space,cex.values=cex.values,xtext=xtext,coord.xtext=convert(x,y.prima),table.intersp=table.intersp,legend.intersp=legend.intersp,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(NULL)
							}
							mousedown <- function(buttons, x, y){
									y.prima<-coef[1]+coef[2]*y
									#y.prima<-ifelse(y.prima<0,0,y.prima)
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,bar.space=bar.space,cex.values=cex.values,xtext=xtext,coord.xtext=convert(x,y.prima),table.intersp=table.intersp,legend.intersp=legend.intersp,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(aux$coord.xtext)
							}
							coord.xtext=getGraphicsEvent("Click on left button to quit",onMouseMove=mousemove,onMouseDown=mousedown)
						}
						if (sel=="Separació de les barres"){
							mousemove <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,bar.space=x*3,cex.values=cex.values,xtext=xtext,coord.xtext=coord.xtext,table.intersp=table.intersp,legend.intersp=legend.intersp,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(NULL)
							}
							mousedown <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,bar.space=x*3,cex.values=cex.values,xtext=xtext,coord.xtext=coord.xtext,table.intersp=table.intersp,legend.intersp=legend.intersp,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(aux$bar.space)
							}
							bar.space=getGraphicsEvent("Click on left button to quit",onMouseMove=mousemove,onMouseDown=mousedown)
						}
						if (sel=="Separació files llegenda"){
							mousemove <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,table.intersp=table.intersp,legend.intersp=y*5,bar.space=bar.space,cex.values=cex.values,xtext=xtext,coord.xtext=coord.xtext,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(NULL)
							}
							mousedown <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,table.intersp=table.intersp,legend.intersp=y*5,bar.space=bar.space,cex.values=cex.values,xtext=xtext,coord.xtext=coord.xtext,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(aux$legend.intersp)
							}
							legend.intersp=getGraphicsEvent("Click on left button to quit",onMouseMove=mousemove,onMouseDown=mousedown)
						}
						if (sel=="Separació files taula"){
							mousemove <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,legend.intersp=legend.intersp,table.intersp=y*3,bar.space=bar.space,cex.values=cex.values,xtext=xtext,coord.xtext=coord.xtext,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(NULL)
							}
							mousedown <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,legend.intersp=legend.intersp,table.intersp=y*3,bar.space=bar.space,cex.values=cex.values,xtext=xtext,coord.xtext=coord.xtext,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(aux$table.intersp)
							}
							table.intersp=getGraphicsEvent("Click on left button to quit",onMouseMove=mousemove,onMouseDown=mousedown)
						}
						if (sel=="Mida llegenda"){
							mousemove <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=y*3,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,bar.space=bar.space,cex.values=cex.values,xtext=xtext,table.intersp=table.intersp,coord.xtext=coord.xtext,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(NULL)
							}
							mousedown <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=y*3,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,bar.space=bar.space,cex.values=cex.values,xtext=xtext,table.intersp=table.intersp,coord.xtext=coord.xtext,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(aux$cex.legend)
							}
							cex.legend=getGraphicsEvent("Click on left button to quit",onMouseMove=mousemove,onMouseDown=mousedown)
						}
						if (sel=="Llargada taula"){ 
							mousemove <- function(buttons, x, y){
									if (y>0.9) y<-0.9
									if (y<0.2) y<-0.2
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=y,dens=dens,color=color,bar.space=bar.space,cex.values=cex.values,xtext=xtext,table.intersp=table.intersp,coord.xtext=coord.xtext,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera,canvi.cex.screen=TRUE)
									return(NULL)
							}
							mousedown <- function(buttons, x, y){
									if (y>0.9) y<-0.9
									if (y<0.2) y<-0.2
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=y,dens=dens,color=color,bar.space=bar.space,cex.values=cex.values,xtext=xtext,table.intersp=table.intersp,coord.xtext=coord.xtext,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(aux$cex.screen)
							}
							cex.screen=getGraphicsEvent("Click on left button to quit",onMouseMove=mousemove,onMouseDown=mousedown)
						}
						if (sel=="Mida taula"){
							mousemove <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=3*y,cex.screen=cex.screen,dens=dens,color=color,bar.space=bar.space,cex.values=cex.values,xtext=xtext,table.intersp=table.intersp,coord.xtext=coord.xtext,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(NULL)
							}
							mousedown <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=3*y,cex.screen=cex.screen,dens=dens,color=color,bar.space=bar.space,cex.values=cex.values,xtext=xtext,table.intersp=table.intersp,coord.xtext=coord.xtext,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(aux$cex.table)
							}
							cex.table=getGraphicsEvent("Click on left button to quit",onMouseMove=mousemove,onMouseDown=mousedown)
						}
						if (sel=="Densitat"){
							mousemove <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=y*50,color=color,bar.space=bar.space,cex.values=cex.values,xtext=xtext,table.intersp=table.intersp,coord.xtext=coord.xtext,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(NULL)
							}
							mousedown <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=y*50,color=color,bar.space=bar.space,cex.values=cex.values,xtext=xtext,table.intersp=table.intersp,coord.xtext=coord.xtext,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(aux$dens)
							}
							dens=getGraphicsEvent("Click on left button to quit",onMouseMove=mousemove,onMouseDown=mousedown)
						}
						if (sel=="Mida dels valors"){
							mousemove <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,bar.space=bar.space,cex.values=y*3,xtext=xtext,coord.xtext=coord.xtext,table.intersp=table.intersp,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(NULL)
							}
							mousedown <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,bar.space=bar.space,cex.values=y*3,xtext=xtext,coord.xtext=coord.xtext,table.intersp=table.intersp,table.title.w=table.title.w,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(aux$cex.values)
							}
							cex.values=getGraphicsEvent("Click on left button to quit",onMouseMove=mousemove,onMouseDown=mousedown)
						}
						if (sel=="Alçada títol taula"){
							mousemove <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,bar.space=bar.space,cex.values=cex.values,xtext=xtext,coord.xtext=coord.xtext,table.intersp=table.intersp,table.title.w=y*10-10,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(NULL)
							}
							mousedown <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,bar.space=bar.space,cex.values=cex.values,xtext=xtext,coord.xtext=coord.xtext,table.intersp=table.intersp,table.title.w=y*10-10,table.title.cex=table.title.cex,legend.coord=legend.coord,primera=primera)
									return(aux$table.title.w)
							}
							table.title.w=getGraphicsEvent("Click on left button to quit",onMouseMove=mousemove,onMouseDown=mousedown)
						}
						if (sel=="Mida títol taula"){
							mousemove <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,bar.space=bar.space,cex.values=cex.values,xtext=xtext,coord.xtext=coord.xtext,table.intersp=table.intersp,table.title.w=table.title.w,table.title.cex=y*2,legend.coord=legend.coord,primera=primera)
									return(NULL)
							}
							mousedown <- function(buttons, x, y){
									aux<-bar.plot(tipus,title=title,cex.legend=cex.legend,cex.table=cex.table,cex.screen=cex.screen,dens=dens,color=color,bar.space=bar.space,cex.values=cex.values,xtext=xtext,coord.xtext=coord.xtext,table.intersp=table.intersp,table.title.w=table.title.w,table.title.cex=y*2,legend.coord=legend.coord,primera=primera)
									return(aux$table.title.cex)
							}
							table.title.cex=getGraphicsEvent("Click on left button to quit",onMouseMove=mousemove,onMouseDown=mousedown)
						}
					
						plot.acabat=!winDialog("yesno","Vols canviar alguna cosa més del gràfic?")=="YES"						
					}
					barplot.type<-select.list(c("wmf", "png", "jpeg", "jpg", "bmp", "ps", "pdf"),multiple=FALSE,title="En quin format vols guardar el gràfic?")
					savePlot(filename=paste(path,"/bar plot_",iter.barplot,sep=""),type=barplot.type)
					iter.barplot=iter.barplot+1
					dev.off()
				}
			}	
		}
	}



	##################################################################################################
	### anàlisi d'haplotips sota un model (amb variable resposta, i possibles variables ambientals)###
	##################################################################################################


	if (resp.true & (canvi.resposta | canvi.polimorfismes | canvi.ambientals | canvi.min.freq | canvi.haplo.effect | canvi.haplo.glm.min | canvi.na.action | canvi.filtre)){

		fit.glm<-haplo.model(haplo.base=NULL,primera.model,y=y) ; if (!ambxgen.true) iter.ICplot=iter.ICplot+1
		print("")
		print(paste("nombre individus analitzats =",max(fit.glm$model$haplo.post.info[,"indx"])))
		primera.model=FALSE
		geno.sel="geno.base"
		canvi.haplo.base<-winDialog("yesno", "Vols canviar haplotip de referència")=="YES"
		while (canvi.haplo.base & sum(!fit.glm$noms%in%geno.sel)>0){
			geno.sel<-c(geno.sel,sel<-select.list(fit.glm$noms[!fit.glm$noms%in%geno.sel],title="Ref. haplotip"))			
			haplo.base<-as.integer(substr(sel,6,nchar(sel)))
			fit.glm<-haplo.model(haplo.base,primera.model,y=y) ; if (!ambxgen.true) iter.ICplot=iter.ICplot+1
			print("")
			print(paste("nombre individus analitzats =",max(fit.glm$model$haplo.post.info[,"indx"])))
			if (sum(!fit.glm$noms%in%geno.sel)>0) canvi.haplo.base<-winDialog("yesno", "Vols canviar haplotip de referència")=="YES"
		}

		if (winDialog("yesno", "Vols guardar els valors pronosticats?")=="YES"){
			options(digits=10)
			ind.label<-select.list(names(dades),title="Identificador de l'individu")
			if (ambientals.labels==""){
				keep.all=keep & !is.na(dades[,y.label])
				dades.all<-dades[keep.all,c(ind.label,y.label,locus.labels)]
			}
			if (!ambientals.labels==""){
				keep.all=keep & !is.na(dades[,y.label]) & !apply(is.na(as.matrix(dades[,ambientals.labels.aux])),1,any)
				dades.all<-dades[keep.all,c(ind.label,y.label,ambientals.labels.aux,locus.labels)]
			}
			dades.all<-data.frame(list(indx=1:sum(keep.all),dades.all))
			info.model<-cbind(fit.glm$model$haplo.post.info,fit.glm$model$fitted.values)
			info.model<-info.model[-4]
			names(info.model)<-c("indx","hap1","hap2","hap.prob","fitted")
			dades.all<-merge(dades.all,info.model,by="indx",sort=FALSE)			
			#ULL! matxaca l'arxiu anterior
			write.table(dades.all,file=paste(path,"/valors ajustats",".xls",sep=""),col.names=TRUE,row.names=FALSE,append=FALSE,dec=dec.option,sep="\t")
		}
	
	}

	if (canvi.descriptius | canvi.filtre){

		### variables ambientals de descriptius bivariants amb els genotips ###

		if (winDialog("yesno","Vols descriptives bivariants entre ambientals i els haplotips?")=="YES"){
			descriptives<-TRUE
			if (canvi.descriptius | !"descriptius.labels"%in%objects()){
				descriptives.labels<-select.list(names(dades)[!names(dades)%in%locus.labels],title="descriptives",multiple=TRUE)
				if (length(descriptives.labels)==0) descriptives=FALSE
			}
			if (descriptives){
				descriptives.var<-dades[keep,descriptives.labels]
				Wald<-p.wald<-df.wald<-rep(NA,ncol(descriptives.var))
				for (i in 1:ncol(descriptives.var)){
					fit.aux<-haplo.model(haplo.base=NULL,descriptius=TRUE,y=descriptives.var[,i])
					print(fit.aux)
					if (!is.character(fit.aux))
						{Wald[i]<-fit.aux$W; df.wald[i]<-fit.aux$df.haplo; p.wald[i]<-fit.aux$p.haplo}
					if (is.character(fit.aux)) ## vol dir que no ha pogut calcular l'estadístic de Wald
						{Wald[i]<-NA; df.wald[i]<-NA; p.wald[i]<-NA}
				}
				taula.descriptives<-data.frame(list(var=descriptives.labels,Wald=Wald,df=df.wald,p.valor=p.wald))

				printBanner("Bivariants ambientals vs. haplotips")
				printBanner(paste("Filtre: ",sub("filtre<-","",filtre),sep=""), banner.width=40, char.perline=30, border="-")
				taula.descriptives2<-taula.descriptives
				#taula.descriptives2[,-1]<-round(taula.descriptives2[,-1],3)
				print(taula.descriptives2)
				printBanner("",border="")

				if (primera.descriptives) write.table(paste("Freq. mínima: ",min.freq,sep=""),file=paste(path,"/resultats descriptives",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=FALSE)
				if (!primera.descriptives) write.table(paste("Freq. mínima: ",min.freq,sep=""),file=paste(path,"/resultats descriptives",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
				write.table(paste("Efecte de l'haplotip: ",haplo.effect,sep=""),file=paste(path,"/resultats descriptives",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
				if (na.action=="na.exclude") max.missings=0
				write.table(paste("Na.action: ",na.action," (màxims de missings en els SNIP's: ",max.missings,")",sep=""),file=paste(path,"/resultats descriptives",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
				write.table(paste("Filtre: ",sub("filtre<-","",filtre),sep=""),file=paste(path,"/resultats descriptives",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
				write.table(" ",file=paste(path,"/resultats descriptives",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
				write.table(taula.descriptives,file=paste(path,"/resultats descriptives",".xls",sep=""),col.names=TRUE,row.names=FALSE,append=TRUE,dec=dec.option,sep="\t")
				write.table(" ",file=paste(path,"/resultats descriptives",".xls",sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
				primera.descriptives=FALSE
			}

		}

	}

	tornar<-winDialog("yesno", "Vols tornar-hi (amb la mateixa base de dades)?")=="YES"
	if (tornar){
		sel<-select.list(c("resposta","polimorfismes","grup","ambientals","descriptius"),multiple=TRUE,title="Quines variables vols canviar?")
		canvi.resposta<-"resposta"%in%sel
		canvi.polimorfismes<-"polimorfismes"%in%sel
		canvi.ambientals<-"ambientals"%in%sel
		canvi.grup<-"grup"%in%sel
		canvi.descriptius<-"descriptius"%in%sel
		sel<-select.list(c("na.action","haplo.effect","min.freq","EM control","GLM haplo freq. min","filtre"),multiple=TRUE,title="Quines opcions vols canviar?")
		canvi.na.action<-"na.action"%in%sel
		canvi.haplo.effect<-"haplo.effect"%in%sel
		canvi.min.freq<-"min.freq"%in%sel		
		canvi.em.control<-"EM control"%in%sel
		canvi.haplo.glm.min<-"GLM haplo freq. min"%in%sel
		canvi.filtre<-"filtre"%in%sel
	}

}

winDialog("ok","Espero que els resultats hagin sigut del teu agrat!!\n\n                         Apa siau!!\n")

