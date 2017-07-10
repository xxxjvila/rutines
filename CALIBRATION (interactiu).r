##########################################################

############### CALIBRATION TEST #########################

##########################################################




#############################################################################################################
###################################### FUNCIONS PREVIES #####################################################
#############################################################################################################


### FUNCIÓ QUE CARREGA LLIBRERIES ###

carrega.llibreria<-function(llibreries){
	required.library<-llibreries
	missing.library<-required.library[!required.library%in%.packages(all.available = T)] 
	if (length(missing.library)>0) for (i in 1:length(missing.library)) install.packages(missing.library[i])
	if (sum(!paste("package:",required.library,sep="")%in%search())>0) for (i in 1:length(required.library)) library(required.library[i],character.only=T)
}


## FA EL GRÀFIC DE BARRES.

bar.plot<-function(true.legends){

	par(las=1)

	mp<-barplot(t(X), beside = T,col = colors,xlab="group risk intervals (%)",ylab="risk (%)",
				legend.text=F,ylim=c(0,max(HL$taula$events.esp1,HL$taula$events.esp2)*1.2),axisnames = FALSE,axes=FALSE)
	axis(1,mp[2,],risc,tick=F,pos=c(0.2,0))
	axis(1,mp[2,],paste("n=",HL$taula$n,sep=""),tick=F,pos=c(-0.6,0),font=3,cex.axis=0.9)
	axis(2,pretty(seq(0,max(X)*1.05)))

	hh<-t(X)
	abline(h=0)
	if (is.null(strata.label)) title(main="Calibration test")
	if (!is.null(strata.label)){
		title(main="Calibration test:")
		mtext(subtitle)
	}

	if (!true.legends){ # true.legends vol dir que les llegendes ja hi són
		winDialog("ok","place legend bar labels")
		legend.coord<-locator(1)
		legend(legend.coord,bty="n",colnames(X),fill=colors)
				#posa la taula de la chiquadrat dins del gràfic
		rangx<-max(mp)-min(mp)
		rangy<-max(hh)
		yscale=15
		xscale=5
		winDialog("ok","place chisquare results")
		coord.chi<-locator(1)
		adj=c(1,0)
		text(coord.chi$x,coord.chi$y-rangy/yscale,"original",adj=adj)
		text(coord.chi$x,coord.chi$y-2*rangy/yscale,"adapted",adj=adj)
		text(coord.chi$x+rangx/xscale,coord.chi$y,expression(chi^2),adj=adj)
		text(coord.chi$x+2*rangx/xscale,coord.chi$y,"p-value",adj=adj)
		text(coord.chi$x+rangx/xscale,coord.chi$y-rangy/yscale,HL$chisq1,adj=adj)
		text(coord.chi$x+rangx/xscale,coord.chi$y-2*rangy/yscale,HL$chisq2,adj=adj)
		text(coord.chi$x+2*rangx/xscale,coord.chi$y-rangy/yscale,p.valor1,adj=adj)
		text(coord.chi$x+2*rangx/xscale,coord.chi$y-2*rangy/yscale,p.valor2,adj=adj)

		arrows(x0=mp[1,],y0=obs.inf*100,x1=mp[1,],y1=obs.sup*100,length = 0.03, angle = 90, code = 3,lwd = 1)
		mp[1,]<-mp[1,]-(max(mp[1,])-min(mp[1,]))/25
		text(mp,hh+max(HL$taula$events.esp1,HL$taula$events.esp2)/30,format(round(t(X),1)))
	}

}


#############################################################################################################
##################################### FI FUNCIONS PREVIES ###################################################
#############################################################################################################



carrega.llibreria(c("foreign","Hmisc","survival"))

#############################################################################################################
########################################### INPUTS ##########################################################
#############################################################################################################

#file<-choose.files("Escull la base de dades en SPSS")



dades<-fusio
attach(dades)

dades<-subset2(dades,"!is.na(gir) & !is.na(fra) & !is.na(eventci) & !is.na(toeventci)")

risk1.label<-"fra"
risk2.label<-"gir"
cens.label<-"eventci"
time.label<-"toeventci"
strata.label<-"sexo"



time.max<-1826.25*2
cutpoints<-c(2.5,5,10)


exces=TRUE #  "Estimació del risc per excés?"



#############################################################################################################
########################################### FI INPUTS #######################################################
#############################################################################################################





#############################################################################################################
########################################### PROGRAMA PRINCIPAL ##############################################
#############################################################################################################


#cutpoints<-as.double(strsplit(cutpoints,split=" ")[[1]])
cutpoints<-c(5,10)

repeticions=1
valors.strata<-as.integer(attributes(dades[,strata.label])$value.labels)
if (!is.null(valors.strata)){
	valors.strata<-as.integer(attributes(dades[,strata.label])$value.labels)			
	labels.strata<-names(attributes(dades[,strata.label])$value.labels)[order(valors.strata)]
	valors.strata<-sort(valors.strata)			
}


for (i in 1:repeticions){

	windows()

	true.legends=F

	if (!is.null(strata.label)){
		keep<-dades[,strata.label]==valors.strata[i]
		subtitle<-paste(strata.label,": ",labels.strata[i],sep="")
	}

	risk1<-dades[keep,risk1.label]
	risk2<-dades[keep,risk2.label]
	cens<-dades[keep,cens.label]
	time<-dades[keep,time.label]

	if (max(risk1)<100) risk1<-risk1*100  #els riscos en %
	if (max(risk2)<100) risk2<-risk2*100  #els riscos en %

	if (length(cutpoints)==1) 	categ<-cut2(risk1,g=cutpoints,digits=1)
	if (length(cutpoints)>1)	categ<-cut2(risk1,cuts=c(min(risk1),cutpoints,max(risk1)),digits=1)
		
	risc<-attributes(categ)$levels  #etiquetes dels grups
	grups=length(risc)	
	n<-as.integer(tapply(rep(1,length(categ)),categ,sum))
	
	obs<-obs.inf<-obs.sup<-esp1<-esp2<-rep(NA,grups)
	for (g in 1:length(risc)){
		surv<-summary(survfit(Surv(time,cens),subset=categ==risc[g]))
		aux.obs<-1-surv$surv[surv$time<=time.max]
		aux.obs.sup<-1-surv$lower[surv$time<=time.max]
		aux.obs.inf<-1-surv$upper[surv$time<=time.max]
		obs[g]<-aux.obs[length(aux.obs)]
		obs.inf[g]<-aux.obs.inf[length(aux.obs)]
		obs.sup[g]<-aux.obs.sup[length(aux.obs)]
		if (exces) {
			if (max(surv$time)>=time.max){
				aux.obs<-1-surv$surv[surv$time>time.max]
				aux.obs.sup<-1-surv$lower[surv$time>time.max]
				aux.obs.inf<-1-surv$upper[surv$time>time.max]
				obs[g]<-aux.obs[1]
				obs.inf[g]<-aux.obs.inf[1]
				obs.sup[g]<-aux.obs.sup[1]
			}
			if (max(surv$time)<time.max){
				obs[g]<-1-surv$surv[length(surv$surv)]
				obs.inf[g]<-1-surv$lower[length(surv$surv)]
				obs.sup[g]<-1-surv$upper[length(surv$surv)]
			}
		}
	}

	esp1<-as.double(tapply(risk1,categ,mean))
	esp2<-as.double(tapply(risk2,categ,mean))
	chi1<-n*(100*obs-esp1)^2/(esp1*(100-esp1))
	chi2<-n*(100*obs-esp2)^2/(esp2*(100-esp2))
	d=2
	HL<-list(chi1=chi1,chi2=chi2,chisq1=round(sum(chi1),digits=d),pvalor1=pchisq(sum(chi1),grups-2,lower.tail = F),
		chisq2=round(sum(chi2),digits=d),pvalor2=pchisq(sum(chi2),grups-2,lower.tail = F),
		taula=data.frame(list(grup=1:grups,n=n,risc=risc,events=round(obs*100,2),events.esp1=round(esp1,digits=d),
		events.esp2=round(esp2,digits=d))))

	par(las=1)

	if (length(cutpoints)>1){
		risc[1]=paste("<",cutpoints[1],sep="")
		risc[grups]=paste(">=",cutpoints[grups-1],sep="")
	}
	
	X<-as.matrix(cbind(HL$taula$events,HL$taula$events.esp2,HL$taula$events.esp1))
	rownames(X)<-HL$taula$risc
	colnames(X)<-c("observed","adapted","original")	
	colors<-c("light yellow","light blue","magenta")
	p.valor1<-ifelse(HL$pvalor1<0.001,"<0.001",round(HL$pvalor1,3))
	p.valor2<-ifelse(HL$pvalor2<0.001,"<0.001",round(HL$pvalor2,3))

	## PLOT
	bar.plot(true.legends=T)


	## CANVI SUBTITOL ##

	if (!is.null(strata.label)){
		if (winDialog("yesno","Change subtitle?")=="YES") subtitle<-winDialogString(message="Subtitle text",default=subtitle)
		## PLOT
		true.legends<-bar.plot(true.legends)
	}


	## CANVI DE COLORS DE LES BARRES ##

	sel<-winDialog("yesno","Change colors?")

	while (sel=="YES"){

		windows(4,4,xpos=0,ypos=0)
		par(mar=c(0,0,0,0))
		plot(rep(1:25,25),rep(1:25,each=25),col=colours()[1:(25*25)],bg=colours()[1:(25*25)],pch=22,cex=2.3)
	
		windows<-dev.list()

		winDialog("ok","Observed bar color")
		coord<-locator(1)
		coord<-trunc(c(coord$x,coord$y)+0.5)
		colors[1]<-colours()[(coord[1]+25*(coord[2]-1))]
		
		dev.set(windows[length(windows)-1])  #penúltima finestra (gràfic)
		## PLOT
		bar.plot(true.legends=T)

		dev.set(windows[length(windows)])  #última finestra (paleta colors)
		winDialog("ok","Adapted bar color")
		coord<-locator(1)
		coord<-trunc(c(coord$x,coord$y)+0.5)
		colors[2]<-colours()[(coord[1]+25*(coord[2]-1))]
		dev.set(windows[length(windows)-1])
		## PLOT
		bar.plot(true.legends=T)
	
		dev.set(windows[length(windows)])
		winDialog("ok","Original bar color")
		coord<-locator(1)
		coord<-trunc(c(coord$x,coord$y)+0.5)
		colors[3]<-colours()[(coord[1]+25*(coord[2]-1))]
		dev.set(windows[length(windows)-1])
		##PLOT
		bar.plot(true.legends=F)

		sel<-winDialog("yesno","Change default colors?")
	
		dev.off(windows[length(windows)])
	}
}		

print(HL)



#############################################################################################################
########################################### FI PROGRAMA PRINCIPAL ###########################################
#############################################################################################################
