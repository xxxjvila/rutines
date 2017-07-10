llibreries.disponibles<-.packages(all.available = TRUE)
path.llibreries<-"U:/ULEC/Software/R cran project/llibreries de R (versio 2.3)"
llibreries.locals<-list.files(path.llibreries)



carrega.llibreria<-function(llibreria){
	for (i in 1:length(llibreria)){
		if (!llibreria[i]%in%llibreries.disponibles){
			nom.llibreria.local<-grep(llibreria[i],llibreries.locals,value=TRUE)[1]
			cat("----","Downloading local package ",nom.llibreria.local," from ",path.llibreries,"----\n")
			install.packages(paste(path.llibreries,"/",nom.llibreria.local,sep=""),repos=NULL,destdir=.Library)
		}
	}
}

carrega.llibreria("Rcmdr")


require(Rcmdr)

