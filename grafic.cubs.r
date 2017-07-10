if (!"carrega.llibreria"%in%ls()) source(file.path(RutinesLocals,"carrega.llibreria.r"))

carrega.llibreria("scatterplot3d")

grafic.cubs<-function(x,y,z,funcio=NULL,temps.lim=NULL,dec.car=".",rev.x=FALSE,rev.y=FALSE,digits=1,z.lim=0.04,
  xlab=as.character(substitute(x)),ylab=as.character(substitute(y)),zlab="",srt=35,posa.valors=FALSE,digits.valors=digits,
    color.valors="red",colors=NULL,color.cares=c(0.8,0.9,1),color.terra="white",cex.axisx=1,cex.axisy=1,cex.axisz=1,len.tick.marks=7,...){


#x=dislip*100
#funcio=NULL
#temps.lim=NULL
#dec.car="."
#rev.x=FALSE
#rev.y=FALSE
#digits=1
#z.lim=100
#xlab=""
#ylab=""
#zlab=""
#srt=35
#posa.valors=FALSE
#digits.valors=digits
#color.valors="red"
#colors<-list(c(0,0,100),c(0,0,200))
#color.cares=c(0.8,0.9,1)
#color.terra<-"grey"
#cex.axisx=1
#cex.axisy=1
#cex.axisz=1
#len.tick.marks=7


  # x=variable columna (ha de ser un factor els levels del qual són les etiquetes de l'eix x)
  # y=variable fila (ha de ser un factor els levels del qual són les etiquetes de l'eix y)
  # funcio: el valor resum de cada grup format per la combinació x,y (pot ser una funcio definida com mean, min, max, etc -sense cometes-
  #     o la funció "surv" que ha d'anar amb cometes. "surv" serveix per a calcular la incidència per a cada grup en el moment temps.lim
  # temps.lim: si funció=="surv" és el temps en el qual es vol calcular la incidència (p.e 1862 dies = 5 anys).
  # dec.car: símbol pels decimals (per a l'eix de les z).
  # rev.x: inverteix l'ordre dels nivells de x
  # rev.y: inverteix l'ordre dels nivells de y
  # digits: nombre de decimals (eix z)
  # z.lim: límit superior de l'eix de les z (el límit inferior és el zero).
  # srt: angle de l'etiqueta de l'eix de les y.
  # posa.valors: posa els valors en el gràfic (a sobre de cada barra).
  # digits.valors: digits dels valors.
  # colors: codi colors en format rgb (red, green, blue) amb escala de 0 a 255.
  # color.cares: factor (0-1) que multiplica a colors, vector de tres components, c(dreta, sobre, davant).
  # color.terra: color de la base
  # cex.axisx,cex.axisy,cex.axisz: grandària de les etiquetes (en els ticks) dels eixos
  #... altres paràmetres passats a scatterplot3d 


  ## method

  if (!is.null(funcio)){

    if (is.character(funcio)){     # la única possibilitat és que sigui "surv"
      f<-function(x){
        temps<-x[,1]
        cens<-x[,2]
        if (sum(cens)==0) return(0)
        else{
          surv<-summary(survfit(Surv(temps,cens)))
          candidats.neg<-surv$surv[surv$time<=temps.lim]
          candidats.pos<-surv$surv[surv$time>temps.lim]
          if (length(candidats.neg)==0) return(1-candidats.pos[1])
          if (length(candidats.neg)>0)return(1-rev(candidats.neg)[1])
        }
      }
    } else  f=funcio

    z.scale=1
    aux<-by(z,list(x,y),f)
    valors<-matrix(unlist(aux),length(names(table(x))),length(names(table(y))))
    if (is.character(funcio)){
      valors<--log(1-valors)  # si la funcio és supervivencia es donen les incidencies
      z.scale=100
    }
    colnames(valors)<-attributes(y)$levels
    rownames(valors)<-attributes(x)$levels

  } else {
    z.scale=1
    valors=x
  }


  ## inicialitzem el requadre ##

  options(OutDec=dec.car)

  wtick.z=0.09

  wlab.x=z.lim*0.05

#  s<-scatterplot3d(rep(seq(0,(nrow(valors)-1)*2,2)+1,each=ncol(valors)), rep(1:ncol(valors),each=nrow(valors)), c(0,rep(z.lim,ncol(valors)*nrow(valors)-1)),
#    type='n',xlab=xlab, ylab="", zlab=zlab,grid=FALSE, box=FALSE,
#    tick.marks=FALSE,xlim=c(0,nrow(valors)*2),ylim=c(0,ncol(valors)),...)

  s<-scatterplot3d(rep(seq(0,(nrow(valors)-1)*2,2)+1,each=ncol(valors)), rep(1:ncol(valors),each=nrow(valors)), c(0,rep(z.lim,ncol(valors)*nrow(valors)-1)),
    type='n',xlab=xlab, ylab="", zlab=zlab,grid=FALSE, box=FALSE,
    tick.marks=FALSE,xlim=c(0,nrow(valors)*2),ylim=c(0,ncol(valors)))


  #xb <- c(-wtick.z, 0, 0, nrow(valors)*2)
  xb <- c(-wtick.z, 0)
  #yb <- c(0, 0, ncol(valors), ncol(valors))
  yb <- c(0, 0)
  tick.marks.z=seq(0,z.lim,len=len.tick.marks)
  for (i in tick.marks.z){
    #zb <- c(i, i, i, i)
    zb <- c(i, i)
    lines(s$xyz.convert(xb,yb,zb))
  }
  xb <- c(nrow(valors)*2, nrow(valors)*2)
  yb <- c(ncol(valors), ncol(valors))
  zb <- c(0, z.lim)
  #lines(s$xyz.convert(xb,yb,zb))
  xb <- c(0, 0)
  yb <- c(ncol(valors), ncol(valors))
  zb <- c(0,z.lim)
  #lines(s$xyz.convert(xb,yb,zb))

  # color del terra
  polygon(s$xyz.convert(c(0,nrow(valors)*2,nrow(valors)*2,0),c(0,0,ncol(valors),ncol(valors)),c(0,0,0,0)),col=color.terra)


  dibuixa.cub<-function(coord,amplada,llargada,alçada,color){
    #cara dreta
    xb <- c(coord[1]+amplada, coord[1]+amplada, coord[1]+amplada, coord[1]+amplada, coord[1]+amplada)
    yb <- c(coord[2], coord[2]+llargada, coord[2]+llargada, coord[2], coord[2])
    zb <- c(0, 0, alçada, alçada, 0)
    if (grisos) polygon(s$xyz.convert(xb,yb,zb),col=grey(color*color.cares[1]))  # la cara dreta és la més fosca
    if (!grisos) polygon(s$xyz.convert(xb,yb,zb),col=rgb(color[1]*color.cares[1],color[2]*color.cares[1],color[3]*color.cares[1],maxColorValue=255))
    # sobre
    xb <- c(coord[1], coord[1]+amplada, coord[1]+amplada, coord[1], coord[1])
    yb <- c(coord[2], coord[2], coord[2]+llargada, coord[2]+llargada, coord[2])
    zb <- c(alçada, alçada, alçada, alçada, alçada)
    if (grisos) polygon(s$xyz.convert(xb,yb,zb),col=grey(color*color.cares[2]))
    if (!grisos) polygon(s$xyz.convert(xb,yb,zb),col=rgb(color[1]*color.cares[2],color[2]*color.cares[2],color[3]*color.cares[2],maxColorValue=255))
    # davant
    xb <- c(coord[1], coord[1]+amplada, coord[1]+amplada, coord[1], coord[1])
    yb <- c(coord[2], coord[2], coord[2], coord[2], coord[2])
    zb <- c(0, 0, alçada, alçada, 0)
    if (grisos) polygon(s$xyz.convert(xb,yb,zb),col=grey(color)*color.cares[3])
    if (!grisos) polygon(s$xyz.convert(xb,yb,zb),col=rgb(color[1]*color.cares[3],color[2]*color.cares[3],color[3]*color.cares[3],maxColorValue=255))
  }

  par(xpd=NA)
  # eix x.
  tick.marks.x=seq(0,(nrow(valors)-1)*2,2)+1
  text(s$xyz.convert(tick.marks.x, rep(0,nrow(valors)), rep(-wlab.x,nrow(valors))),if (rev.x) rev(rownames(valors)) else rownames(valors),cex=cex.axisx)

  # eix y.
  text(s$xyz.convert(rep(nrow(valors)*2,ncol(valors))+wtick.z*6, 0:(ncol(valors)-1)+0.5, rep(0,ncol(valors))), format(if (rev.y) rev(colnames(valors)) else colnames(valors),trim=TRUE, justify="left"),cex=cex.axisy)
  if (ylab!=""){
    winDialog("ok","Posa l'etiqueta de l'eix de les y's")
    text(locator(1),ylab,srt=srt, adj=c(0,1))
  }

  # eix z.
  text(s$xyz.convert(-rep(wtick.z*2.5,length(tick.marks.z)), rep(0,length(tick.marks.z)), tick.marks.z), format(round(tick.marks.z*z.scale,digits),trim=TRUE,nsmall=digits,justify="right"),cex=cex.axisz)
  par(xpd=FALSE)


  ## dibuixa els cubs (de darrera a davant i d'esquerre a dreta) ##

  grisos<-FALSE
  if (is.null(colors)){
    colors<-list(0.1,0.5,0.9)
    grisos<-TRUE
  }
  
  
  pos.x<-tick.marks.x-0.5
  pos.y<-0:(ncol(valors)-1)+0.15
  if (rev.x) pos.x=rev(pos.x)
  if (rev.y) pos.y=rev(pos.y)
  for (j in rev(1:ncol(valors))){
    for (i in 1:nrow(valors)){
      dibuixa.cub(coord=c(pos.x[i],pos.y[j]),amplada=1,llargada=0.7,alçada=valors[i,j],color=colors[[j]])
      Sys.sleep(0)
    }
  }

  if (posa.valors){
    xb=rep(tick.marks.x,each=ncol(valors))
    if (rev.x) xb=rev(xb)
    yb=rep(0:(ncol(valors)-1)+0.5,nrow(valors))
    if (rev.y) yb=rev(yb)
    zb=NULL
    for (i in 1:nrow(valors)) zb=c(zb,as.double(valors[i,]))
    text(s$xyz.convert(xb,yb,zb+z.lim*0.01),format(round(zb*z.scale,digits.valors),trim=TRUE,nsmall=digits.valors),col=color.valors,cex=1.2)
  }


}
