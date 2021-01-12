

#' Effort comparison observed (experimental) vs predicted by the IFD calculation
#'
#' @param obj an object of class 'Landscape'
#' @param funcy real number, an optional function minimization value that can be plotted
#' @param lcex positive real number, the label font size
#' @param pcol character, the color of the points
#' @param pcex positive real number, the size of the points
#' @param log logical, should log effort be plotted?
#' @param nice logical, should a word cloud arrangement of lake labels be used (can be unstable)?
#' @param lims vector 2 long, x and y axis limit
#' @param forpaper logical, journal style changes
#' @param dens logical, should effort density be plotted?
#' @param plottex logical, should lake names be added?
#' @param Rsta logical, should the coefficient of determination ('R-squared') value be plotted?
#' @author T. Carruthers
#' @export Ecomp
#' @import wordcloud
Ecomp<-function(obj,funcy=NA,lcex=0.7,pcol=NA,pcex=0.7,log=T,nice=T,lims=NA,forpaper=F,dens=F,plottext=T,Rstat=T){

  tiny=1E-20

  if(forpaper){
    if(is.na(pcol))pcol=makeTransparent('black',100)
    one2one='#99999999'
    fitcol=pcol
    textcol='black'
  }else{
    if(is.na(pcol))pcol="#ff000080"
    one2one='blue'
    fitcol='red'
    textcol="#0000ff90"
  }

  if(log){
    if(dens){
      x<-log(obj@exeff[1,]/obj@lakearea+tiny)
      y<-log(apply(obj@eff[1,1,,,],2,sum)/obj@lakearea+tiny)
      if(is.na(lims[1]))lims<-range(c(x,y),na.rm=T)
      if(nice)plot(x,y,xlab="Observed effort density (log of days per hectare)",ylab="Predicted effort density (log of days per hectare)",col="white",xlim=lims,ylim=lims)
      if(!nice)plot(x,y,xlab="Observed effort density (log of days per hectare)",ylab="Predicted effort density (log of days per hectare)",col=pcol,pch=19,xlim=lims,ylim=lims)

    }else{
      x<-log(obj@exeff[1,]+tiny)
      y<-log(apply(obj@eff[1,1,,,],2,sum)+tiny)
      if(dim(obj@eff)[3]==1)y<-log(apply(obj@eff[1,1,,,],1,sum))
      if(is.na(lims[1]))lims<-range(c(x,y),na.rm=T)
      if(nice)plot(x,y,xlab="Observed effort (log days)",ylab="Predicted effort (log days)",col="white",xlim=lims,ylim=lims)
      if(!nice)plot(x,y,xlab="Observed effort (log days)",ylab="Predicted effort (log days)",col=pcol,pch=19,xlim=lims,ylim=lims)

    }
    if(is.na(lims[1]))lims<-range(c(x,y),na.rm=T)
  }else{
    if(dens){
      x<-obj@exeff[1,]/obj@lakearea
      y<-apply(obj@eff[1,1,,,],2,sum)/obj@lakearea
      if(is.na(lims[1]))lims<-range(c(x,y),na.rm=T)
      if(nice)plot(x,y,xlab="Observed effort density (days per hectare)",ylab="Predicted effort density (days per hectare)",col="white",xlim=lims,ylim=lims)
      if(!nice)plot(x,y,xlab="Observed effort density (days per hectare)",ylab="Predicted effort density (days per hectare)",col=pcol,pch=19,xlim=lims,ylim=lims)

    }else{
      x<-obj@exeff[1,]
      y<-apply(obj@eff[1,1,,,],2,sum)
      if(dim(obj@eff)[3]==1)y<-apply(obj@eff[1,1,,,],1,sum)
      if(is.na(lims[1]))lims<-range(c(x,y),na.rm=T)
      if(nice)plot(x,y,xlab="Observed effort (days)",ylab="Predicted effort (days)",col="white",xlim=lims,ylim=lims)
      if(!nice)plot(x,y,xlab="Observed effort (days)",ylab="Predicted effort (days)",pch=19,col=pcol,xlim=lims,ylim=lims)

    }
  }

  test<-lm(y~x-1)
  R2<-summary(test)$r.squared
  cond<-!is.na(x)

  if(plottext){
    if(nice){
      ttp(x[cond],y[cond],obj@longnam[cond],col=textcol,cex=lcex,pcex=pcex,pcol=pcol,new=F,xlim=lims,ylim=lims)
    }else{
      text(x[cond],y[cond],obj@longnam[cond],col=textcol,cex=lcex)
    }
  }#else{
  #points(x,y,col=pcol,pch=19,cex=pcex)
  #}


  if(Rstat){
    lines(c(-10e10,10e10),c(-10e10,10e10),col=one2one,lty=2)
    lines(seq(0,max(x,na.rm=T)*1.5,length.out=100),predict(test,newdat=data.frame(x=seq(0,max(x,na.rm=T)*1.5,length.out=100))),col=fitcol)
    texty=bquote(bold(R^2 == .(round(R2,3))))
    if(!is.na(funcy))legend('bottomright',legend=c(texty,paste("f =",round(funcy,3))),text.col=fitcol,bty='n')
    if(is.na(funcy))legend('bottomright',legend=texty,text.col=fitcol,bty='n')
  }
}


#' Plot IFD convergence diagnostics
#'
#' @param obj an object of class 'Landscape'
#' @param tol positive real number, the tolerance of the detection of convergence (absolute effort difference
#' @param totE logical, should the convergence diagnostic run on total lake effort (as opposed to angler x lake effort)?
#' @param n positive integer, the number of convergence data points to plot
#' @param ylimy vector length 2, the y axis limits
#' @param forpaper logical, journal style changes
#' @author T. Carruthers
#' @export plotconv
plotconv<-function(obj,tol=0.1,totE=F,n=20,ylimy=NA,forpaper=F){

  coly<-rainbow(n)
  ltys<-rep(1,n)
  if(forpaper){
    coly<-rep(makeTransparent(gray.colors(ceiling(n/3),start=0,end=0.75),180),each=3)
    ltys<-rep(1:3,ceiling(n/3))
  }

  conv<-obj@conv
  if(totE)conv<-obj@Econv
  for(i in 1:nrow(conv))conv[i,]<-std(conv[i,])
  conv[is.na(conv)]<-1
  itn<-1
  dif<-rep(1,n)
  indc<-2:ncol(conv)

  while(max(dif)>(tol/100)&itn<dim(conv)[2]){
    itn<-itn+1

    if(itn<5)ref<-c(1:itn,dim(conv)[2])
    if(itn>4)ref<-c((itn-4):itn,dim(conv)[2])
    dif<-apply(conv[,ref],1,sd,na.rm=T)
    dif[is.na(dif)]<-0
  }

  if(is.na(ylimy[1]))ylimy=quantile(conv[1:n,indc],c(0.005,0.999),type=7)

  if(forpaper){
    if(!totE)plot(std(obj@conv[1,indc]),axes=F,ylim=ylimy,col=coly[1],type="l",lty=ltys[1],lwd=2,main="",xlab="Iteration",ylab="Effort (lake x angler x pop. centre)")
    if(totE)plot(std(obj@conv[1,indc]),axes=F,ylim=ylimy,col=coly[1],type="l",lty=ltys[1],lwd=2,main="",xlab="Iteration",ylab="Effort (by lake)")
  }else{
    if(!totE)plot(std(obj@conv[1,indc]),ylim=ylimy,col=coly[1],type="l",lty=ltys[1],lwd=2,main="",xlab="Iteration",ylab="Effort (lake x angler x pop. centre)")
    if(totE)plot(std(obj@conv[1,indc]),ylim=ylimy,col=coly[1],type="l",lty=ltys[1],lwd=2,main="",xlab="Iteration",ylab="Effort (by lake)")
  }

  if(itn<(dim(conv)[2]))abline(v=itn,col="#00000050",lwd=2.3)
  for(i in 2:n)lines(std(obj@conv[i,indc]),col=coly[i],lty=ltys[i],lwd=2)
}

#' Standardize an effort convergence time series, relative to the last observation
#'
#' @param x vector of effort values, nits long
#' @author T. Carruthers
#' @export std
std<-function(x){
  log(x+0.0000001)-(log(x+0.0000001)[length(x)])
}


#' Plot IFD convergence diagnostics version 2
#'
#' @param obj an object of class 'Landscape'
#' @param totE logical, should the convergence diagnostic run on total lake effort (as opposed to angler x lake effort)?
#' @param n positive integer, the number of convergence data points to plot
#' @param ylimy vector length 2, the y axis limits
#' @param forpaper logical, journal style changes
#' @author T. Carruthers
#' @export plotconv2
plotconv2<-function(obj,totE=F,n=20,ylimy=NA,forpaper=F){

  coly<-rainbow(n)
  ltys<-rep(1,n)
  if(forpaper){
    coly<-rep(makeTransparent(gray.colors(ceiling(n/3),start=0,end=0.75),180),each=3)
    ltys<-rep(1:3,ceiling(n/3))
  }

  conv<-obj@conv
  if(totE)conv<-obj@Econv
  for(i in 1:nrow(conv)){
    conv[i,]<-conv[i,]-conv[i,ncol(conv)]
  }
  conv[is.na(conv)]<-1
  itn<-1
  dif<-rep(1,n)
  indc<-2:ncol(conv)

  if(is.na(ylimy[1]))ylimy=quantile(conv[1:n,indc],c(0.1,0.9))

  if(forpaper){
    if(!totE)plot(std(obj@conv[1,indc]),axes=F,ylim=ylimy,col=coly[1],type="l",lty=ltys[1],lwd=2,main="",xlab="Iteration",ylab="Change in Effort among iterations, lake x angler x pop. centre (days fished))")
    if(totE)plot(std(obj@conv[1,indc]),axes=F,ylim=ylimy,col=coly[1],type="l",lty=ltys[1],lwd=2,main="",xlab="Iteration",ylab="Change in Effort among iterations by lake (days fished)")
  }else{
    if(!totE)plot(std(obj@conv[1,indc]),ylim=ylimy,col=coly[1],type="l",lty=ltys[1],lwd=2,main="",xlab="Iteration",ylab="Change in Effort among iterations (lake x angler x pop. centre)")
    if(totE)plot(std(obj@conv[1,indc]),ylim=ylimy,col=coly[1],type="l",lty=ltys[1],lwd=2,main="",xlab="Iteration",ylab="Change in Effort among iterations (by lake)")
  }
  abline(h=c(-1,1),lty=2,col='grey')
  for(i in 2:n)lines(std(obj@conv[i,indc]),col=coly[i],lty=ltys[i],lwd=2)

}


#' Text positioning for word cloud
#'
#' @param x vector x coordinates
#' @param y vector of y coordinates
#' @param cex font size
#' @param pcex point size
#' @param pcol color of points
#' @param new logical, new plot needed?
#' @param show.lines logical, should lines between points and text be included
#' @author T. Carruthers
#' @export ttp
ttp<-function (x, y, words, cex = 1, pcex=1,pcol='red',new = TRUE, show.lines = TRUE,
               ...)
{
  if (new)
    plot(x, y, type = "n", ...)
  lay <- wordlayout(x, y, words, cex, ...)
  if (show.lines) {
    for (i in 1:length(x)) {
      xl <- lay[i, 1]
      yl <- lay[i, 2]
      w <- lay[i, 3]
      h <- lay[i, 4]
      if (x[i] < xl || x[i] > xl + w || y[i] < yl || y[i] >
          yl + h) {
        points(x[i], y[i], pch = 16, col = pcol, cex = pcex)
        nx <- xl + 0.5 * w
        ny <- yl + 0.5 * h
        lines(c(x[i], nx), c(y[i], ny), col = "grey")
      }
    }
  }
  text(lay[, 1] + 0.5 * lay[, 3], lay[, 2] + 0.5 * lay[, 4],
       words, cex = cex, ...)
}

#' Text positioning for word cloud (just data points)
#'
#' @param x vector x coordinates
#' @param y vector of y coordinates
#' @param works character vector of words to plot
#' @param cex font size
#' @param pcex point size
#' @param pcol color of points
#' @param new logical, new plot needed?
#' @param show.lines logical, should lines between points and text be included
#' @param lwd postive real number, the width of the lines
#' @param lcol character, the color of the lines
#' @author T. Carruthers
#' @export ttp
ttp_pos<-function (x, y, words, cex = 1, pcex=1,pcol='red',new = TRUE, show.lines = TRUE,lwd=1,lcol='black',
                   ...)
{
  #if (new)  plot(x, y, type = "n", ...)
  lay <- wordlayout(x, y, words, cex, ...)
  if (show.lines) {
    for (i in 1:length(x)) {
      xl <- lay[i, 1]
      yl <- lay[i, 2]
      w <- lay[i, 3]
      h <- lay[i, 4]
      #if (x[i] < xl || x[i] > xl + w || y[i] < yl || y[i] >
      #yl + h) {
      #points(x[i], y[i], pch = 16, col = pcol, cex = pcex)
      nx <- xl + 0.5 * w
      ny <- yl + 0.5 * h
      lines(c(x[i], nx), c(y[i], ny), col = lcol,lwd=lwd)
      #}
    }
  }
  cbind(lay[, 1] + 0.5 * lay[, 3], lay[, 2] + 0.5 * lay[, 4])
}


#' Plot a map of landscape effort
#'
#' @param obj an object of class 'Landscape'
#' @param maxps real number, maximim size of points
#' @param latlim vector 2 long, the x axis, latitude range of the plot
#' @param lonlim vector 2 long, the y axis, longitude range of the plot
#' @param nman positive integer, the number of managmeent options to plot
#' @param nsim positive integer, the number of simulations to plot
#' @param nice logical, should a word cloud arrangement of lake labels be used (can be unstable)?
#' @param dens logical, should effort density be plotted?
#' @param U logical, should utility be plotted instead of effort?
#' @param lcex the label font size
#' @param pcex the point size
#' @param lpick vector of either lake names or their index number, to be highlighted
#' @author T. Carruthers
#' @export mapeff
mapeff<-function(obj,maxps=2,latlim=NA,lonlim=NA,nman=1,nsim=1,dens=F,U=F,lcex=0.55,pcex=0.75,lpick=NA){

  if(is.na(lonlim[1]))lonlim<-range(c(obj@lakex,obj@pcx))+c(0.5,-0.5)
  if(is.na(latlim[1]))latlim<-range(c(obj@lakey,obj@pcy))+c(-0.5,0.5)
  #windows(width=(lonlim[2]-lonlim[1])*0.65,height=latlim[2]-latlim[1])
  if(nman>1&nsim>1){
    nman<-min(nman,5,obj@nmanage)
    nsim<-min(nsim,5,obj@nsim)
    par(mfrow=c(nman,nsim),mar=rep(0,4))
  }else if(nman>1){
    nman<-min(nman,25,obj@nmanage)
    par(mfrow=c(ceiling(nman/ceiling(nman^0.5)),ceiling(nman^0.5)),mar=rep(0,4))
  }else if(nsim>1){
    nsim<-min(nsim,25,obj@nsim)
    par(mfrow=c(ceiling(nsim/ceiling(nsim^0.5)),ceiling(nsim^0.5)),mar=rep(0,4))
  }else if(nsim==1&nman==1){
    par(mfrow=c(1,1),mar=rep(0,4))
  }

  #jpeg(paste(Drive,":/Trout lakes/Images/map topo.jpg",sep=""),width=10, height=8, units="in", res=600)
  if(is.na(lpick)|!class(lpick)%in%c('numeric','integer')){
    lcol="#0000ff80"
    llcol="#0000ff90"

  }else{
    lcol<-rep("#99999970",obj@nl)
    llcol=rep('#99999970',obj@nl)
    lcol[lpick]<-"#0000ff80"
    llcol[lpick]<-"blue"

  }


  for(mm in 1:nman){
    for(ss in 1:nsim){
      map(database = "world", xlim=lonlim, ylim=latlim,resolution = 0,col=obj@topcols[1],mar=rep(0.3,4))  # make a first plot of the map to define the range of the plot
      #polygon(x=rep(lonlim*c(0.5,1.5),each=2),y=c(latlim*c(0.5,1.5),(latlim*c(0.5,1.5))[2:1]),col=colors()[257],border=NA)
      #for(i in 1:length(obj@cont))lines(obj@cont[[i]]$x,obj@cont[[i]]$y,type="l",col=obj@topcols[(obj@cont[[i]]$level/500)+1])#lapply(bc@cont,plottop,cols=bc@topcols)
      #for(i in 1:length(obj@cont))polygon(obj@cont[[i]]$x,obj@cont[[i]]$y,col=obj@topcols[(obj@cont[[i]]$level/500)+1],border=obj@topcols[(obj@cont[[i]]$level/500)+1])#lapply(bc@cont,plottop,cols=bc@topcols)
      ladj<-0.1
      latadj<-rep(0.2,obj@npc)
      eff<-apply(obj@eff[mm,ss,,,],2,sum,na.rm=T)
      if(U)eff<-apply(obj@U[mm,ss,,,],2,sum,na.rm=T)
      if(dens)eff<-eff/obj@lakearea
      eff<-eff/mean(eff)
      cexy<-((0.1+eff*(4/max(eff)))*maxps)^0.5
      points(obj@lakex,obj@lakey,pch=16,cex=cexy,col=lcol)
      textplot(obj@lakex,obj@lakey+ladj,obj@longnam,col=lcol,cex=lcex,new=FALSE,show.lines=F,font=2)

      activepop<-obj@pcsize[ss,]
      activepop<-activepop/mean(activepop)
      cexy<-((0.1+activepop*(4/max(activepop)))*maxps)^0.5
      points(obj@pcx,obj@pcy,pch=16,cex=cexy,col="#ff000080")
      text(obj@pcx,obj@pcy+latadj,obj@pcnam,col="#ff000090",cex=pcex,lwd=2,font=2)
    }
  }
}




#' Plot a map of landscape effort
#'
#' @param ED an object of class 'Landscape'
#' @param maxps real number, maximim size of points
#' @param latlim vector 2 long, the x axis, latitude range of the plot
#' @param lonlim vector 2 long, the y axis, longitude range of the plot
#' @param nman positive integer, the number of managmeent options to plot
#' @param nsim positive integer, the number of simulations to plot
#' @param nice logical, should a word cloud arrangement of lake labels be used (can be unstable)?
#' @param dens logical, should effort density be plotted?
#' @param U logical, should utility be plotted instead of effort?
#' @param lcex the label font size
#' @param pcex the point size
#' @param lpick vector of either lake names or their index number, to be highlighted
#' @author T. Carruthers
#' @export mapeff
UTplot<-function(ED,aclassnam,popnams,lonlim,latlim,obj,region_list,cex.axis=0.8){

  acol<-c("white","light grey","dark grey","black")
  plot(lonlim,latlim,col="white",axes=F,xlab="",ylab="")
  map(database = "worldHires", xlim=lonlim, ylim=latlim,resolution = 0,fill=T,col="grey85",mar=rep(0.3,4),add=T,lwd=1)#make a first plot of the map to define the range of the plot
  map(database = "worldHires", xlim=lonlim, ylim=latlim,resolution = 0,fill=F,col="grey85",mar=rep(0.3,4),add=T,lwd=1)#make a first plot of the map to define the range of the plot
  for(i in 1:length(region_list))polygon(region_list[[i]][,1],region_list[[i]][,2],border=rgb(1,1,1,0.5))
  lines(c(-122.9,-112),c(49,49),col="white",lwd=3)

  out<-ttp_pos(obj@pcx,obj@pcy,rep("HH",obj@npc),cex=6.5,lwd=2.5,lcol="grey70")

  set.seed(3)
  sz<-1.6
  xlims<-cbind(out[,1]-sz/1.8,out[,1]+sz/1.8)#cbind(obj@pcx,obj@pcx+1)
  ylims<-cbind(out[,2]-sz/3.5,out[,2]+sz/3.5)#cbind(obj@pcy,obj@pcy+1)
  ylimy<-range(ED)
  locs<-pretty(seq(ylimy[1]*1.1,ylimy[2]*1.1,length.out=4))
  for(i in 1:nrow(xlims)){
    subplot({
      border<-rep('black',4)
      border[ED[i,]==0]<-'NA'
      barplot(ED[i,],axes=F,ylim=ylimy,col=acol,xlab="",ylab="",main="",border=border)

      #if(max(ED[i,])>(0.6*ylimy[2])){
      #  text(2.5,max(ED[i,])/2+10,obj@pcnam[i],cex=0.75)
      #}else{
      #  text(2.5,max(ED[i,])+10,obj@pcnam[i],cex=0.75)
      #}
      axis(2,locs,locs,cex.axis=cex.axis,padj=0.8)
      #abline(h=1,col='#50505060')
      #abline(v=0.5,col='#50505060')
      mtext(popnams[i],3,line=-0.1,cex=0.7,adj=0.1)

    },xlims[i,],ylims[i,],pars=list(bg='white'))
  }

  #x<-(-50:-200)
  #y<-(30:80)
  #axis(1,at=x,labels=as.character(x),cex.axis=0.8)
  #axis(2,at=y,labels=as.character(y),cex.axis=0.8)

}

#' Make a standard color transparent
#'
#' @param someColor character string, a color
#' @param alpha percentage, the transparency of the color
#' @author T. Carruthers
#' @export makeTransparent
makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}


