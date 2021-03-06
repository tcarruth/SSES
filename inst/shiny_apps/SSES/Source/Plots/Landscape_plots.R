
plotLand <- function(dummy=1){

  par(mfrow=c(1,3),mai=c(0.8,0.3,0.05,0.05),omi=c(0,0.5,0,0))
  selind<-obj@longnam%in%input$Lsel

  histplot(obj@lakearea,selind,qs=c(0,0.97),xlab="Lake size (ha)")
  histplot(obj@GDD,selind,qs=c(0,0.99),xlab="Growing Deg Days")
  mudist<-apply(array(obj@pcsize,c(obj@npc,obj@nl))*obj@pcxl[1,,],2,sum)/sum(obj@pcsize)
  histplot(mudist,selind,qs=c(0,0.99),xlab="Mean angler distance (km)")
  mtext("Freq.",line=0.7,2,outer=T)

}

plotMan <-function(dummy=1,Sel=F){

  Lind<-rep(T,obj@nl)
  if(Sel)Lind<-select$Lind
  dummy<-sum(manage$lxslev)+sum(manage$lxattr)
  if(Sel&sum(Lind)==0){

    plot(1,1,col='white',axes=F,xlab="",ylab="",main="")
    legend('center',legend="< No lakes selected >",bty='n',text.col="grey")

  }else{

    temp<-select$Mind
    par(mfrow=c(3,2),mai=c(0.3,0.6,0.15,0.05))
    dat<-apply(manage$lxslev[,Lind,,drop=F],c(1,3),sum)/1000
    colnames(dat)<-obj@stnam
    cols<-mcols[1:length(obj@misc$Mnams)]
    barplot(dat,beside=TRUE,col=cols,border=NA)
    legend('topleft',legend=obj@misc$Mnams,text.col=cols,bty='n',cex=1.2,text.font=2)
    mtext("Stocking (,000 fish)",2,line=2.6)

    mout<-list()

    multiple<-sum(Lind)>1
    if(multiple){
      for(mm in 1:obj@nmanage){
        mout[[mm]]<-apply(array(obj@lxattr[mm,Lind,],c(sum(Lind),6)),2,table)
      }
    }

    labnams<-c("BoatRes","MotorRes","GearRes","TakeLim")
    attind<-c(1,3,4,6)
    for(ll in 1:length(labnams)){
      nlev<-length(get(labnams[ll]))
      if(multiple){
        temp<-lapply(mout,function(X,nlev){
          out<-rep(0,nlev)
          test<-X[[attind[ll]]]
          out[as.numeric(names(test))]<-test
          out
          },nlev=nlev)
        dat<-t(matrix(unlist(temp),nrow=nlev,ncol=obj@nmanage))
        colnames(dat)<-get(labnams[ll])
      }else{
        dat<-matrix(0,ncol=nlev,nrow=obj@nmanage)
        dat[cbind(1:obj@nmanage,obj@lxattr[,Lind,attind[ll]])]<-1
        colnames(dat)<-get(labnams[ll])

      }

      barplot(dat,beside=TRUE,col=cols,border=NA)
      mtext("No. lakes)",2,line=2.6)
    }
  }

}


histplot<-function(all,selind,qs=c(0.01,0.99),namey="",xlab=NA){

  rng<-quantile(all,qs)
  suby<-all[selind]
  all<-all[all>rng[1]&all<rng[2]]
  suby<-suby[suby>rng[1]&suby<rng[2]]
  breaks<-seq(rng[1],rng[2],length.out=30)
  hist(all,breaks=breaks,col="#0000ff60",border=NA,freq=F,main="",xlab="",ylab="")
  abline(v=c(quantile(all,c(0.05,0.95)),mean(all)),col='blue',lty=c(2,2,1))
  abline(v=c(quantile(suby,c(0.05,0.95)),mean(suby)),col='red',lty=c(2,2,1))

  hist(all[selind],breaks=breaks,col="#ff000060",border=NA,add=T,freq=F)
  legend('topright',legend=namey,bty='n',cex=1.2,text.font=2)
  if(!is.na(xlab))mtext(xlab,1,line=2.5)

}


plotOut<-function(Sel=F){

  cols<-mcols[1:length(obj@misc$Mnams)]
  Lind<-rep(T,obj@nl)
  if(Sel)Lind<-select$Lind

  if(Sel&sum(Lind)==0&Calc()==1){

    plot(1,1,col='white',axes=F,xlab="",ylab="",main="")
    legend('center',legend="< No lakes selected >",bty='n',text.col="grey")

  }else{

    multiple<-sum(Lind)>1

    if(!multiple){
      yline<-2.4
      layout(matrix(c(1,4,2,4,3,4),nrow=2))
      par(mai=c(0.3,0.6,0.15,0.05))
      Effort<-matrix(apply(obj@eff[,,,Lind,,drop=F],1,sum),nrow=obj@nmanage)/1000
      barplot(Effort,beside=T,col=cols,border=NA)
      legend('top',legend=obj@misc$Mnams,text.col=cols,border="#99999950")
      mtext("Effort (,000 days)",2,line=yline)
      Cost<-matrix(apply(obj@lxslev[,Lind,,drop=T]*array(rep(obj@Scosts[,Lind,,drop=T],each=obj@nmanage),c(obj@nmanage,obj@nst)),1,sum),nrow=obj@nmanage)/1000
      barplot(Cost,beside=T,col=cols,border=NA)
      mtext("Stock. cost ($k)",2,line=yline)
      EpC<-Effort/Cost
      barplot(EpC,beside=T,col=cols,border=NA)
      mtext("Effort / Cost (day / $)",2,line=yline)
      EbA<-apply(obj@eff[,,,Lind,,drop=T],c(1,3),sum)
      colnames(EbA)<-obj@anam
      barplot(EbA,beside=T,col=cols,border=NA)
      #mtext("Effort by ang. class (,000 days)",2,line=yline)

    }else{
      yline<-2.4
      layout(matrix(c(1,4,2,4,3,4),nrow=2))
      par(mai=c(0.3,0.6,0.15,0.05))
      Effort<-matrix(apply(obj@eff[,,,Lind,,drop=T],1,sum),nrow=obj@nmanage)/1000
      barplot(Effort,beside=T,col=cols,border=NA)
      legend('top',legend=obj@misc$Mnams,text.col=cols,border="#99999950")
      mtext("Effort (,000 days)",2,line=yline)
      Cost<-matrix(apply(obj@lxslev[,Lind,,drop=F]*array(rep(obj@Scosts[,Lind,,drop=F],each=obj@nmanage),c(obj@nmanage,sum(Lind),obj@nst)),1,sum),nrow=obj@nmanage)/1000
      barplot(Cost,beside=T,col=cols,border=NA)
      mtext("Stock. cost ($k)",2,line=yline)
      EpC<-Effort/Cost
      barplot(EpC,beside=T,col=cols,border=NA)
      mtext("Effort / Cost (day / $)",2,line=yline)
      EbA<-apply(obj@eff[,,,Lind,,drop=T],c(1,4),sum)
      colnames(EbA)<-obj@anam
      barplot(EbA,beside=T,col=cols,border=NA)
      mtext("Effort by ang. class (,000 days)",2,line=yline)
    }

  }


}



plotOutComp<-function(Sel=F){

  cols<-mcols[1:length(obj@misc$Mnams)]
  Lind<-rep(T,obj@nl)
  if(Sel)Lind<-select$Lind
  dummy<-sum(manage$lxslev)+sum(manage$lxattr)+select$Lind
  Mref<-match(input$MToComp,obj@misc$Mnams)
  Mindy<-1:length(obj@misc$Mnams)
  Mcomp<-Mindy[!Mindy%in%Mref]
  nc<-length(Mcomp)

  if(Sel&sum(Lind)==0&Calc()==1){

    plot(1,1,col='white',axes=F,xlab="",ylab="",main="")
    legend('center',legend="< No lakes selected >",bty='n',text.col="grey")

  }else{

    multiple<-sum(Lind)>1

    if(!multiple){


    }else{
      yline<-2.4
      layout(matrix(c(1,4,2,4,3,4),nrow=2))
      par(mai=c(0.3,0.6,0.15,0.05))
      Effort_r<-matrix(apply(obj@eff[Mref,,,Lind,,drop=F],1,sum),nrow=1)
      Effort_c<-matrix(apply(obj@eff[Mcomp,,,Lind,,drop=F],1,sum),nrow=nc)
      Effort_d<-as.vector(Effort_c)-as.vector(Effort_r)
      #ylim=c(0,max(1,Effort_d))
      barplot(Effort_d/1000,beside=T,col=cols[Mcomp],border=NA)
      mtext("Effort inc. (,000 days)",2,line=yline)

      Cost_r<-matrix(apply(obj@lxslev[Mref,Lind,,drop=F]*array(rep(obj@Scosts[,Lind,,drop=F],each=1),c(1,sum(Lind),obj@nst)),1,sum),nrow=1)
      Cost_c<-matrix(apply(obj@lxslev[Mcomp,Lind,,drop=F]*array(rep(obj@Scosts[,Lind,,drop=F],each=nc),c(nc,sum(Lind),obj@nst)),1,sum),nrow=nc)
      Cost_d<-as.vector(Cost_c)-as.vector(Cost_r)
      #ylim=c(0,max(1,Cost_d))
      barplot(Cost_d/1000,beside=T,col=cols[Mcomp],border=NA)
      mtext("Stock. cost inc. (,000 $)",2,line=yline)

      EpC<-Effort_d/max(1E-10,Cost_d)

      barplot(EpC,beside=T,col=cols[Mcomp],border=NA)
      mtext("Effort / Cost (day / $)",2,line=yline)

      EbA_r<-apply(obj@eff[Mref,,,Lind,,drop=T],3,sum)
      EbA_c<-apply(obj@eff[Mcomp,1,,Lind,,drop=F],c(1,5),sum)
      EbA_d<-EbA_c-array(rep(EbA_r,each=nc),dim(EbA_c))
      colnames(EbA_d)<-obj@anam
      barplot(EbA_d/1000,beside=T,col=cols[Mcomp],border=NA)
      mtext("Effort inc. by ang. class (,000 days)",2,line=yline)
    }

  }


}


