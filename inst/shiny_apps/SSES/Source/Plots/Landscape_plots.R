
plotLand <- function(dummy=1){

  par(mfrow=c(1,3),mai=c(0.3,0.3,0.05,0.05))
  selind<-obj@longnam%in%input$Lsel

  histplot(obj@lakearea,selind,qs=c(0,0.97),namey="Lake size (ha)")
  histplot(obj@GDD,selind,qs=c(0,0.99),namey="Growing Deg Days")
  mudist<-apply(array(obj@pcsize,c(obj@npc,obj@nl))*obj@pcxl[1,,],2,sum)/sum(obj@pcsize)
  histplot(mudist,selind,qs=c(0,0.99),namey="Mean angler distance (km)")

}

plotMan <-function(dummy=1,Sel=F){

  Lind<-rep(T,obj@nl)
  if(Sel)Lind<-select$Lind

  if(Sel&sum(Lind)==0){

    plot(1,1,col='white',axes=F,xlab="",ylab="",main="")
    legend('center',legend="< No lakes selected >",bty='n',text.col="grey")

  }else{

    temp<-select$Mind
    par(mfrow=c(2,3),mai=c(0.3,0.3,0.05,0.05))
    dat<-apply(manage$lxslev[,Lind,,drop=F],c(1,3),sum)
    colnames(dat)<-obj@stnam
    cols<-mcols[1:length(obj@misc$Mnams)]
    barplot(dat,beside=TRUE,col=cols,border=NA)
    legend('topleft',legend=obj@misc$Mnams,text.col=cols,bty='n')

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
    }
  }

}




histplot<-function(all,selind,qs=c(0.01,0.99),namey=""){

  rng<-quantile(all,qs)
  suby<-all[selind]
  all<-all[all>rng[1]&all<rng[2]]
  suby<-suby[suby>rng[1]&suby<rng[2]]
  breaks<-seq(rng[1],rng[2],length.out=30)
  hist(all,breaks=breaks,col="#0000ff60",border=NA,freq=F,main="",xlab="",ylab="")
  abline(v=c(quantile(all,c(0.05,0.95)),mean(all)),col='blue',lty=c(2,2,1))
  abline(v=c(quantile(suby,c(0.05,0.95)),mean(suby)),col='red',lty=c(2,2,1))

  hist(all[selind],breaks=breaks,col="#ff000060",border=NA,add=T,freq=F)
  legend('topright',legend=namey,bty='n')

}






