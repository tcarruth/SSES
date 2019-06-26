# ==============================================================================================================================================================
# === Landscape manipulation functions =========================================================================================================================================
# ==============================================================================================================================================================

#' Add stochasticity to the landscape object
#'
#' For all landscape characteristics that are uncertain, these parameters are sampled.
#'
#' @param obj an object of class 'Landscape'
#' @param nsim integer: the number of simulations (stochastic samples of parameters)
#' @author T. Carruthers
#' @export Stoch
Stoch<-function(obj,nsim){

  # Dimensions
  nl<-obj@nl
  obj@nsim<-nsim
  npc<-obj@npc
  nattr<-obj@nattr
  na<-obj@na
  nage<-obj@nage
  nst<-obj@nst
  ncat<-obj@ncat

  # Create an nsim dimension to each attribute (the first dimension allowing for easy parallel computing)
  obj@pcsize<-array(geterr(nsim*npc,fac2num(obj@errs$pcsize[2]))*rep(obj@pcsize,each=nsim),dim=c(nsim,npc))
  #obj@pcxl<-array(geterr(nsim*npc*nl,fac2num(obj@errs$pcxl[2]))*rep(obj@pcxl,each=nsim),dim=c(nsim,npc,nl))
  errvec<-rep(geterr(nsim,fac2num(obj@errs$GDDamong[2])),nl)*rep(geterr(nl,fac2num(obj@errs$GDDacross[2])),each=nsim)
  obj@GDD<-array(errvec*rep(obj@GDD,each=nsim),c(nsim,nl))
  errvec<-rep(geterr(nsim,fac2num(obj@errs$TDSamong[2])),nl)*rep(geterr(nl,fac2num(obj@errs$TDSacross[2])),each=nsim)
  obj@TDS<-array(errvec*rep(obj@TDS,each=nsim),c(nsim,nl))

  obj@pcxa<-getmvlogerr(obj@pcxa,fac2num(obj@errs$pcxa[2]),nsim)
  obj@costs<-array(geterr(nsim*nl*(nattr-3)*max(fac2num(obj@attr[1,4:obj@nattr])),fac2num(obj@errs$cost[2]))*
                     rep(obj@costs,each=nsim),dim=c(nsim,nl,nattr-1,ncat))
  obj@Scosts<-array(geterr(nsim*nst*nl,fac2num(obj@errs$cost[2]))*
                      rep(obj@Scosts,each=nsim),dim=c(nsim,nl,nst))
  obj@effval<-array(geterr(nsim*na,fac2num(obj@errs$effval[2]))*rep(obj@effval,each=nsim),dim=c(nsim,na))
  obj@licval<-array(geterr(nsim,fac2num(obj@errs$licval[2]))*obj@licval,dim=c(nsim,1))
  obj@aq<-array(geterr(nsim*na,fac2num(obj@errs$aq[2]))*rep(obj@aq,each=nsim),dim=c(nsim,na))

  nofish<-1-obj@apr
  aprerr<-rnorm(nsim*na,0,fac2num(obj@errs$apr[2]))+log(rep(obj@apr,each=nsim))
  obj@apr<-array(exp(aprerr)/(exp(aprerr)+rep(nofish,each=nsim)),dim=c(nsim,na))

  # error in the way in which anglers are attracted to lake characteristics
  lnind<-(1:nattr)[obj@attr[2,]=="lognorm"]
  mvind<-(1:nattr)[obj@attr[2,]=="mvlogistic"]
  nln<-length(lnind)
  nmv<-length(mvind)
  obj@axattr<-array(rep(obj@axattr,each=nsim),dim=c(nsim,dim(obj@axattr)[2:4]))
  obj@axattr[,,lnind,]<-obj@axattr[,,lnind,]*geterr(nsim*na*nln*max(fac2num(obj@attr[1,])),rep(fac2num(obj@attr[3,lnind]),each=nsim*na))
  obj@axattr[,,mvind,]<-obj@axattr[,,mvind,]+rnorm(nsim*na*nmv*max(fac2num(obj@attr[1,])),0,rep(fac2num(obj@attr[3,mvind]),each=nsim*na))

  # error in the population variables
  npe<-ncol(obj@poperr)
  nind<-(1:npe)[obj@poperr[1,]=="norm"]
  lnind<-(1:npe)[obj@poperr[1,]=="lognorm"]
  nnm<-length(nind)
  nlog<-length(lnind)
  popvals<-array(NA,dim=c(nsim,ncol(obj@poperr),nst))
  popvals[,nind,]<-rnorm(nsim*nnm*nst,1,rep(fac2num(obj@poperr[2,nind]),each=nsim*nst))*rep(obj@popval[,nind,],each=nsim)
  popvals[,lnind,]<-getbias(nsim*nlog*nst,rep(fac2num(obj@poperr[2,lnind]),each=nsim*nst))*rep(obj@popval[,lnind,],each=nsim)
  #popvals<-as.data.frame(popvals)
  #names(popvals)<-names(obj@popval)
  obj@popval<-popvals

  obj@Mage<-array(getbias(nsim*nage*nst,fac2num(obj@errs$Mage[2]))*rep(obj@Mage,each=nsim),dim=c(nsim,nage,nst))
  obj

}
