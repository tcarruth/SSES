
#' Optimize for slope on accessibility values
#'
#' @param par A single parameter estimating the relative impact of the accessibility on overall utility
#' @param obj an object of class 'Landscape'
#' @param nits integer value, the number of iterations for resolving the IFD
#' @param opt logical, return funciton value or object?
#' @param ploty logical, plot the fit as the optimizer runs?
#' @param varind positive real number, the slope of the logit choice (low values are 'choosey' a value of 1 is proportional to utility)
#' @param uprat fraction, When converging on a stable effort distribution this is the contributing fraction of new effort
#' @param log logical, plot log effort?
#' @author T. Carruthers
#' @export opt_acc
opt_acc<-function(par,obj,nits=15,opt=T,ploty=F,varind=0.15,uprat=0.05,log=T){

  obj2<-obj
  obj2@acc_a<-par
  obj2<-calceff(obj=obj2,nits=nits,startE=T,varind=varind,uprat=uprat)
  obj2@exeff[obj2@exeff<0.01]<-0.01
  dif<-obj2@exeff[1,]-apply(obj2@eff[1,1,,,],2,sum)
  dif<-(dif^2)^0.5
  dif<-dif[!is.na(dif)]
  print(par)
  funcy<-sum(dif) #-sum(dnorm(dif,0,0.5,log=T),na.rm=T) # negative log likelihood of log effort difference
  if(ploty)Ecomp(obj2,funcy,log=log,nice=F,lims=range(log(obj2@exeff[1,]),na.rm=T))
  print(funcy)
  if(opt)return(funcy)
  if(!opt)return(obj2)

}

#' Optimize for slope on accessibility values
#'
#' @param par A single parameter estimating the relative impact of the accessibility on overall utility
#' @param obj an object of class 'Landscape'
#' @param nits integer value, the number of iterations for resolving the IFD
#' @param opt logical, return funciton value or object?
#' @param ploty logical, plot the fit as the optimizer runs?
#' @param varind positive real number, the slope of the logit choice (low values are 'choosey' a value of 1 is proportional to utility)
#' @param uprat fraction, When converging on a stable effort distribution this is the contributing fraction of new effort
#' @param log logical, plot log effort?
#' @param quiet logical, suppress outputs while fitting?
#' @author T. Carruthers
#' @export opt_DR_acc
opt_DR_acc<-function(par,obj,nits=15,opt=T,ploty=F,varind=0.15,uprat=0.05,log=T,quiet=T){

  tiny=1E-20
  obj2<-obj
  DR<-exp(par[1])/(1+exp(par[1]))
  obj2@DR<-matrix(rep(DR,obj@na),nrow=1)
  obj2@acc_a<-par[2]
  obj2<-calceff(obj2,nits=nits,startE=T,quiet=quiet)
  obj2@exeff[obj2@exeff<0.01]<-0.01
  dif<-log(obj2@exeff[1,]+tiny)-log(apply(obj2@eff[1,1,,,],2,sum)+tiny)
  print(paste("DR =",DR,"  acc_a =",par[2]))
  funcy<--sum(dnorm(dif,0,0.5,log=T),na.rm=T) # negative log likelihood of log effort difference
  if(ploty)Ecomp(obj2,funcy,nice=F,lims=c(1,10),plottext=F)
  print(funcy)
  if(opt)return(funcy)
  if(!opt)return(obj2)
}



#' Optimize for mean effort per license on a population centre by population centre basis
#'
#' @param par A single parameter estimating the relative impact of the accessibility on overall utility
#' @param obj an object of class 'Landscape'
#' @param nits integer value, the number of iterations for resolving the IFD
#' @param opt logical, return funciton value or object?
#' @param ploty logical, plot the fit as the optimizer runs?
#' @param varind positive real number, the slope of the logit choice (low values are 'choosey' a value of 1 is proportional to utility)
#' @param uprat fraction, When converging on a stable effort distribution this is the contributing fraction of new effort
#' @param log logical, plot log effort?
#' @author T. Carruthers
#' @export opt_apr
opt_apr<-function(par,obj,nits=15,opt=T,ploty=F,varind=0.15,uprat=0.05,log=T){

  pcmulti<-exp(par)
  obj2<-obj
  obj2@apr<-obj2@apr*pcmulti
  obj2<-calceff(obj2,nits=nits,startE=T,varind=varind,uprat=uprat)
  obj2@exeff[obj2@exeff<0.01]<-0.01
  dif<-((obj2@exeff[1,]-apply(obj2@eff[1,1,,,],2,sum))^2)^0.5

  cat("\n")
  cat("--------------------")
  cat("\n")
  print(rbind(substr(obj@pcnam,1,6) ,round(pcmulti*100,2)))
  funcy<-sum(dif)#-sum(dnorm(dif,0,100,log=T),na.rm=T) # negative log likelihood of log effort difference
  funcy<-funcy+sum((par^2)^0.5*1000)#sum(dlnorm(exp(par),0,2,log=T)) # negative log likelihood of multiplicative deviation from couch effect
  if(ploty)Ecomp(obj2,funcy,log=log,nice=F)
  cat(funcy)
  cat("\n")
  if(opt)return(funcy)
  if(!opt)return(obj2)

}


#' Optimize for discard rate and average mean effort multiplier on the landscape object slot apr
#'
#' @param par A single parameter estimating the relative impact of the accessibility on overall utility
#' @param obj an object of class 'Landscape'
#' @param nits integer value, the number of iterations for resolving the IFD
#' @param opt logical, return funciton value or object?
#' @param ploty logical, plot the fit as the optimizer runs?
#' @param varind positive real number, the slope of the logit choice (low values are 'choosey' a value of 1 is proportional to utility)
#' @param uprat fraction, When converging on a stable effort distribution this is the contributing fraction of new effort
#' @author T. Carruthers
#' @export opt_DR_apr
opt_DR_apr<-function(par,obj,nits=200,opt=T,ploty=F,varind=0.15,uprat=0.05){

  DR<-exp(par[1])/(1+exp(par[1]))
  obj2<-obj
  obj2@DR<-matrix(rep(DR,obj@na),nrow=1)
  totmulti<-exp(par[2])
  obj2@apr<-obj2@apr*totmulti
  obj2<-calceff(obj2,nits=nits,varind=varind,uprat=uprat,startE=T,quiet=T)
  dif<-log(obj2@exeff[1,]+tiny)-log(apply(obj2@eff[1,1,,,],2,sum)+tiny)
  print(paste("DR=",DR,"  apr mult =",totmulti))
  funcy<--sum(dnorm(dif,0,0.5,log=T),na.rm=T) #funcy<-funcy-sum(dlnorm(exp(par),0,2,log=T)) # negative log likelihood of multiplicative deviation from couch effect
  if(ploty)Ecomp(obj2,funcy,nice=F,log=T,plottext=F,lim=c(3,10))
  print(funcy)
  if(opt)return(funcy)
  if(!opt)return(obj2)

}
