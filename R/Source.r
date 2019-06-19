
# ================= LSM =========================================================================================================
# ================= Source code for landscape scale simulator ========================================================

# Tom Carruthers t.carruther@fisheries.ubc.ca
# v1.13 beta February 2015


#options(warn=-1)
# Install required packages ---------------------------------------------------------------------------------------------------------------
if (!require('maps'))    install.packages('maps',repos='http://cran.stat.sfu.ca')
if (!require('mapdata')) install.packages('mapdata',repos='http://cran.stat.sfu.ca')
if (!require('mapplots'))    install.packages('mapplots',repos='http://cran.stat.sfu.ca')
if (!require('animation')) install.packages('animation',repos='http://cran.stat.sfu.ca')
if (!require('vegan'))install.packages('vegan',repos='http://cran.stat.sfu.ca')
if (!require('wordcloud'))install.packages('vegan',repos='http://cran.stat.sfu.ca')

# Load required packages -----------------------------------------------------------------------------------------------------------------
library(maps)
library(mapdata)
library(mapplots)
library(animation)
library(vegan)
library(wordcloud)
#options(warn=1)
print("test")

tiny=1E-20

# =================================================================================
# The effort calculator ===========================================================
calceff<-function(obj,nits=15,couch=F,startE=F,fc=1,varind=0.2,uprat=0.1,GModel="New Lester",quiet=F){
  
  options(warn=1)
  
  conv<-array(NA,dim=c(50,nits))
  convind<-cbind(rep(1,50),rep(1,50),sample(1:obj@npc,50,replace=T),sample(1:obj@nl,50,replace=T),sample(1:obj@na,50,replace=T))
  Econv<-array(NA,dim=c(obj@nl,nits))
  
  rampup<-rep(1,nits)
  
  nsim<-obj@nsim
  nman<-obj@nmanage
  pcsize<-obj@pcsize
  pcxl<-obj@pcxl
  pcxa<-obj@pcxa
  axattr<-obj@axattr
  attrtype<-obj@attr[1,]
  lxattr<-obj@lxattr
  ncat<-obj@ncat
  nattr<-obj@nattr
  attrty<-fac2num(obj@attr[1,5:nattr])
  npc<-obj@npc
  nl<-obj@nl
  na<-obj@na
  nage<-obj@nage
  nst<-obj@nst
  lmt<-obj@lm
  FTroutAng<-obj@FTroutAng
  lxslev<-obj@lxslev
  lxstocked<-array(as.integer(lxslev>0),dim(lxslev))
  couchlev<-obj@couch
  
  mspla<-as.matrix(expand.grid(1:nman,1:nsim,1:npc,1:nl,1:na))
  msla<-mspla[,c(1,2,4,5)]
  spla<-mspla[,2:5]
  spl<-mspla[,2:4]
  mpl<-mspla[,c(1,3,4)]
  msal<-mspla[,c(1,2,5,4)]
  mspa<-mspla[,c(1,2,3,5)]
  sp<-mspla[,2:3]
  spa<-mspla[,c(2,3,5)]
  sa<-mspla[,c(2,5)]
  l<-mspla[,4]
  sl<-mspla[,c(2,4)]
  p<-mspla[,3]
  aaa<-mspla[,5]
  
  dis<-array(NA,dim=c(nman,nsim,npc,nl,na))
  dis[mspla]<-pcxl[mpl]
  
  # The gravity of angler types to each lake based on categorical lake attributes
  msalc<-as.matrix(expand.grid(1:nman,1:nsim,1:na,1:nl,1:length(attrty)))  # master index matrix. management option x sim x angler type x lake x attribute
  attrp<-array(NA,dim=c(nman,nsim,na,nl,nattr-4))
  
  latt<-msalc[,c(1,4,5)] # locate the management option lake x management attribute
  lcat<-lxattr[latt]     # what is the level of the categorical effect?
  
  aatt<-cbind(msalc[,c(2,3,5)],lcat)   # locate the sim, angler, category by level
  attrp[msalc]<-axattr[aatt]  # what is numerical effect of this level of this category, sim and angler?
  
  sigattrp<-apply(attrp,1:4,sum) # by man, sim, angler type and lake what does this add up to?
  
  # The gravity due to travel time of anglers in a population centre to different lakes
  vals<-array(NA,dim=c(nman,nsim,npc,nl,na))
  x<-as.vector(dis)
  a<-as.factor(rep(1:na,each=nman*nsim*npc*nl))
  preddat<-data.frame(a,x)
  disval<-array(predict(lmt[[3]],newdata=preddat),dim=c(nman,nsim,npc,nl,na))
   
  accval<-obj@acc
  vals[mspla]<-disval[mspla]+obj@acc_a*accval[sl]+sigattrp[msal]
  
  vals2<-array(NA,dim=c(nman,nsim,npc,nl,na))
  
  vals2[mspla]<-exp(vals[mspla])      # ignore average catch size indSS,  # add the catch rate effect make this zero in first iteration (ignore size)
  sig2<-apply(vals2,c(1,2,3,5),sum)   # total attraction across lakes
  
  if(couch|startE){
    E<-obj@eff
  }else{
    E<-array(0,dim=c(nman,nsim,npc,nl,na))
    Ebysz<-obj@lakearea*apply(obj@lxslev[1,,],1,sum)
    Ebysz<-Ebysz/mean(Ebysz)
    E[mspla]<-pcsize[sp]*pcxa[spa]*vals2[mspla]/sig2[mspa]*unlist(obj@apr[spa])*FTroutAng[p]*Ebysz[l]
  }
 
  Eold<-E
  
  conv[,1]<-E[convind]
  if(dim(E)[3]>1)Econv[,1]<-apply(E[1,1,,,],2,sum)
  if(dim(E)[3]==1)Econv[,1]<-apply(E[1,1,,,],1,sum)
  #               m s l a subd
  sigE<-apply(E,c(1,2,4,5),sum)#tomapply(x=E,obj@nmanage,obj@nsim)
  Fmslat<-as.matrix(expand.grid(1:nman,1:nsim,1:nl,1:na,1:nst))
  Fmsla<-Fmslat[,c(1,2,3,4)]
  Fsa<-Fmslat[,c(2,4)]
  prind<-match("prmort",names(obj@poperr))
  inds<-cbind(Fmslat[,2],rep(prind,nrow(Fmslat)),Fmslat[,5])
  Fl<-Fmslat[,3] 
  
  FF<-FM<-array(0,dim=c(nman,nsim,nl,na,nst))
  CR<-array(0,dim=c(nman,nsim,nl,na,nst,nage))
  Rel<-array(0,dim=c(nman,nsim,nl,na))
  FF[Fmslat]<-sigE[Fmsla]*unlist(obj@aq[Fsa])/obj@lakearea[Fl]
  FM[Fmslat]<-(1-obj@DR[Fsa])*sigE[Fmsla]*unlist(obj@aq[Fsa])/obj@lakearea[Fl]
  FFmsl<-apply(FF,1:3,sum)
  FMmsl<-apply(FM,1:3,sum)
  
  N<-array(0,dim=c(nman,nsim,nl,nst,nage))  # N over all age classes
  L<-array(0,dim=c(nman,nsim,nl,nst,nage))  # L over all age classes
  Dens<-array(0,dim=c(nman,nsim,nl))        # Fish density over all types
  
  sigM<-obj@Mage/2  #approximation to the mid season survival rate   # change this to a Baranov - doesn't make much difference tho!
  for(i in 2:nage) sigM[,i,]<-apply(array(obj@Mage[,1:(i-1),],dim=c(nsim,i-1,nst)),c(1,3),sum)+obj@Mage[,i,]/2
  
  presatage<-array(1,c(nst,nage))
  presatage[obj@aclass==1,1]<-0
  presatage[obj@aclass==2,1:2]<-0
  
  Fmulti<-c(0,0.5,rep(1,obj@nage-2))#c(0,seq(0.5,nage-1.5,length.out=nage-1)) # no fishing mortality rate in age class 0
  Fmulti<-cumsum(obj@sel*Fmulti)
  
  # Some indexing
  indN<-as.matrix(expand.grid(1:nman,1:nsim,1:nl,1:nst,1:nage))
  inda2<-indN[,5]
  indmsl<-indN[,1:3]
  indsat<-indN[,c(2,5,4)]
  indta<-indN[,4:5]
  indmlt<-indN[,c(1,3,4)]
  indst<-indN[,c(2,4)]
  indsl<-indN[,2:3]
  indt<-indN[,4]
  indMage<-cbind(indN[,2],rep(1,nrow(indN)),indN[,4])
  indmlt<-indN[,c(1,3,4)]
  indNl<-indN[,3]
  indNs<-indN[,2]
  indmslt<-indN[,1:4]
  indsa<-indN[,c(2,5)]
  
  N[indN]<-presatage[indta]*obj@lxslev[indmlt]*exp(-Fmulti[inda2]*FMmsl[indmsl]-sigM[indsat])
  sumN<-apply(N,1:4,sum)
  
  agemulti<-(1:nage)-0.5 # multiplier for calculating cumulative mortality rate at age
  
  # growth rate indexing   #gmax alpha beta gamma delta peta theta	prmort
  gmax<-array(obj@popval[,match("gmax",names(obj@poperr)),],dim=c(nsim,nst))
  alpha<-array(obj@popval[,match("alpha",names(obj@poperr)),],dim=c(nsim,nst))
  bet<-array(obj@popval[,match("beta",names(obj@poperr)),],dim=c(nsim,nst))
  gamm<-array(obj@popval[,match("gamma",names(obj@poperr)),],dim=c(nsim,nst))
  delta<-array(obj@popval[,match("delta",names(obj@poperr)),],dim=c(nsim,nst))
  peta<-array(obj@popval[,match("peta",names(obj@poperr)),],dim=c(nsim,nst))
  thetai<-array(obj@popval[,match("theta",names(obj@poperr)),],dim=c(nsim,nst))
  
  # Initial density estimate the yearling equivalent 
  Dens<-array(NA,dim=c(nman,nsim,nl,nst))
  Dens[indmslt]<-(obj@lxslev[indmlt]*(obj@stlen[indt]^2*10^-6)*exp(-obj@Mage[indMage]))/obj@lakearea[indNl]
  Dens<-apply(Dens,1:3,sum)     # sum dens over all fish for initial calculation
  
  AA<-(obj@aclass[indt]+agemulti[inda2])*obj@GDD[indsl]*0.001   
  
  if(GModel=="Lester1"){
    
    GL<-exp(gmax[indst]-alpha[indst]*log(obj@stwt[indt])*Dens[indmsl]-bet[indst]*log(obj@stwt[indt]))
    LA<-(GL*obj@stlen[indt]*AA)+obj@stlen[indt]
    hh = obj@stlen[indt] * GL #exp(gmax[indst]-alpha[indst]*log(obj@stwt[indt])*Dens[indmsl]-bet[indst]*log(obj@stwt[indt]))  #immature growth increment
    TT<-gamm[indst]*hh^(-delta[indst])-obj@GDD[indsl]*0.001
    gg<-peta[indst]*hh+thetai[indst]
    gg[gg>0.99]<-0.99
    
  }else if(GModel=="Lester2"){
    
    hh=gmax[indst]*exp(-alpha[indst]*Dens[indmsl]) *obj@GDD[indsl]*10^-3 
    TT=gamm[indst]*hh^-delta[indst]
    LA<-hh*AA+obj@stlen[indt]
    gg<-peta[indst]*hh+thetai[indst]
    gg[gg>0.99]<-0.99
    
  }else if(GModel=="New Lester"){
    
    h_T=gmax[indst]*obj@stlen[indt]^bet[indst]*exp(-alpha[indst]*Dens[indmsl])
    hh=h_T/(obj@GDD[indsl]*0.001)
    TT=delta[indst]*hh^-gamm[indst]
    gg<-1.18*(1-exp(-0.2)) # fixed g parameter
    LA<-hh*AA+obj@stlen[indt]
    
  }  
  
  Linf<-(3*hh)/gg
  K<-log(1+gg/3)
  t0<-TT+log(1-(gg*(hh*TT+obj@stlen[indt])/(3*hh)))/log(1+gg/3)
  cond2<-t0>(hh*TT+obj@stlen[indt])
  t0[cond2]<-hh*TT+obj@stlen[indt]
  
  LA2<-Linf*(1-exp(-K*(AA-t0)))
  L[indN]<-LA
  L[indN[AA>TT,]]<-LA2[AA>TT]
  
  if(!couch|!startE)print(1)
  flush.console()
  
  # Catch rate indexing
  indCR<-as.matrix(expand.grid(1:nman,1:nsim,1:nl,1:na,1:nst,1:nage))
  CRmsla<-indCR[,1:4]
  CRa<-indCR[,c(2,4)]
  CRl<-indCR[,3]
  CRml<-indCR[,c(1,3)]
  CRmsl<-indCR[,1:3]
  CRsat<-indCR[,c(2,4,5)]
  CRage<-indCR[,6]
  CRmsltage<-indCR[,c(1:3,5:6)]
  CRsage<-indCR[,c(2,6)]
  
  V<-array(0,dim=c(nman,nsim,nl,nst,nage))
  Fmod<-array(0,dim=c(nman,nsim,nl,na))
  
  V[indN]<-N[indN]*obj@sel[indsa]
  AvS<-apply((L*V)/array(rep(apply(V,1:3,sum),nst*nage),dim=c(nman,nsim,nl,nst,nage)),1:3,sum)
  
  # set up size and F modifier (according to take limit) indexing
  indSZ<-as.matrix(expand.grid(1:nman,1:nsim,1:nl,1:na))
  SZml<-indSZ[,c(1,3)]  
  
  # set up fishing mortality rate indexing
  indF<-as.matrix(expand.grid(1:nman,1:nsim,1:nl,1:na,1:nst))
  indFE<-indF[,1:4]
  indFs<-indF[,2]
  indFl<-indF[,3]
  indFsa<-indF[,c(2,4)]
  indFmsla<-indF[,1:4]
  
  take<-matrix(obj@lxattr[,,match('take',names(obj@attr))-(obj@nattr-dim(obj@lxattr)[3])],nrow=nman)
  
  for(itn in 2:nits){
    
    valsAC<-array(NA,dim=c(nman,nsim,nl,na))
    CR[indCR]<-(N[CRmsltage]*exp((-Fmulti[CRage]*FMmsl[CRmsl]-sigM[CRsat])/2))/obj@lakearea[CRl]*unlist(obj@aq[CRa])*obj@sel[CRsage]+0.00001
    CRsum<-apply(CR,1:4,sum)
    Rel[CRmsla]<-ppois(obj@BagLim[CRml],CRsum[CRmsla],lower.tail=F)# fraction released
    valsCR<-array(NA,dim=c(nman,nsim,nl,na))
    
    for(aa in 1:obj@na){
      
      RBstk<-1:obj@nst#grep("RB",obj@stnam)
      x<-as.vector(apply(array(CR[,,,aa,RBstk,],c(nman,nsim,nl,length(RBstk),nage)),1:3,sum))
      preddatCR<-data.frame(x)
      valsCR[,,,aa]<-predict(lmt[[2]][[aa]],newdata=preddatCR)  
      
    }
    
    # Expected crowding ------
    Esum<-apply(Eold,c(1,2,4),sum)
    x<-Esum/rep(obj@lakearea,nman*nsim)/90 # guess at effort density for crowding calc
    nudat<-as.data.frame(cbind(rep(x,na),a<-rep(1:na,each=length(x))))
    names(nudat)<-c("x","a")
    nudat$a<-as.factor(nudat$a)
    pred<-predict(lmt[[1]],newdat=nudat)
    valsCROW<-array(pred,c(nman,nsim,nl,na))
   
    # Expected size
    V[indN]<-N[indN]*obj@sel[indsa]
    AvSo<-apply((L*V)/array(rep(apply(V,1:3,sum),nst*nage),dim=c(nman,nsim,nl,nst,nage)),1:3,sum)
    
    valsSZ<-array(NA,dim=c(nman,nsim,nl,na))
    aa<-as.factor(rep(1:na,each=nman*nsim*nl))
    x<-rep(AvSo,na)
    preddatSZ<-data.frame(x/25.4,aa) # convert mm (growth model) to inches (HD)
    names(preddatSZ)<-c("x","a")
    valsSZ[indSZ]<-predict(lmt[[4]],newdata=preddatSZ)  
    
    msl2<-mspla[,c(1,2,4)]
    vals2[mspla]<-exp((valsSZ[msla]+vals[mspla]+valsCR[msla]+valsCROW[msla])/varind)    
    sig2<-apply(vals2,c(1,2,3,5),sum,na.rm=T)   # total attraction across lakes
    
    if(couch){
      E[mspla]<-pcsize[sp]*pcxa[spa]*vals2[mspla]/(sig2[mspa]+couchlev[mspa])*FTroutAng[p]*unlist(obj@maxdays[sa])
    }else{  
      E[mspla]<-pcsize[sp]*pcxa[spa]*vals2[mspla]/sig2[mspa]*unlist(obj@apr[spa])*FTroutAng[p]
    }
    
    #availE<-obj@FTroutAng*obj@pcxa[1,,]*array(rep(unlist(obj@maxdays[1,]),each=obj@npc),dim=dim(obj@pcxa[1,,]))*obj@pcsize[1,]
    #UT<-apply(E[1,1,,,],c(1,3),sum)/availE
    
    
    E[is.na(E)]<-0   
    conv[,itn]<-E[convind]
    if(dim(E)[3]>1)Econv[,itn]<-apply(E[1,1,,,],2,sum)
    if(dim(E)[3]==1)Econv[,itn]<-apply(E[1,1,,,],1,sum)
    
    sigE<-apply(uprat*E+(1-uprat)*Eold,c(1,2,4,5),sum)
    Eold<-uprat*E+(1-uprat)*Eold
    FF[indF]<-(sigE[indFE]*unlist(obj@aq[indFsa]))/obj@lakearea[indFl] # total caught
    FM[indF]<-(1-obj@DR[indFsa])*(1-Rel[indFmsla])*FF[indF]+((1-(1-obj@DR[indFsa])*(1-Rel[indFmsla]))*obj@DD[indFs])*FF[indF] # subject to discard rate DR and baglimit
    
    FFmsl<-apply(FF,1:3,sum)
    FMmsl<-apply(FM,1:3,sum)
    
    N[indN]<-presatage[indta]*obj@lxslev[indmlt]*exp(-sigM[indsat]-Fmulti[inda2]*FMmsl[indmsl])#-sigM[indsat]) # knife-edge vulnerability @ age 1
    
    Dens<-N*(L^2)*10^-6
    Dens[indN]<-Dens[indN]/obj@lakearea[indNl]
    Dens<-apply(Dens,1:3,sum)
    
    if(GModel=="Lester1"){
      
      GL<-exp(gmax[indst]-alpha[indst]*log(obj@stwt[indt])*Dens[indmsl]-bet[indst]*log(obj@stwt[indt]))
      LA<-(GL*obj@stlen[indt]*AA)+obj@stlen[indt]
      hh = obj@stlen[indt] * GL 
      TT<-gamm[indst]*hh^(-delta[indst])-obj@GDD[indsl]*0.001
      gg<-peta[indst]*hh+thetai[indst]
      gg[gg>0.99]<-0.99
      
    } else if(GModel=="Lester2"){
      
      hh=gmax[indst]*exp(-alpha[indst]*Dens[indmsl]) *obj@GDD[indsl]*10^-3 
      TT=gamm[indst]*hh^-delta[indst]
      LA<-hh*AA+obj@stlen[indt]
      gg<-peta[indst]*hh+thetai[indst]
      gg[gg>0.99]<-0.99
      
    } else if(GModel=="New Lester"){
      
      h_T=gmax[indst]*obj@stlen[indt]^bet[indst]*exp(-alpha[indst]*Dens[indmsl])
      hh=h_T/(obj@GDD[indsl]*0.001)
      TT=delta[indst]*hh^-gamm[indst]
      gg<-1.18*(1-exp(-0.2))
      LA<-hh*AA+obj@stlen[indt]
      
    }  
    
    Linf<-(3*hh)/gg
    K<-log(1+gg/3)
    trial<-1-(gg*(hh*TT+obj@stlen[indt])/(3*hh))
    trial[trial<0.2]<-0.2
    t0<-TT+log(trial)/log(1+gg/3)
    cond2<-t0>(hh*TT+obj@stlen[indt]) # constraints on t0 from ground truthing workshop Penticton 2015
    t0[cond2]<-hh*TT+obj@stlen[indt]
    
    inter<-AA-t0
    LA2<-Linf*(1-exp(-K*(inter)))
    L[indN]<-LA
    L[indN[AA>TT,]]<-LA2[AA>TT]
    
    if(!quiet)print(itn)
    flush.console()
    
  }
  
  obj@conv<-conv
  obj@Econv<-Econv
  obj@U<-vals2
  
  options(warn=1)
  obj@eff<-E
  obj@sigeff<-apply(obj@eff,c(1,2,4),sum)
  obj@avs<-AvSo
  obj@cr<-apply(CR,1:4,sum)/4
  obj<-calcCB(obj)
  obj
  
}


inspect<-function(obj,lakename=1,Frange=c(0,1.5),nF=10,nits=20,map=T,GModel="New Lester"){
  
  i<-NA
  if(class(lakename)%in%c("numeric","integer")){
    i<-lakename
  }else{
    i<-grep(lakename,obj@lakenam,ignore.case=T)
    if(length(i)==0)i<-grep(lakename,obj@longnam,ignore.case=T)
  }  
  
  if(length(i)==0){
    print(paste('Could not match the lakename:',lakename,"in obj@lakenam or obj@longnam"))
    stop()
  }
  
  if(class(lakename)%in%c("numeric","integer")){
    if(lakename>obj@nl){
      print(paste('Index:',i,"is too long. The number of lakes in the object Obj@nl is:",obj@nl))
      stop()
    }}
  
  nage<-obj@nage
  na<-obj@na
  nst<-obj@nst
  lxslev<-obj@lxslev[1,i,]
  stlen<-obj@stlen
  stwt<-obj@stwt
  Mage<-obj@Mage[1,,]
  lakearea<-obj@lakearea[i]
  G<-obj@GDD[1,i]
  sigM<-Mage/2  #approximation to the mid season survival rate   # change this to a Baranov - doesn't make much difference tho!
  for(h in 2:nage) sigM[h,]<-apply(matrix(Mage[1:(h-1),],nrow=h-1),2,sum)+Mage[h,]/2
  Fs<-seq(Frange[1],Frange[2],length.out=nF)
  Fmulti<-c(0,seq(0.5,nage-1.5,length.out=nage-1)) # no fishing mortality rate in age class 0
  agemulti<-(1:nage)-0.5
  presatage<-array(1,c(nst,nage))
  presatage[obj@aclass==1,1]<-0
  N<-hh<-gg<-V<-D<-Qa<-w<-L<-LA<-LA2<-array(0,c(nF,nst,nage))
  
  
  gmax<-obj@popval[1,match("gmax",names(obj@poperr)),]
  alpha<-obj@popval[1,match("alpha",names(obj@poperr)),]
  beta<-obj@popval[1,match("beta",names(obj@poperr)),]
  gamma<-obj@popval[,match("gamma",names(obj@poperr)),]
  delta<-obj@popval[,match("delta",names(obj@poperr)),]
  phi<-obj@popval[,match("peta",names(obj@poperr)),]
  theta<-obj@popval[,match("theta",names(obj@poperr)),]
  
  indN<-as.matrix(expand.grid(1:nF,1:nst,1:nage))
  inda<-indN[,3]
  indat<-indN[,c(3,2)]
  indta<-indN[,2:3]
  indt<-indN[,2]
  indF<-indN[,1]
  indFt<-indN[,1:2]
  indtF<-indN[,2:1]
  
  Qa[indN]<-agemulti[inda]*G*0.001   
  
  N[indN]<-presatage[indta]*lxslev[indt]*exp(-Fmulti[inda]*Fs[indF]-sigM[indat])
  
  L[indN]<-rep(1000,nst)[indt]*(1-exp(-rep(0.1,nst)[indt]*(Qa[indN]-rep(-0.5,nst)[indt])))*1.3
  
  conv<-array(NA,c(nits,nF))
  
  #D[indN]<-(lxslev[indt]*(stlen[indt]^2*10^-6)*exp(-Fmulti[inda]*Fs[indF]-sigM[indat]))/lakearea
  for(j in 1:nits){
    
    D<-(N*(L^2)*10^-6)/lakearea
    sigD<-apply(D,1,sum)     # sum dens over all fish for initial calculation
    
    if(GModel=="Lester1"){
      w[indN]<-exp(gmax[indt]-alpha[indt]*log(stwt[indt])*sigD[indF]-beta[indt]*log(stwt[indt]))
      LA[indN]<-(w*stlen[indt]*Qa)+stlen[indt] #Length_I = I_Avec * LS * exp(Gmax-alpha*log(WS)*D-beta*log(WS)) + LS #This is equation (1) rearragned for LA 
      hh = obj@stlen[indt] * w 
      TT<-gamma[indt]*hh^(-delta[indt])-G*0.001
      gg<-phi[indt]*hh+theta[indt]
      gg[gg>0.99]<-0.99
      
    }else if(GModel=="Lester2"){
      
      #hh=gmax*obj@stlen[indt]^beta[indst]*exp(-alpha[indst]*Dens[indmsl])
      hh=gmax[indt]*exp(-alpha[indt]*sigD[indF]) *G*10^-3 
      TT=gamm[indt]*hh^-delta[indt]
      LA<-hh*Qa+obj@stlen[indt]
      gg<-peta[indt]*hh+thetai[indt]
      gg[gg>0.99]<-0.99
      
    }else if(GModel =="New Lester"){
      
      h_T=gmax[indt]*obj@stlen[indt]^beta[indt]*exp(-alpha[indt]*sigD[indF])
      hh[indN]=h_T/(G*0.001)
      TT=delta[indt]*hh^-gamma[indt]
      gg<-1.18*(1-exp(-0.2))
      LA<-hh*Qa+obj@stlen[indt]
      
    }  
    
    
    Linf<-(3*hh)/gg
    K<-log(1+gg/3)
    
    trial<-1-(gg*(hh*TT+obj@stlen[indt])/(3*hh))
    trial[trial<0.2]<-0.2
    t0<-TT+log(trial)/log(1+gg/3)
    
    #cond<-t0<0
    #t0[cond]<-0
    cond2<-t0>(hh*TT+obj@stlen[indt]) # constraints on t0 from ground truthing workshop Penticton 2015
    t0[cond2]<-hh*TT+obj@stlen[indt]
    
    LA2[indN]<-Linf*(1-exp(-K*(Qa[indN]-t0)))
    L<-LA
    cond<-Qa[indN]>TT
    L[indN[cond,]]<-LA2[indN[cond,]]
    
    
    #print(Linf[1,])
    conv[j,]<-sigD#Linf[,ceiling(nF/2)]
    
  }
  
  sel<-c(0,rep(1,nage-1))
  V[indN]<-N[indN]*sel[inda]
  AvS<-apply(L*V,1:2,sum)/apply(V,1:2,sum) 
  
  eff<-apply(obj@eff[1,1,,i,],2,sum)
  FF<-eff*unlist(obj@aq)/lakearea
  
  par(mfrow=c(3,2),mai=c(0.7,0.7,0.1,0.05),omi=c(0.05,0.05,0.2,0.01))
  conv<-conv/rep(conv[nits,],each=nits)
  matplot(conv,ylim=range(c(conv,0.98,1.02)),type='l',xlab="Iteration",ylab="Density of fish in lake")
  
  cols<-c('red','green','blue','black')
  
  matplot(Fs,Linf[,,1],type='l',col=cols,xlab="Fishing mortality rate",ylab="Linf (mm)")
  legend('topright',legend=obj@stnam,text.col=cols,bty='n')
  legend('right',legend="Pred. F",text.col="#99999990",bty='n')
  
  abline(v=sum(FF),col="#99999940",lwd=2)
  
  matplot(Fs,t0[,,1],type='l',col=cols,xlab="Fishing mortality rate",ylab="t0 (yrs)")
  legend('bottomright',legend=obj@stnam,text.col=cols,bty='n')
  legend('right',legend="Pred. F",text.col="#99999990",bty='n')
  abline(v=sum(FF),col="#99999940",lwd=2)
  
  matplot(Fs,apply(D,1:2,sum),type='l',col=cols,xlab="Fishing mortality rate",ylab="Density (mm sq h-1)")
  legend('topright',legend=obj@stnam,text.col=cols,bty='n')
  legend('right',legend="Pred. F",text.col="#99999990",bty='n')
  abline(v=sum(FF),col="#99999940",lwd=2)
  
  # predicted catch rate
  CR<-array(NA,c(na,nF,nst,nage))
  id<-as.matrix(expand.grid(1:na,1:nF,1:nst,1:nage))
  idang<-id[,1]
  idfta<-id[,2:4]
  ida<-id[,4]
  idF<-id[,2]
  idat<-id[,c(1,3)]
  
  CR[id]<-(N[idfta]*exp((-Fmulti[ida]*Fs[idF]-sigM[idat])/2))/lakearea*unlist(obj@aq[idang])*sel[ida]+0.001
  CRagg<-apply(CR,1:2,sum)
  
  matplot(Fs,t(CRagg),type='l',col=cols,xlab="Fishing mortality rate",ylab="Catch rate (fish per day)")
  legend('topright',legend=obj@anam,text.col=cols,bty='n')
  abline(v=FF,col=cols)
  abline(v=sum(FF),col="#99999940",lwd=2)
  legend('right',legend="Pred. F",text.col="#99999990",bty='n')
  
  matplot(Fs,AvS,type='l',col=cols,xlab="Fishing mortality rate",ylab="Average size of catch (mm)")
  legend('topright',legend=obj@stnam,text.col=cols,bty='n')
  #abline(v=FF,col=cols)
  abline(v=sum(FF),col="#99999940",lwd=2)
  
  effa<-apply(obj@eff[1,1,,i,],2,sum)
  cra<-obj@cr[1,1,i,]
  
  mat<-data.frame(t(matrix(
    c(obj@exeff[1,i],obj@sigeff[1,1,i]     ,effa,
      obj@excr[1,i],sum(cra*effa)/sum(effa),cra,
      obj@exavs[1,i],obj@avs[1,1,i],rep(obj@avs[1,1,i],obj@na)),nrow=2+obj@na)))
  row.names(mat)<-c("Effort","Catch rate","Size")
  names(mat)<-c("Observed","Predicted",obj@anam)
  
  mtext(paste(obj@longnam[i],obj@lakenam[i],paste("index ",i),sep=" - "),side=3,line=-0.3,outer=T,col='blue')
  
  
  # Now calculate utilites
  
  pcsize<-obj@pcsize
  pcxl<-obj@pcxl
  pcxa<-obj@pcxa
  axattr<-obj@axattr
  attrtype<-obj@attr[1,]
  lxattr<-obj@lxattr
  ncat<-obj@ncat
  nattr<-obj@nattr
  attrty<-fac2num(obj@attr[1,5:nattr])
  npc<-obj@npc
  na<-obj@na
  nl<-obj@nl
  nage<-obj@nage
  nst<-obj@nst
  lmt<-obj@lm
  FTroutAng<-obj@FTroutAng
  lxslev<-obj@lxslev
  lxstocked<-array(as.integer(lxslev>0),dim(lxslev))
  couchlev<-obj@couch
  nman=1
  nsim=1
  
  
  mspla<-as.matrix(expand.grid(1:nman,1:nsim,1:npc,1:nl,1:na))
  msla<-mspla[,c(1,2,4,5)]
  spla<-mspla[,2:5]
  spl<-mspla[,2:4]
  mpl<-mspla[,c(1,3,4)]
  msal<-mspla[,c(1,2,5,4)]
  mspa<-mspla[,c(1,2,3,5)]
  sp<-mspla[,2:3]
  spa<-mspla[,c(2,3,5)]
  sa<-mspla[,c(2,5)]
  l<-mspla[,4]
  p<-mspla[,3]
  aaa<-mspla[,5]
  
  dis<-array(NA,dim=c(nman,nsim,npc,nl,na))
  dis[mspla]<-pcxl[mpl]
  
  # The gravity of angler types to each lake based on categorical lake attributes
  msalc<-as.matrix(expand.grid(1:nman,1:nsim,1:na,1:nl,1:length(attrty)))  # master index matrix. management option x sim x angler type x lake x attribute
  attrp<-array(NA,dim=c(nman,nsim,na,nl,nattr-4))
  
  latt<-msalc[,c(1,4,5)] # locate the management option lake x management attribute
  lcat<-lxattr[latt]     # what is the level of the categorical effect?
  
  aatt<-cbind(msalc[,c(2,3,5)],lcat)   # locate the sim, angler, category by level
  attrp[msalc]<-axattr[aatt]  # what is numerical effect of this level of this category, sim and angler?
  
  sigattrp<-apply(attrp,1:4,sum) # by man, sim, angler type and lake what does this add up to?
  
  # The gravity due to travel time of anglers in a population centre to different lakes
  vals<-array(NA,dim=c(nman,nsim,npc,nl,na))
  x<-as.vector(dis)
  a<-as.factor(rep(1:na,each=nman*nsim*npc*nl))
  preddat<-data.frame(a,x)
  disval<-array(predict(lmt[[3]],newdata=preddat),dim=c(nman,nsim,npc,nl,na))
  vals[mspla]<-disval[mspla]+sigattrp[msal]
  
  valsCR<-rep(NA,na)
  for(a in 1:obj@na){
    x<-as.vector(obj@cr[1,1,i,a])
    preddatCR<-data.frame(x)
    valsCR[a]<-predict(lmt[[2]][[a]],newdata=preddatCR)  
  }
  
  x<-rep(obj@avs[1,1,i]/25.4,na)
  preddatSZ<-data.frame(x,as.factor(1:na)) # convert mm (growth model) to inches (HD)
  names(preddatSZ)<-c("x","a")
  valsSZ<-predict(lmt[[4]],newdata=preddatSZ)  
  
  U<-as.data.frame(round(exp(vals[1,1,,i,]+rep(valsSZ,each=npc)+rep(valsCR,each=npc)),3))
  U<-cbind(obj@pcxl[1,,i],U)
  row.names(U)<-obj@pcnam
  names(U)<-c("Dist (km)",obj@anam)
  
  #cat("-- Population parameters ------")
  #popval<-as.data.frame(obj@popval[1,,])
  #names(popval)<-obj@stnam
  #row.names(popval)<-names(obj@poperr)
  
  
  lch<-as.data.frame(cbind(i,obj@lakenam[i],obj@longnam[i],round(obj@lakex[i],4),round(obj@lakey[i],4),obj@lakearea[i],obj@GDD[i],obj@TDS[i]))
  names(lch)<-c("Index","WBID","Name","Lon","Lat","Area(ha)","GDD","TDS")
  
  cat("-- Lake characteristics ------")
  cat("\n")
  print(lch)
  cat("\n")
  cat("\n")
  
  
  attr<-as.data.frame(matrix(NA,nrow=obj@na+1,ncol=6))
  names(attr)<-names(obj@attr)[5:10]
  attr[1,]<-obj@lxattr[1,i,]
  amat<-array(NA,c(obj@na,6))
  ind<-as.matrix(cbind(rep(1,obj@na*6),expand.grid(1:6,1:obj@na)[,2:1],rep(obj@lxattr[1,i,],obj@na)))
  amat[ind[,2:3]]<-obj@axattr[ind]
  attr[2:5,]<-amat
  row.names(attr)<-c("level",obj@anam)
  
  cat("-- Categorical management options ------")
  cat("\n")
  print(attr)
  cat("\n")
  cat("\n")
  
  acc<-data.frame(matrix(c(obj@acc_a,obj@acc_a*obj@acc[1,i]),nrow=1))
  names(acc)<-c("Access metric","PWU")
  cat("-- Access ------")
  cat("\n")
  print(acc)
  cat("\n")
  cat("\n")
  
  lxslev<-obj@lxslev[1,i,]
  names(lxslev)<-obj@stnam
  cat("-- Stocking levels (fish per lake) ------")
  cat("\n")
  print(lxslev)
  cat("\n")
  cat("\n")
  
  cat("-- Observed vs Predicted ------")
  cat("\n")
  mat<-round(mat,3)
  print(mat)
  cat("\n")
  cat("\n")
  
  cat("-- Sum utility of lake ------")
  cat("\n")
  print(U)
  cat("\n")
  cat("\n") 
  
  U<-obj@U[1,1,,,]
  ind<-as.matrix(expand.grid(1:obj@npc,1:obj@nl,1:obj@na))
  indpa<-ind[,c(1,3)]
  sigU<-apply(U,c(1,3),sum)
  U[ind]<-U[ind]/sigU[indpa]
  
  U2<-as.data.frame(cbind(obj@pcxl[1,,i],U[,i,]*100))
  row.names(U2)<-obj@pcnam
  names(U2)<-c("Dist (km)",obj@anam)
  
  cat("-- Angler choice % of anglers originating from a class and population centre ------")
  cat("\n")
  print(round(U2,2))
  cat("\n")
  cat("\n") 
  
  U3<-as.data.frame(round(U[,i,]*obj@pcxa[1,,]*obj@pcsize[1,]*obj@FTroutAng*obj@apr[1,,],0))
  U3<-cbind(obj@pcxl[1,,i],U3)
  row.names(U2)<-obj@pcnam
  row.names(U3)<-obj@pcnam
  names(U3)<-c("Dist (km)",obj@anam)
  
  cat("-- Angler composition on lake ------")
  cat("\n")
  print(U3)
  cat("\n")
  cat("\n") 
  
  dis<-as.data.frame(disval[1,1,,i,])
  dis<-cbind(obj@pcxl[1,,i],dis)
  row.names(dis)<-obj@pcnam
  names(dis)<-c("Dist (km)",obj@anam)
  
  cat("-- Part worth utility of distance ------")
  cat("\n")
  print(dis)
  cat("\n")
  cat("\n")
  
  names(valsSZ)<-obj@anam
  cat("-- Part worth utility of size of catch ------")
  cat("\n")
  print(valsSZ)
  cat("\n")
  cat("\n")
  
  names(valsCR)<-obj@anam
  cat("-- Part worth utility of catch rate ------")
  cat("\n")
  print(valsCR)
  cat("\n")
  cat("\n")
  
  cat("-- Link to google maps ------")
  cat("\n")
  cat(paste("http://www.google.com/maps/place/",obj@lakey[i],",",obj@lakex[i],"/data=!3m1!1e3",sep=""))
  cat("\n")
  
  if(map){
    windows()
    mapeff(obj,lpick=i)
    lch2 <- rapply(lch, as.character, classes="factor", how="replace")
    legend('topright',legend=paste(names(lch2),lch2,sep=": "),bty='n',text.col='blue')
    
  }
  # where is the utility coming from?
  # add predicted average size.. predicted cpue
  
  # title of lake
  # biological model type
  # things to plot Mage
  # keep rate
  # length age
  # density
  # cpue (angler type)
  # catch at size (angler type)
  # predicted F at current E (abline v on each plot)
  # GDD
  # hectares
  # Stocking rate
  # Name of lake
  # Table of observed vs pred eff, CR, AvS + biological parameteers
  # Stocking levels etc
  # Also properly name plots
  
}



setClass("Landscape",representation(Name="character",Note="character",npc="numeric",
                                    nl="numeric",na="numeric",nattr="numeric",ncat="numeric",nsim="numeric",
                                    nmanage="numeric",nage="numeric",nst="numeric",nattrvals="numeric",
                                    pcnam="character",
                                    pcsize="array",pcx="numeric",pcy="numeric",lakenam="character",longnam="character",
                                    lakex="numeric",lakey="numeric",lakearea="numeric",lxslev="array",
                                    GDD="array",TDS="array",stockable="numeric",anam="character",apr="array",aq="array",
                                    attr="data.frame",pcxl="array",pcxa="array",lxattr="array",axattr="array",
                                    eff="array",avs="array",cr="array",exeff="array",exavs="array",excr="array",sigeff="array",sigexeff="array",cont="list",
                                    topcols="character",costs="array",Scosts="array",totcost="array",effval="array",licval="array",stnam="character",
                                    aclass="numeric",stwt="numeric",stlen="numeric",popval="array",poperr="data.frame",errs="data.frame",Mage="array",CB="array",
                                    conv="array",Econv="array",lm='list',U="array",FTroutAng="numeric",acc="array",acc_a="numeric",
                                    misc="data.frame",sel="array",GModel="character",couch="array",fac="array",fac_a="numeric",
                                    DR="array",DD="numeric",BagLim="array",maxdays="array"))

setMethod("initialize", "Landscape", function(.Object,datafile){
  
  #load("template")    # default setup
  #.Object<-template   # default setup
  #.Object})  
  # Load setup file. One line per attribute
  #dat<-read.csv(datafile,header=F)
  dat<-read.csv(datafile,header=F)
  nam<-dat[,1]
  dat<-as.data.frame(t(dat[,2:ncol(dat)]))
  names(dat)<-nam
  
  # Record the dimensions of the problem
  npc<-fac2num(dat$npc[1])
  nl<-fac2num(dat$nl[1])
  nattr<-fac2num(dat$nattr[1])
  na<-fac2num(dat$na[1])
  nsim<-fac2num(dat$nsim[1])
  nmanage<-fac2num(dat$nmanage[1])
  nage<-fac2num(dat$nage[1])
  nst<-fac2num(dat$nst[1])
  nattrvals<-sum(fac2num(dat$attrtype[1:nattr]))
  
  # Assign some attributes to the object
  .Object@Name<-as.character(dat$Name[1])
  .Object@Note<-"alpha testing - population dynamics modelling and IDF calculations"
  .Object@npc<-npc
  .Object@nl<-nl
  .Object@na<-na
  .Object@nattr<-nattr
  .Object@nsim<-nsim
  .Object@nmanage<-nmanage
  .Object@nage<-nage
  .Object@nst<-nst
  .Object@nattrvals<-nattrvals
  .Object@pcnam<-as.character(dat$pcnam[1:npc])
  .Object@pcsize<-array(fac2num(dat$pcsize[1:npc]),dim=c(1,npc))
  .Object@pcx<-fac2num(dat$pcx[1:npc])
  .Object@pcy<-fac2num(dat$pcy[1:npc])
  .Object@lakenam<-as.character(dat$lakenames[1:nl])
  .Object@longnam<-rep("NA",nl)
  .Object@lakex<-fac2num(dat$lakex[1:nl])
  .Object@lakey<-fac2num(dat$lakey[1:nl])
  .Object@lakearea<-fac2num(dat$lakearea[1:nl])
  
  
  .Object@GDD<-array(fac2num(dat$GDD[1:nl]),dim=c(1,nl))
  .Object@TDS<-array(fac2num(dat$TDS[1:nl]),dim=c(1,nl))
  .Object@stockable<-fac2num(dat$stockable[1:nl])
  .Object@anam<-as.character(dat$anam[1:na])
  .Object@apr<-array(fac2num(dat$apr[1:na]),dim=c(1,na))
  .Object@aq<-array(fac2num(dat$aq[1:na]),dim=c(1,na))
  
  # Create a table of the different lake attributes, their variable types (continous or categorical) and their error distributions
  attrnam<-as.character(dat$attrnam[1:nattr])
  attr<-fac2num(dat$attrtype[1:nattr])
  attrerrtype<-as.character(dat$attrerrtype[1:nattr])
  attrCV<-as.character(dat$attrCV[1:nattr])
  names(attr)<-attrnam
  attr<-as.data.frame(rbind(attr,attrerrtype,attrCV))
  .Object@attr<-attr 
  .Object@ncat<-max(fac2num(attr[1,]))
  
  # pcxa is population centre x angler type (fraction of angler types in each population centre, including non fishers which explains the na+1)
  .Object@pcxa<-array(fac2num(dat$pcxa[1:(npc*(1+na))]),dim=c(npc,na+1))
  if(is.na(dat$lxattr[1])) .Object@lxattr<-esimlakemanage(fac2num(dat$attrtype[4:nattr]),nl,nmanage)
  if(!is.na(dat$lxattr[1])) .Object@lxattr<-array(fac2num(dat$lxattr[1:(nattr*nl*nmanage)]),dim=c(nl,nattr,nmanage))
  
  # axattr is angler type x attribute matrix (how anglers are affected by lake attributes)
  .Object@axattr<-getattr(fac2num(dat$axattr[1:(nattrvals*na)]),fac2num(dat$attrtype[1:nattr]))
  
  # Current observed effort
  .Object@eff<-array(NA,dim=c(2,2))
  .Object@avs<-array(NA,dim=c(2,2))
  .Object@cr<-array(NA,dim=c(2,2))
  .Object@sigeff<-array(NA,dim=c(2,2))
  # The effort 'experiments'
  .Object@exeff<-array(NA,dim=c(2,2))
  .Object@exavs<-array(NA,dim=c(2,2))
  .Object@excr<-array(NA,dim=c(2,2))
  .Object@acc<-array(1,dim=c(1,.Object@nl))
  .Object@acc_a<-0
  .Object@DR<-array(NA,dim=c(2,2))
  .Object@DD<-0.05
  .Object@BagLim<-array(NA,dim=c(2,2))
  .Object@maxdays<-array(NA,dim=c(2,2))
  .Object@sel<-array(c(0,0.5,rep(1,obj@nage-2)),dim=c(1,obj@nage))
  
  # Assign costs to each lake attribute (e.g. stocking rate etc)
  if(is.na(dat$costs[1])).Object@costs<-array(runif((nattr-3)*nl*dim(.Object@axattr)[4]),dim=c(1,nl,nattr-3,dim(.Object@axattr)[4])) # fake costs array
  if(!is.na(dat$costs[1])) .Object@costs<-array(fac2num(dat$costs[1:((nattrvals-3)*nl)]),dim=c(1,nl,nattrvals-3))
  if(is.na(dat$Scosts[1])).Object@Scosts<-array(runif(nst*nl),dim=c(1,nl,nst)) # fake costs array
  if(!is.na(dat$Scosts[1])) .Object@Scosts<-array(fac2num(dat$Scosts[1:(nst*nl)]),dim=c(1,nl,nst))
  
  # Assign a default plotting object (contours / contour colours for the landscape
  Drive<-substr(getwd(),1,1)
  if(file.exists(paste(Drive,":/Trout lakes/Data/cont",sep=""))){
    load(paste(Drive,":/Trout lakes/Data/cont",sep=""))
    load(paste(Drive,":/Trout lakes/Data/topcols",sep=""))
    .Object@cont<-cont
    .Object@topcols<-topcols
  }else{
    .Object@cont<-NA
    .Object@topcols<-NA
  }
  
  # Assign values to calculated outputs (License sales / total effort)
  .Object@effval<-array(fac2num(dat$effval[1:na]),dim=c(1,na))
  .Object@licval<-array(fac2num(dat$licval[1]),dim=c(1,1))
  
  .Object@stnam<-as.character(dat$stnam[1:nst])
  .Object@aclass<-fac2num(dat$aclass[1:nst])
  .Object@stwt<-fac2num(dat$stwt[1:nst])
  .Object@stlen<-fac2num(dat$stlen[1:nst])
  
  if(is.na(dat$lxslev[1])).Object@lxslev<-getlxslev(fac2num(dat$lakearea[1:nl]),nst,nmanage,.Object@stwt)
  if(!is.na(dat$lxslev[1])) array(fac2num(dat$stlev[1:(nl*nst)]),dim=c(1,nl,nst))
  
  
  # The default values for population parameters
  npopvals<-sum(!is.na(fac2num(dat$popval)))/nst
  .Object@popval<-array(fac2num(dat$popval[1:(npopvals*nst)]),dim=c(1,npopvals,nst))
  #names(.Object@popval)<-as.character(dat$popnam[1:npopvals])
  
  # Create a table of the population variables (growth rate etc) and their error distributions
  poperr<-as.character(dat$poperrtype[1:npopvals])  # Mage is included as an error
  names(poperr)<-as.character(dat$popnam[1:npopvals])
  poperr<-as.data.frame(rbind(poperr,fac2num(dat$poperrCV[1:npopvals])))
  row.names(poperr)<-c("errtype","CV")
  .Object@poperr<-poperr
  
  # Create a table of other error terms such as population size, GDD, costs, effval, licval
  notherrs<-sum(!is.na(fac2num(dat$errCV)))
  errs<-fac2num(dat$errCV[1:notherrs])
  names(errs)<-as.character(dat$errnam[1:notherrs])
  errs<-as.data.frame(rbind(errs,as.character(dat$errtype[1:notherrs]))[2:1,])
  row.names(errs)<-c("errtype","CV")
  .Object@errs<-errs
  # pcxl is population centre x lake travel time (this is down here because it requires .Object@errs)
  if(is.na(dat$pcxl[1]))   .Object@pcxl<-getEdist(fac2num(dat$pcx[1:npc]),fac2num(dat$pcy[1:npc]),fac2num(dat$lakex[1:nl]),fac2num(dat$lakey[1:nl]),nmanage,fac2num(.Object@errs$pcxl[2]),stand=T)
  if(!is.na(dat$pcxl[1]))  .Object@pcxl<-array(fac2num(dat$pcxl[1:(npc*nl*nmanage)]),dim=c(nmanage,npc,nl))
  
  #.Object<-calceff(.Object)
  # These are attributes that are optimized to fit model predictions of effort to observations
  #.Object@nfpc<-array(0,dim=c(1,1))
  #.Object@nfa<-array(0,dim=c(1,1))
  #.Object@nfint<-array(0,dim=c(1,1))
  
  .Object@Mage<-array(fac2num(dat$Mage[1:(nage*nst)]),dim=c(1,nage,nst))
  
  ind<-as.matrix(expand.grid(1:nmanage,1:nsim,1:nl,1:nst))
  indm<-ind[,c(1,3,4)]
  inds<-ind[,c(2,3,4)]
  Scostarray<-array(NA,dim=c(nmanage,nsim,nl,nst))
  Scostarray[ind]<-.Object@Scosts[inds]*.Object@lxslev[indm]
  
  ind<-as.matrix(expand.grid(1:nmanage,1:nsim,1:nl,1:(nattr-3)))
  lev<-.Object@lxattr[ind[,c(1,3,4)]]
  inds<-cbind(ind[,c(2,3,4)],lev)
  costarray<-array(NA,dim=c(nmanage,nsim,nl,nattr-3))
  costarray[ind]<-.Object@costs[inds]
  
  .Object@totcost<-apply(Scostarray,1:2,sum,na.rm=T)+apply(costarray,1:2,sum,na.rm=T)
  .Object@CB<-array(NA,c(2,2))
  if(nsim>1).Object<-Stoch(.Object,nsim)
  
  .Object@conv<-array(NA,c(2,2))
  .Object@Econv<-array(NA,c(2,2))
  .Object@lm<-new('list')
  .Object@U<-array(NA,c(2,2))
  .Object@FTroutAng<-0.8
  .Object@couch<-array(NA,c(2,2))
  .Object
  
})

SimMO<-function(obj,nmanage){ # simulates management error
  
  nl<-obj@nl
  npc<-obj@npc
  nst<-obj@nst
  obj@nmanage<-nmanage
  obj@lxattr<-esimlakemanage(fac2num(obj@attr[1,4:obj@nattr]),nl,nmanage)
  obj@lxslev<-getlxslev(fac2num(obj@lakearea[1:nl]),nst,nmanage,obj@stwt)
  obj@pcxl<-array(getbias(nmanage*nl*npc,fac2num(obj@errs$pcxl[2]))*
                    rep(obj@pcxl,each=nmanage),dim=c(nmanage,npc,nl))
  obj@BagLim<-array(rep(obj@BagLim,each=nmanage),dim=c(nmanage,nl))
  obj
  
}

replic8<-function(obj,nmanage){ # replicates the object over multiple management options
  
  nl<-obj@nl
  npc<-obj@npc
  nst<-obj@nst
  obj@nmanage<-nmanage
  obj@lxattr<-array(rep(obj@lxattr,each=nmanage),c(nmanage,dim(obj@lxattr)[2:3]))
  obj@lxslev<-array(rep(obj@lxslev,each=nmanage),c(nmanage,dim(obj@lxslev)[2:3]))
  obj@pcxl<-array(rep(obj@pcxl,each=nmanage),c(nmanage,dim(obj@pcxl)[2:3]))
  obj@BagLim<-array(rep(obj@BagLim,each=nmanage),dim=c(nmanage,nl))
  obj@couch<-array(rep(obj@couch,each=nmanage),dim=c(nmanage,dim(obj@couch)[2:4]))
  if(length(dim(obj@eff))>2)obj@eff<-array(rep(obj@eff,each=nmanage),dim=c(nmanage,dim(obj@eff)[2:5]))
  obj
  
}


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
    
    #if(itn<5)ref<-itn-1
    #if(itn>4)ref<-itn-4
    #higher<-conv[,itn]>=conv[,ref]
    #dif[higher]<-(conv[higher,itn]-conv[higher,ref])/conv[higher,itn] 
    #dif[!higher]<- (conv[!higher,ref]-conv[!higher,itn])/conv[!higher,ref] 
    
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
    if(!totE)plot(std(obj@conv[1,indc]),axes=F,ylim=ylimy,col=coly[1],type="l",lty=ltys[1],lwd=2,main="",xlab="Iteration",ylab="Effort lake x angler x pop. centre (days fished))")
    if(totE)plot(std(obj@conv[1,indc]),axes=F,ylim=ylimy,col=coly[1],type="l",lty=ltys[1],lwd=2,main="",xlab="Iteration",ylab="Effort by lake (days fished)")
  }else{
    if(!totE)plot(std(obj@conv[1,indc]),ylim=ylimy,col=coly[1],type="l",lty=ltys[1],lwd=2,main="",xlab="Iteration",ylab="Effort (lake x angler x pop. centre)")
    if(totE)plot(std(obj@conv[1,indc]),ylim=ylimy,col=coly[1],type="l",lty=ltys[1],lwd=2,main="",xlab="Iteration",ylab="Effort (by lake)")
  }
  abline(h=c(-1,1),lty=2,col='grey')
  for(i in 2:n)lines(std(obj@conv[i,indc]),col=coly[i],lty=ltys[i],lwd=2)
  
 
}


StockB<-function(obj,nits=200,uprat=0.1,varind=0.8,inc=1.5){

  nl<-obj@nl
  npc<-obj@npc
  nst<-obj@nst
  nattr<-obj@nattr
  obj@nmanage<-nl
  nman<-nl
  obj@lxattr<-array(rep(obj@lxattr,each=nman),dim=c(nman,dim(obj@lxattr)[2:3]))
  obj@eff<-array(rep(obj@eff,each=nman),dim=c(nman,dim(obj@eff)[2:5]))
  obj@couch<-array(rep(obj@couch,each=nman),dim=c(nman,dim(obj@couch)[2:4]))
  lxslev<-obj@lxslev
  obj@pcxl<-array(rep(obj@pcxl,each=nman),dim=c(nman,npc,nl))
  typeind<-apply(lxslev[1,,]*obj@Scosts[1,,],1,which.max)
  ind<-as.matrix(cbind(expand.grid(1:nman,1:nl),rep(typeind,each=nman))) # m, lake, most costly stocking type by lake
  ind2<-ind[,c(1,1,3)]
  obj@lxslev<-array(rep(lxslev,each=nl),dim=c(nman,nl,nst))
  obj@lxslev[ind2]<-obj@lxslev[ind2]*inc
  obj<-calceff(obj,uprat=uprat,nits=nits,varind=varind,couch=T)
  obj
  
}

StockAlt<-function(obj,nits=200,uprat=0.1,varind=0.8,inc=1.5,types=NA){
 
  nman<-obj@nmanage
  nsim<-obj@nsim
  nl<-obj@nl
  nst<-obj@nst
  nattr<-obj@nattr
  
  ind<-as.matrix(expand.grid(1:nman,1:nsim,1:nl,1:nst))
  indm<-ind[,c(1,3,4)]
  inds<-ind[,c(2,3,4)]
  Scostarray<-array(NA,dim=c(nman,nsim,nl,nst))
  Scostarray[ind]<-obj@Scosts[inds]*obj@lxslev[indm]
  totcost<-apply(Scostarray,1:3,sum,na.rm=T)
  
  nstock<-array(NA,dim=c(nman,nsim,nl,nst))
  nstock[ind]<-totcost[ind[,1:3]]/obj@Scosts[inds]
  
  
  nty<-length(types)
  nl<-obj@nl
  npc<-obj@npc
  nst<-obj@nst
  nattr<-obj@nattr
  obj@nmanage<-nl*nty
  nman<-nl*nty
  obj@lxattr<-array(rep(obj@lxattr,each=nman),dim=c(nman,dim(obj@lxattr)[2:3]))
  obj@eff<-array(rep(obj@eff,each=nman),dim=c(nman,dim(obj@eff)[2:5]))
  obj@couch<-array(rep(obj@couch,each=nman),dim=c(nman,dim(obj@couch)[2:4]))
  lxslev<-obj@lxslev
  obj@pcxl<-array(rep(obj@pcxl,each=nman),dim=c(nman,npc,nl))
  obj@lxslev<-array(rep(lxslev,each=nman),dim=c(nman,nl,nst))
  typeind<-match(types,obj@stnam)
  m<-0
  for(i in 1:nl){
    for(j in 1:nty){
      m<-m+1
      obj@lxslev[m,i,]<-0
      obj@lxslev[m,i,typeind[j]]<-nstock[1,1,i,typeind[j]]
    }
  }

  obj<-calceff(obj,uprat=uprat,nits=nits,varind=varind,couch=T)
  obj
  
}

calcCB<-function(obj){
  
  nman<-obj@nmanage
  nsim<-obj@nsim
  nl<-obj@nl
  nst<-obj@nst
  nattr<-obj@nattr
  
  ind<-as.matrix(expand.grid(1:nman,1:nsim,1:nl,1:nst))
  indm<-ind[,c(1,3,4)]
  inds<-ind[,c(2,3,4)]
  Scostarray<-array(NA,dim=c(nman,nsim,nl,nst))
  Scostarray[ind]<-obj@Scosts[inds]*obj@lxslev[indm]
  
  #ind<-as.matrix(expand.grid(1:nman,1:nsim,1:nl,1:(nattr-3)))
  #lev<-obj@lxattr[ind[,c(1,3,4)]]
  #inds<-cbind(ind[,c(2,3,4)],lev)
  #costarray<-array(NA,dim=c(nman,nsim,nl,nattr-3))
  #costarray[ind]<-obj@costs[inds]
  
  obj@totcost<-apply(Scostarray,1:2,sum,na.rm=T)#+apply(costarray,1:2,sum,na.rm=T)
  #obj@CB<-apply(obj@sigeff,1:2,sum)*obj@effval[1]/obj@totcost
  obj@CB<-apply(obj@sigeff,1:2,sum)/obj@totcost
  obj
  
}

tomapply<-function(x,nman,nsim){
  if(nman==1 & nsim==1)  z<- array(apply(x,2:3,sum),dim=c(1,1,dim(x)[2:3]))
  if(nman==1&nsim>1)   z<- array(apply(x,2:4,sum),dim=c(1,dim(x)[3:4]))
  if(nman>1&nsim==1)  z<-  array(apply(x,c(1,3,4),sum),dim=c(dim(x)[1],1,dim(x)[3:4]))
  z
}


#std<-function(x)log(x+0.0000001)/(log(x+0.0000001)[length(x)])
std<-function(x)log(x+0.0000001)-(log(x+0.0000001)[length(x)])

simexattr<-function(.Object){
  .Object@exattr<-esimlakemanage(.Object@attrtype[2:(.Object@nattr-1)],.Object@nl,.Object@exn)
  .Object
}

logit<-function(x)exp(x)/(1+exp(x))
#choice<-function(x,nf)exp(x)/(nf+exp(x))

getattr<-function(vals,dims){
  nattr<-length(dims)
  nattrvals<-sum(dims)
  maxlev<-max(dims)
  na<-length(vals)/nattrvals
  out<-array(NA,dim=c(1,na,nattr,maxlev))
  for(i in 1:nattr){
    for(j in 1:dims[i]){
      for(k in 1:na){
        if(i==1&j==1&k==1){
          temp<-rep(1,4)
        }else{
          temp<-rbind(temp,c(1,k,i,j))
        }
      }
    }
  }
  out[temp]<-vals
  out
}

#getlxslev<-function(areas,nst,nmanage) array(log(rep(areas,each=nmanage))*(400/nst)*floor(runif(nmanage*nst*length(areas))+0.35)*runif(nmanage*nst*length(areas)),dim=c(nmanage,length(areas),nst))

getlxslev<-function(areas,nst,nmanage,stwt){
  
  nlakes<-length(areas)
  lxslev<-array(0,dim=c(nmanage,nlakes,nst))
  ntot<-nmanage*nlakes
  stype<-ceiling(runif(ntot)*nst)
  indss<-cbind(as.matrix(expand.grid(1:nmanage,1:nlakes)),stype)
  lxslev[indss]<-(3/stwt[indss[,3]])^0.5*200*areas[indss[,2]]*runif(ntot,0.5,1.5)
  lxslev
  
} 
#apply(getlxslev(fac2num(dat$lakearea[1:nl]),nst),2,FUN=function(x)sum(x==0))==18
calcexeff<-function(.Object){
  
  pcsize<-obj@pcsize
  pcxl<-obj@pcxl
  pcxa<-obj@pcxa
  axattr<-obj@axattr
  lxattr<-obj@lxattr
  npc<-dim(pcxl)[1]
  nl<-dim(pcxl)[2]
  na<-dim(pcxa)[2]-1
  attrtype<-obj@attrtype
  obj@exeff<-array(NA,dim=c(npc,nl,na,obj@exn))
  
  for(i in 1:obj@exn){
    lxattr<-obj@exattr[,,i]
    natt<-ncol(lxattr)
    attrty<-attrtype[2:(natt+1)]
    pla<-as.matrix(expand.grid(1:npc,1:nl,1:na))
    pl<-pla[,1:2]
    pa<-pla[,c(1,3)]
    aa<-cbind(pla[,3],rep(1,npc*nl*na),rep(1,npc*nl*na))
    
    dis<-array(NA,dim=c(npc,nl,na))
    dis[pla]<-pcxl[pl]*axattr[aa]
    
    contcol<-(1:length(attrty))[attrty==1]
    catcol<-(1:length(attrty))[attrty>1]
    
    attrp<-array(NA,dim=c(na,nl,natt))
    
    alatt<-as.matrix(expand.grid(1:na,1:nl,contcol))
    aatt<-cbind(alatt[,1],alatt[,3]+1,rep(1,na*nl*length(contcol)))
    latt<-alatt[,c(2,3)]
    attrp[alatt]<-axattr[aatt]*lxattr[latt]
    
    alatt<-as.matrix(expand.grid(1:na,1:nl,catcol))
    latt<-alatt[,c(2,3)]
    lcat<-lxattr[latt]
    aatt<-cbind(alatt[,1],alatt[,3]+1,lcat)
    attrp[alatt]<-axattr[aatt]
    
    sigattrp<-apply(attrp,1:2,sum)
    
    vals<-array(NA,dim=c(npc,nl,na))
    
    aa<-cbind(pla[,3],rep(natt+2,npc*nl*na),rep(1,npc*nl*na))
    al<-pla[,c(3,2)]
    vals[pla]<-exp(dis[pla]+axattr[aa]+sigattrp[al])
    gravs<-array(NA,dim=c(npc,nl,na))
    sigvals<-apply(vals,c(1,3),sum)
    pa<-pla[,c(1,3)]
    gravs[pla]<-vals[pla]/(sigvals[pa]+exp(obj@nfpc[p]+obj@nfa[a]+obj@nfint))
    
    p<-pla[,1]
    a<-pla[,3]
    plai<-cbind(pla,rep(i,npc*nl*na))
    obj@exeff[plai]<-365*pcsize[p]*pcxa[pa]*gravs[pla]
    
    #obj@exeff[plai]<-365*pcsize[p]*pcxa[pa]*(1-nofish[a])*gravs[pla]
  }
  obj@sigexeff<-apply(obj@exeff,c(2,4),sum)
  obj
  
}

# Write data to ADMB for tuning
writeADMB<-function(obj,datfile){
  
  write("# Dimensions of model ----------------------------",datfile,1)
  write("# npc number of population centres",datfile,1)
  write(obj@npc,datfile,1,append=T)
  write("# nl number of lakes",datfile,1,append=T)
  write(obj@nl,datfile,1,append=T)
  write("# na number of angler types",datfile,1,append=T)
  write(obj@na,datfile,1,append=T)
  write("# nattr number of lake attributes",datfile,1,append=T)
  write(obj@nattr,datfile,1,append=T)
  write("# pcsize population centre size (number of inhabitants)",datfile,1,append=T)
  write(obj@pcsize,datfile,obj@npc,append=T)
  #write("# pcx longitude of population centre",datfile,1,append=T)
  #write(obj@pcx,datfile,obj@npc,append=T)
  #write("# pcy latitude of population centre",datfile,1,append=T)
  #write(obj@pcy,datfile,obj@npc,append=T)
  #write("# lakex longitude of lake",datfile,1,append=T)
  #write(obj@lakex,datfile,obj@nl,append=T)
  #write("# lakey latitude of lake",datfile,1,append=T)
  #write(obj@lakey,datfile,obj@nl,append=T)
  write("# lakearea surface area of lake (hectares)",datfile,1,append=T)
  write(obj@lakearea,datfile,obj@nl,append=T)
  write("# attrtype type of lake attribute (x=1 continuous linear, x=2+ categorical with x levels)",datfile,1,append=T)
  write(obj@attrtype,datfile,obj@nattr,append=T)
  write("# apr participation rate fraction of time spent not fishing by angler type (e.g. 0.9 is 9/10 days are spent doing something other than fishing)",datfile,1,append=T)
  write(obj@apr,datfile,obj@na,append=T)
  write("# nofish intercept",datfile,1,append=T)
  write(obj@nfint,datfile,obj@na,append=T)
  write("# nofish pop centre effects",datfile,1,append=T)
  write(obj@nfpc[2:npc],datfile,obj@npc,append=T)
  write("# nofish angler effects",datfile,1,append=T)
  write(obj@nfa[2:na],datfile,obj@na,append=T)
  write("# pcxl population centre x lake distance matrix or transformation of matrix such that factor = pcxl*axattr[,1]",datfile,1,append=T)
  write(obj@pcxl,datfile,prod(dim(obj@pcxl)),append=T)
  write("# pcxa population centre x angler type matrix, fraction of population of each angler type - has an extra column for non fishers ",datfile,1,append=T)
  write(obj@pcxa,datfile,prod(dim(obj@pcxa)),append=T)
  write("# lxattr lake x lake attribute matrix, either continous or categorical effects",datfile,1,append=T)
  write(obj@lxattr,datfile,prod(dim(obj@lxattr)),append=T)
  write("# ncats maximum number of categorical effects",datfile,1,append=T)
  write(dim(obj@axattr)[3],datfile,1,append=T)
  axattr<-obj@axattr
  axattr[is.na(axattr)]<-0
  write("# axattr angler x attribute matrix, either continous or categorical effects",datfile,1,append=T)
  write(axattr,datfile,prod(dim(axattr)),append=T)
  write("# eff current effort",datfile,1,append=T)
  write(obj@sigeff,datfile,obj@nl,append=T)
  write("# exn number of experimental years",datfile,1,append=T)
  write(obj@exn,datfile,1,append=T)
  write("# exattr experimental lake attribute matrix, either continous or categorical effects",datfile,1,append=T)
  write(obj@exattr,datfile,prod(dim(obj@exattr)),append=T)
  write("# exeff experimental effort by lake matrix",datfile,1,append=T)
  write(obj@sigexeff,datfile,prod(dim(obj@exeff)[c(2,4)]),append=T)
  write("# costs lake x attribute matrix, either continous or categorical effects",datfile,1,append=T)
  write(obj@costs,datfile,prod(dim(obj@costs)),append=T)
  write("# effval the value of a unit of effort by angler type",datfile,1,append=T)
  write(obj@effval,datfile,na,append=T)
  write("# licval the value of a license sale",datfile,1,append=T)
  write(obj@licval,datfile,1,append=T)
  write("# data check",datfile,1,append=T)
  write(999999,datfile,1,append=T)
  
}

Lsub<-function(obj,ind){
  obj@nl<-length(ind)
  obj@lakenam<-obj@lakenam[ind]
  obj@longnam<-obj@longnam[ind]
  obj@lakex<-obj@lakex[ind]
  obj@lakey<-obj@lakey[ind]
  obj@lakearea<-obj@lakearea[ind]
  dim0=dim(obj@lxslev)
  dim0[2]<-length(ind)
  obj@lxslev<-array(obj@lxslev[,ind,],dim0)
  obj@GDD<-t(matrix(obj@GDD[,ind]))
  obj@TDS<-t(matrix(obj@TDS[,ind]))
  obj@BagLim<-t(matrix(obj@BagLim[,ind]))
  obj@stockable<-obj@stockable[ind]
  dim0=dim(obj@pcxl)
  dim0[3]<-length(ind)
  obj@pcxl<-array(obj@pcxl[,,ind],dim0)
  dim0=dim(obj@lxattr)
  dim0[2]<-length(ind)
  obj@lxattr<-array(obj@lxattr[,ind,],dim0)
  obj@Scosts<-array(obj@Scosts[,ind,],dim0)
  dim0=dim(obj@exeff)
  dim0[2]<-length(ind)
  obj@exeff<-array(obj@exeff[,ind],dim0)
  dim0<-dim(obj@eff)
  dim0[4]<-length(ind)
  if(length(dim(obj@eff))>2)obj@eff<-array(obj@eff[,,,ind,],dim0)
  obj  
}


pc_simp<-function(obj,h=1000){
  
  npc<-obj@npc
  nl<-obj@nl
  mat<-array(NA,c(npc,npc))
  for(i in 1:npc){
    for(j in 1:npc){
      dif<-((obj@pcxl[1,i,]-obj@pcxl[1,j,])^2)^0.5
      mat[i,j]<-sum(dif)/nl
    }
  }
  rownames(mat)<-obj@pcnam
  mat<-as.dist(mat)
  #mat<-cbind(obj@pcx,obj@pcy)
  #rownames(mat)<-obj@pcnam
  out<-hclust(dist(mat),method="ward.D")
  plot(out)
  rect.hclust(out,h=h)
  groups<-cutree(out, h=h)
  
  npc<-max(groups)
  
  pcsize<-matrix(aggregate(obj@pcsize[1,],by=list(as.vector(groups)),sum)$x,nrow=1)
  pcx<-aggregate(obj@pcx*obj@pcsize[1,],by=list(as.vector(groups)),sum)$x/aggregate(obj@pcsize[1,],by=list(as.vector(groups)),sum)$x
  pcy<-aggregate(obj@pcy*obj@pcsize[1,],by=list(as.vector(groups)),sum)$x/aggregate(obj@pcsize[1,],by=list(as.vector(groups)),sum)$x
  
  pcnam<-rep(NA,npc)
  szg<-aggregate(rep(1,length(groups)),by=list(as.vector(groups)),sum)$x
  for(i in 1:npc){
    if(szg[i]==1)pcnam[i]<-obj@pcnam[match(i, groups)]
    if(szg[i]>1)pcnam[i]<-paste(obj@pcnam[match(i, groups)],"_etal",sep="")
    #pcnam[i]<-obj@pcnam[match(i, groups)] 
  }
  
  apr<-array(NA,c(1,npc,4))
  for(i in 1:4){
    apr[1,,i]<-aggregate(obj@apr[1,,i]*obj@pcsize[1,],by=list(as.vector(groups)),sum)$x/
      aggregate(obj@pcsize[1,],by=list(as.vector(groups)),sum)$x
  }
  
  pcxl<-array(NA,c(1,npc,nl))
  for(i in 1:nl){
    pcxl[1,,i]<-aggregate(obj@pcxl[1,,i]*obj@pcsize[1,],by=list(as.vector(groups)),sum)$x/
      aggregate(obj@pcsize[1,],by=list(as.vector(groups)),sum)$x
  }
  
  
  totno<-obj@pcxa[1,,]*obj@pcsize[1,]
  pcxa<-array(NA,c(1,npc,4))
  for(i in 1:max(groups)){
    ind<-(1:length(groups))[i==groups]
    if(length(ind)>1){
      temp<-apply(totno[ind,],2,sum)
    }else{
      temp<-totno[ind,]
    } 
    pcxa[1,i,]<-temp/sum(temp)
  }
  obj@FTroutAng<-aggregate(obj@FTroutAng*obj@pcsize[1,],by=list(as.vector(groups)),sum)$x/
    aggregate(obj@pcsize[1,],by=list(as.vector(groups)),sum)$x
  obj@npc<-npc
  obj@pcsize<-pcsize
  obj@pcx<-pcx
  obj@pcy<-pcy
  obj@pcnam<-pcnam
  obj@apr<-apr
  obj@pcxl<-pcxl
  obj@pcxa<-pcxa
  obj
  
}

pc_collapse<-function(obj){ # shrink to just 1 population centre

  nl<-obj@nl
  npc<-1
  pcsize<-matrix(sum(obj@pcsize,nrow=1))
  pcx<-mean(obj@pcx)
  pcy<-mean(obj@pcy)
  
  pcnam<-"All PCs"
  
  apr<-array(NA,c(1,npc,4))
  for(i in 1:4){
    apr[1,,i]<-sum(obj@apr[1,,i]*obj@pcsize[1,])/ sum(obj@pcsize[1,])
  }
  
  pcxl<-array(NA,c(1,npc,nl))
  for(i in 1:nl){
    pcxl[1,,i]<-sum(obj@pcxl[1,,i]*obj@pcsize[1,])/sum(obj@pcsize[1,])
  }
  
  totno<-apply(obj@pcxa[1,,]*obj@pcsize[1,],2,sum)
  pcxa<-array(NA,c(1,npc,4))
  pcxa[1,1,]<-totno/sum(totno)
  
  obj@FTroutAng<-sum(obj@FTroutAng*obj@pcsize[1,])/sum(obj@pcsize[1,])
  obj@npc<-npc
  obj@pcsize<-pcsize
  obj@pcx<-pcx
  obj@pcy<-pcy
  obj@pcnam<-pcnam
  obj@apr<-apr
  obj@pcxl<-pcxl
  obj@pcxa<-pcxa
  obj
  
}




optEfunB<-function(par,obj,nits,opt=T,ploty=F){
  print(exp(par))
  obj2<-obj
  obj2@couch<-obj2@couch*exp(par)
  obj2<-calceff(obj2,nits=nits,couch=T) 
  obj2@exeff[obj2@exeff<0.01]<-0.01
  dif<-log(obj2@exeff[1,]+tiny)-log(apply(obj2@eff[1,1,,,],2,sum)+tiny)
  print(dif[!is.na(dif)])
  print(mean(dif,na.rm=T))
  funcy<--sum(dnorm(dif,0,0.3,log=T),na.rm=T) # negative log likelihood of log effort difference
  #funcy<-funcy-sum(dlnorm(exp(par),0,2,log=T)) # negative log likelihood of multiplicative deviation from couch effect
  if(ploty){
    plot(log(apply(obj2@eff[1,1,,,],2,sum)),log(obj2@exeff[1,]),xlab="predicted",ylab="observed")
    lines(c(-10e10,10e10),c(-10e10,10e10),col='orange')
  }#funcy<-funcy+sum((par-0)^3)*20
  print(funcy)
  if(opt)return(funcy)
  if(!opt)return(obj2)
}

getEdist<-function(xfrom,yfrom,xto,yto,nmanage,CVd,stand){    # Calculates Euclidean distance from x to y, length(x) rows and length(y) columns
  nf<-length(xfrom)
  nt<-length(xto)
  xf<-array(xfrom,dim=c(nf,nt))
  xt<-array(rep(xto,each=nf),dim=c(nf,nt))
  yf<-array(yfrom,dim=c(nf,nt))
  yt<-array(rep(yto,each=nf),dim=c(nf,nt))
  dis<-sqrt(((xf-xt)^2)+((yf-yt)^2))
  dis<-array(rep(dis,each=nmanage)*geterr(nf*nt*nmanage,CVd),dim=c(nmanage,nf,nt))
  if(!stand)dis
  if(stand)dis/array(rep(apply(dis,1,mean),each=nf*nt),dim=c(nmanage,nf,nt)) # standardize to mean 1
}

#simlakemanage<-function(attrtype,nl){
#out<-array(NA,dim=c(nl,length(attrtype)))
#contcol<-(1:length(attrtype))[attrtype==1]
#catcol<-(1:length(attrtype))[attrtype>1]
#out[,contcol]<-getbias(length(contcol)*nl,0.5)
#out[,catcol]<-floor(runif(length(catcol)*nl)*rep(attrtype[catcol],each=nl))+1
#out
#}

esimlakemanage<-function(attrtype,nl,exn){
  out<-array(NA,dim=c(exn,nl,length(attrtype)))
  contcol<-(1:length(attrtype))[attrtype==1]
  catcol<-(1:length(attrtype))[attrtype>1]
  out[,,contcol]<-getbias(length(contcol)*nl*exn,0.2) # 20% CV
  out[,,catcol]<-floor(runif(length(catcol)*nl*exn)*rep(attrtype[catcol],each=nl*exn))+1
  out
}

fac2num<-function(x){
  x<-as.matrix(x)
  as.numeric(as.character(x))
  
}
sdconv<-function(m,sd)(log(1+((sd^2)/(m^2))))^0.5
mconv<-function(m,sd)log(m)-0.5*log(1+((sd^2)/(m^2)))
getbias<-function(nsim,CV) exp(rnorm(n=nsim,mconv(1,CV),sdconv(1,CV)))
tna<-function(x)sum(is.na(x))
ninety<-function(x)quantile(x,c(0.1,0.9))
#std<-function(x)x/mean(x)

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
      map(database = "worldHires", xlim=lonlim, ylim=latlim,resolution = 0,col=obj@topcols[1],mar=rep(0.3,4))  # make a first plot of the map to define the range of the plot
      #polygon(x=rep(lonlim*c(0.5,1.5),each=2),y=c(latlim*c(0.5,1.5),(latlim*c(0.5,1.5))[2:1]),col=colors()[257],border=NA)
      for(i in 1:length(obj@cont))lines(obj@cont[[i]]$x,obj@cont[[i]]$y,type="l",col=obj@topcols[(obj@cont[[i]]$level/500)+1])#lapply(bc@cont,plottop,cols=bc@topcols)
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


setMethod("summary",
          signature(object = "Landscape"),
          function(object,lcex=0.4,pcex=0.8,maxps=4){            
            
            obj<-object      
            
            lonlim<-range(c(obj@lakex,obj@pcx))+c(-1,1)
            latlim<-range(c(obj@lakey,obj@pcy))+c(-0.5,0.5)
            windows(width=(lonlim[2]-lonlim[1])*0.65,height=latlim[2]-latlim[1])
            par(mfrow=c(1,1),mar=rep(0,4))
            
            #jpeg(paste(Drive,":/Trout lakes/Images/map topo.jpg",sep=""),width=10, height=8, units="in", res=600)
            
            map(database = "worldHires", xlim=lonlim, ylim=latlim,resolution = 0,col=obj@topcols[1],mar=rep(0.3,4))  # make a first plot of the map to define the range of the plot
            #polygon(x=rep(lonlim*c(0.5,1.5),each=2),y=c(latlim*c(0.5,1.5),(latlim*c(0.5,1.5))[2:1]),col=colors()[257],border=NA)
            for(i in 1:length(obj@cont))lines(obj@cont[[i]]$x,obj@cont[[i]]$y,type="l",col=obj@topcols[(obj@cont[[i]]$level/500)+1])#lapply(bc@cont,plottop,cols=bc@topcols)
            #for(i in 1:length(obj@cont))polygon(obj@cont[[i]]$x,obj@cont[[i]]$y,col=obj@topcols[(obj@cont[[i]]$level/500)+1],border=obj@topcols[(obj@cont[[i]]$level/500)+1])#lapply(bc@cont,plottop,cols=bc@topcols)
            
            latadj<-0.1
            
            lar<-obj@lakearea/mean(obj@lakearea)
            cexy<-((0.1+lar*(4/max(lar)))*maxps)^0.5
            points(obj@lakex,obj@lakey,pch=16,cex=cexy,col="#0000ff90")
            textplot(obj@lakex,obj@lakey+latadj,obj@longnam,col="#0000ff90",cex=lcex,new=FALSE,show.lines=F,font=2)
            
            activepop<-obj@pcsize[1,]/mean(obj@pcsize[1,])
            cexy<-((0.1+activepop*(4/max(activepop)))*maxps)^0.5
            points(obj@pcx,obj@pcy,pch=16,cex=cexy,col="#ff000085")
            textplot(obj@pcx,obj@pcy+latadj,obj@pcnam,col="#ff000096",cex=pcex,new=FALSE,show.lines=F,font=2)
            
            abline(h=-360:360,col="#99999940")
            abline(v=-180:180,col="#99999940")
            
            polygon(c(-130,-128,-128,-130),c(48.5,48.5,49.5,49.5),col=NA,border="black")
            legend('topright',legend=c(paste("npc =",obj@npc),paste("nl =",obj@nl)),text.co=c("#ff000096","#0000ff90"),bty='n')
            
            out<-as.data.frame(cbind(obj@longnam,obj@lakenam,obj@lakearea,as.integer(obj@GDD[1,]),obj@lxslev[1,,],as.integer(obj@exeff[1,])))
            names(out)<-c("Name","Code","Area(ha)","GDD",paste("SR:",obj@stnam,sep=""),"Obs eff")
            out
            
          })



mapEconv<-function(obj,maxps=0.6,latlim=NA,lonlim=NA,itn){
  
  if(is.na(lonlim[1]))lonlim<-range(c(obj@lakex,obj@pcx))+c(0.5,-0.5)
  if(is.na(latlim[1]))latlim<-range(c(obj@lakey,obj@pcy))+c(-0.5,0.5)
  #windows(width=10,height=13)
  #par(mfrow=c(1,1),mar=rep(0,4))
  
  #jpeg(paste(Drive,":/Trout lakes/Images/map topo.jpg",sep=""),width=10, height=8, units="in", res=600)
  
  map(database = "worldHires", xlim=lonlim, ylim=latlim,resolution = 0,col=obj@topcols[1],mar=rep(0.3,4))  # make a first plot of the map to define the range of the plot
  polygon(x=rep(lonlim*c(0.5,1.5),each=2),y=c(latlim*c(0.5,1.5),(latlim*c(0.5,1.5))[2:1]),col=colors()[257],border=NA)
  for(i in 1:length(obj@cont))polygon(obj@cont[[i]]$x,obj@cont[[i]]$y,col=obj@topcols[(obj@cont[[i]]$level/500)+1],border=obj@topcols[(obj@cont[[i]]$level/500)+1])#lapply(bc@cont,plottop,cols=bc@topcols)
  
  latadj<-rep(0.3,obj@npc)
  eff<-obj@Econv[,itn]
  eff<-eff/mean(eff)
  cexy<-(1+eff*(4/max(eff)))*maxps
  points(obj@lakex,obj@lakey,pch=20,cex=cexy,col="#0000ff95")
  
  activepop<-obj@pcsize[1,]
  activepop<-activepop/mean(activepop)
  cexy<-(1+activepop*(4/max(activepop)))*maxps
  points(obj@pcx,obj@pcy,pch=20,cex=cexy,col="#ff000080")
  text(obj@pcx,obj@pcy+latadj,obj@pcnam,col="#ff000080",cex=0.9,lwd=2,font=2)
  
}


mapexeff<-function(.Object,maxps,latlim=NA,lonlim=NA){
  
  if(is.na(lonlim[1]))lonlim<-range(c(.Object@lakex,.Object@pcx))+c(0.5,-0.5)
  if(is.na(latlim[1]))latlim<-range(c(.Object@lakey,.Object@pcy))+c(-0.5,0.5)
  
  windows(width=lonlim[2]-lonlim[1],height=latlim[2]-latlim[1])
  #jpeg(paste(Drive,":/Trout lakes/Images/map topo.jpg",sep=""),width=10, height=8, units="in", res=600)
  par(mfrow=c(ceiling(.Object@exn/2),2),mai=rep(0.0001,4),omi=rep(0.0001,4))
  for(k in 1:.Object@exn){
    map(database = "worldHires", xlim=lonlim, ylim=latlim,resolution = 0,col=.Object@topcols[1])  # make a first plot of the map to define the range of the plot
    
    for(i in 1:length(bc@cont))lines(.Object@cont[[i]]$x,.Object@cont[[i]]$y,type="l",col=.Object@topcols[(.Object@cont[[i]]$level/500)+1])#lapply(bc@cont,plottop,cols=bc@topcols)
    
    latadj<-rep(0.3,.Object@npc)
    eff<-apply(.Object@exeff[,,,k],2,sum)
    eff<-eff/mean(eff)
    cexy<-(1+eff*(4/max(eff)))*maxps
    points(.Object@lakex,.Object@lakey,pch=20,cex=cexy,col="#0000ff80")
    
    activepop<-.Object@pcsize
    activepop<-activepop/mean(activepop)
    cexy<-(1+activepop*(4/max(activepop)))*maxps
    points(.Object@pcx,.Object@pcy,pch=20,cex=cexy,col="#ff000080")
    text(.Object@pcx,.Object@pcy+latadj,.Object@pcnam,col="#ff000080",cex=0.9,lwd=2,font=2)
  }
}


getlev<-function(x)x$level
plottop<-function(x,cols)lines(x$x,x$y,type="l",cols[(x$level/500)+1])
geterr<-function(n,CV)exp(rnorm(n,mconv(1,CV),sdconv(1,CV)))
CVs<-function(x)sd(x,na.rm=T)/mean(x,na.rm=T)

getmvlogerr<-function(x,CV,nsim){
  
  temp<-array(rep(log(x),each=nsim)*rnorm(nsim*prod(dim(x)),1,CV),dim=c(nsim,dim(x)))
  temp<-exp(temp)
  indlist<-new('list')
  indlist[[1]]<-1:nsim
  for(i in 1:length(dim(x)))indlist[[i+1]]<-1:(dim(x)[i])
  ind<-as.matrix(expand.grid(indlist))
  tempsum<-apply(temp,1:(length(dim(temp))-1),sum)
  temp[ind]<-temp[ind]/tempsum[ind[,1:(length(dim(temp))-1)]]
  temp  
  
}


plotranking<-function(obj){
  rnk<-obj@CB
  for(i in 1:obj@nsim)rnk[,i]<-obj@nl+1-rank(obj@CB[,i])
  cols<-heat.colors(ceiling(obj@nl*1.15))[1:obj@nl]
  y1<-(0:(obj@nl-1))/obj@nl
  y2<-1:obj@nl/obj@nl
  x1<-(0:(obj@nsim-1))/obj@nsim
  x2<-1:obj@nsim/obj@nsim
  y1<-rep(y1,obj@nsim)
  y2<-rep(y2,obj@nsim)
  x1<-rep(x1,each=obj@nl)
  x2<-rep(x2,each=obj@nl)
  ind<-as.matrix(expand.grid(1:obj@nl,1:obj@nsim))
  coly<-cols[rnk[ind]]
  windows()
  par(mai=c(0.5,0.5,0,0),omi=rep(0,4))
  plot(0:1,0:1,col="white",axes=F,xlab="",ylab="",main="")
  for(i in 1:nrow(ind))polygon(c(x1[i],x1[i],x2[i],x2[i]),c(y1[i],y2[i],y2[i],y1[i]),col=coly[i],border=coly[i])
  mtext("Simulation",1,line=0.3)
  mtext("Lake Ranking (red is high marginal benefit of stocking)",2,line=0.3)
}

getrank<-function(obj){
  rnk<-obj@CB
  for(i in 1:obj@nsim)rnk[,i]<-obj@nl+1-rank(obj@CB[,i])
  rnk
}

convEspec<-function(obj,tol=1,...){
  coly<-rainbow(n=20)
  conv<-obj@conv
  for(i in 1:20)conv[i,]<-std(conv[i,])
  #itn<-1
  #dif<-rep(1,20)
  #while(max(dif)>(tol/100)){
  #itn<-itn+1
  #higher<-conv[,itn]>conv[,itn-1]
  #dif[higher]<-(conv[higher,itn]-conv[higher,itn-1])/conv[higher,itn]
  # dif[!higher]<- (conv[!higher,itn-1]-conv[!higher,itn])/conv[!higher,itn-1]
  #}
  for(j in 1:20){
    plot(c(conv[1,1:j],rep(NA,20-j)),ylim=range(conv),col=coly[1],type="l",lwd=2,main="",xlab="Iteration",ylab="Effort (lake x angler x pop. centre)")
    #abline(v=itn,col="#00000050",lty=2)
    for(i in 2:20)lines(c(conv[i,1:j],rep(NA,20-j)),col=coly[i],lwd=2)
    ani.pause()
  }
}

convEtot<-function(obj,tol=1,...){
  
  conv<-obj@Econv[1:20,]
  for(i in 1:20)conv[i,]<-std(conv[i,])
  #itn<-1
  #dif<-rep(1,20)
  #while(max(dif)>(tol/100)){
  # itn<-itn+1
  #higher<-conv[,itn]>conv[,itn-1]
  #dif[higher]<-(conv[higher,itn]-conv[higher,itn-1])/conv[higher,itn]
  #dif[!higher]<- (conv[!higher,itn-1]-conv[!higher,itn])/conv[!higher,itn-1]
  #}
  for(j in 1:20){
    plot(c(conv[1,1:j],rep(NA,20-j)),ylim=range(conv),col="#0000ff90",type="l",lwd=2,main="",xlab="Iteration",ylab="Effort (lake x angler x pop. centre)")
    # abline(v=itn,col="#00000050",lty=2)
    for(i in 2:20)lines(c(conv[i,1:j],rep(NA,20-j)),col="#0000ff90",lwd=2)
    ani.pause()
  }
}


anieffort<-function(obj,...){
  maxps<-4
  lonlim<-c(-124, -119)
  latlim<-c(50.5,54.5)
  
  for(x in 1:20){                                     # B2  Calculate effort distribution resulting from setup file
    #mapEconv(obj,,itn=x)               # A5  User specified spatial range (zoom)
    map(database = "worldHires", xlim=lonlim, ylim=latlim,resolution = 0,col=obj@topcols[1],mar=rep(0.3,4))  # make a first plot of the map to define the range of the plot
    polygon(x=rep(lonlim*c(0.5,1.5),each=2),y=c(latlim*c(0.5,1.5),(latlim*c(0.5,1.5))[2:1]),col=colors()[257],border=NA)
    for(i in 1:length(obj@cont))polygon(obj@cont[[i]]$x,obj@cont[[i]]$y,col=obj@topcols[(obj@cont[[i]]$level/500)+1],border=obj@topcols[(obj@cont[[i]]$level/500)+1])#lapply(bc@cont,plottop,cols=bc@topcols)
    
    latadj<-rep(0.3,obj@npc)
    eff<-obj@Econv[,x]
    eff<-eff/mean(eff)
    cexy<-(1+eff*(4/max(eff)))*maxps
    points(obj@lakex,obj@lakey,pch=20,cex=cexy,col="#0000ff95")
    
    activepop<-obj@pcsize[1,]
    activepop<-activepop/mean(activepop)
    cexy<-(1+activepop*(4/max(activepop)))*maxps
    points(obj@pcx,obj@pcy,pch=20,cex=cexy,col="#ff000080")
    text(obj@pcx,obj@pcy+latadj,obj@pcnam,col="#ff000080",cex=2,lwd=2,font=2)
    
    ani.pause()
  }
}

ebar<-function(obj,maxbars=50){
  
  x11(width=20,height=10)
  totE<-apply(obj@eff[1,1,,,],2,sum)
  ind<-order(totE)
  barplot(totE[ind],names.arg=obj@lakenam[ind],las=2,cex.names=0.1,col='blue',ylab="Effort (days per year)",xlab="WBID",border='blue')
  
}

erank<-function(obj,rnk=10){
  
  totE<-apply(obj@eff[1,1,,,],2,sum)
  ind<-order(totE)
  rind<-c(1:rnk,(obj@nl-rnk+1):obj@nl)
  nind<-ind[rind]
  out<-as.data.frame(cbind(rind,obj@longnam[nind],floor(totE[nind])))
  names(out)<-c("Rank","WBID","Effort(dpy)")
  out
  
}  

ecomp<-function(obj){
  plot(apply(obj@eff[1,1,,,],2,sum),obj@exeff[1,],xlab="Predicted effort (angler days yr-1)",ylab="Observed effort (angler days yr-1)",main="",col='blue')
  lines(c(-10e10,10e10),c(-10e10,10e10),col="orange",lty=2) 
}  

calccouch<-function(obj){
  mspa<-as.matrix(expand.grid(1:obj@nmanage,1:obj@nsim,1:obj@npc,1:obj@na))
  #a<-mspa[,4]
  U<-apply(obj@U,c(1,2,3,5),sum) # sum of utility across lakes
  #frac<-as.numeric(obj@apr)/365
  couch<-array(NA,c(obj@nmanage,obj@nsim,obj@npc,obj@na))
  couch[mspa]<-((U[mspa]*365)/as.numeric(obj@apr))-U[mspa] 
  obj@couch=couch
  obj
}


calccouch2<-function(obj){
  mspa<-as.matrix(expand.grid(1:obj@nmanage,1:obj@nsim,1:obj@npc,1:obj@na))
  #a<-mspa[,4]
  U<-apply(obj@U,c(1,2,3,5),sum) # sum of utility across lakes
  maxdays<-as.matrix(obj@maxdays)
  #frac<-as.numeric(obj@apr)/365
  couch<-array(NA,c(obj@nmanage,obj@nsim,obj@npc,obj@na))
  couch[mspa]<-((U[mspa]*unlist(maxdays[mspa[,c(2,4)]]))/as.numeric(obj@apr))-U[mspa] 
  obj@couch=couch
  obj
}

plot_LvsU<-function(obj){
  
  U<-apply(obj@U[1,1,,,],c(1,3),sum)
  pop<-obj@pcxa[1,,]*array(obj@pcsize,c(obj@npc,obj@na))
  anam<-obj@anam
  par(mfrow=c(5,5),mai=c(0.3,0.1,0.1,0.1),omi=c(0.5,0.6,0.01,0.6))
  
  for(x in 1:obj@npc){
    y2=F
    if(x%in%((1:5)*5))y2=T
    y1=F
    if(x%in%((1:5)*5-4))y1=T
    tbar(x,U,pop,anam,pcnam=obj@pcnam[x],y1=y1,y2=y2)
    
  }
  
  mtext("Angler class",1,line=1.3,outer=T)
  mtext("Landscape utility (total)",2,line=2.5,outer=T)
  mtext("Number of anglers (,000 licenses sold)",4,line=1.9,outer=T,col='orange')
}

tbar<-function(x,U,pop,anam,pcnam,y1,y2){
  
  Ut<-U[x,]
  popt<-pop[x,]
  Un<-Ut/max(U)
  popn<-popt/max(pop)
  barplot(rbind(Un,popn), beside = TRUE,
          yaxt = "n", names.arg = anam,
          ylim=c(0, max(c(1, 1))),col=c('grey','orange'))
  
  myLeftAxisLabs <- pretty(seq(0, max(U), length.out = 10))
  myRightAxisLabs <- pretty(seq(0, max(pop/1000), length.out = 10))
  
  myLeftAxisAt <- myLeftAxisLabs/max(U)
  myRightAxisAt <- myRightAxisLabs/max(pop/1000)
  
  if(y1)axis(2, at = myLeftAxisAt, labels = myLeftAxisLabs)
  if(!y1)axis(2, at = myLeftAxisAt, labels = rep("",length(myLeftAxisAt)))
  if(y2)axis(4, at = myRightAxisAt, labels = myRightAxisLabs)
  if(!y2)axis(4,at=myRightAxisAt,labels=rep("",length(myRightAxisAt)))
  
  legend('top',legend=pcnam,bty='n',text.col='blue')
  
}

inreal<-function(vec,ndays=10000){
  evec<-exp(vec)
  evec/sum(evec)*ndays
}

Ecomp<-function(obj,funcy=NA,lcex=0.7,pcol=NA,pcex=0.7,log=T,nice=T,lims=NA,forpaper=F,dens=F,plottext=T,Rstat=T){
  
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

Ecomp_add<-function(obj,funcy=NA,lcex=0.7,pcol=NA,pcex=0.7,log=T,nice=T,lims=NA,forpaper=F,dens=F,plottext=T,Rstat=T,extraR2=NA){
  
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
    }else{
      x<-log(obj@exeff[1,]+tiny)
      y<-log(apply(obj@eff[1,1,,,],2,sum)+tiny)
    }
  }else{
    if(dens){
      x<-obj@exeff[1,]/obj@lakearea
      y<-apply(obj@eff[1,1,,,],2,sum)/obj@lakearea
    }else{
      x<-obj@exeff[1,]
      y<-apply(obj@eff[1,1,,,],2,sum)
    }
  }
  if(is.na(lims[1]))lims<-range(c(x,y),na.rm=T)
  if(!nice)points(x,y,col=pcol,pch=19)
  test<-lm(y~x-1)
  R2<-summary(test)$r.squared
  cond<-!is.na(x)
  
  if(plottext){
    if(nice){
      ttp(x[cond],y[cond],obj@longnam[cond],col=textcol,cex=lcex,pcex=pcex,pcol=pcol,new=F,xlim=lims,ylim=lims)
    }else{
      text(x[cond],y[cond],obj@longnam[cond],col=textcol,cex=lcex)
    }
  }
 
  if(Rstat){
    lines(c(-10e10,10e10),c(-10e10,10e10),col=one2one,lty=2)
    lines(seq(0,max(x,na.rm=T)*1.5,length.out=100),predict(test,newdat=data.frame(x=seq(0,max(x,na.rm=T)*1.5,length.out=100))),col=fitcol)
    texty=bquote(bold(R^2 == .(round(R2,3))))
    if(!is.na(funcy))legend('bottomright',legend=c(texty,paste("f =",round(funcy,3))),text.col=fitcol,bty='n')
    if(is.na(funcy)){
      if(is.na(extraR2[1])){
        legend('bottomright',legend=texty,text.col=fitcol,bty='n')
      }else{
        legend('bottomright',legend=texty,text.col=fitcol,bty='n')
        legend('right',legend=extraR2,text.col='grey',bty='n')
      }  
    }
  }
}

EcompD<-function(obj,funcy=NA,lcex=0.7,log=T,nice=T,lims=NA){
  
  if(log){
    x<-log(obj@exeff[1,]/obj@lakearea+tiny)
    y<-log(apply(obj@eff[1,1,,,],2,sum)/obj@lakearea+tiny)
    if(is.na(lims[1]))lims<-range(x,y,na.rm=T)
    if(nice)plot(x,y,xlab="Observed effort (log)",ylab="Predicted effort (log)",col="white",xlim=lims,ylim=lims)
    if(!nice)plot(x,y,xlab="Observed effort (log)",ylab="Predicted effort (log)",col="#ff000080",pch=19,xlim=lims,ylim=lims)
  }else{
    x<-obj@exeff[1,]/obj@lakearea
    y<-apply(obj@eff[1,1,,,],2,sum)/obj@lakearea
    if(is.na(lims[1]))lims<-range(x,y,na.rm=T) 
    if(nice)plot(x,y,xlab="Observed effort",ylab="Predicted effort",pch=19,col="white",xlim=lims,ylim=lims)
    if(!nice)plot(x,y,xlab="Observed effort",ylab="Predicted effort",pch=19,col="#ff000080",xlim=lims,ylim=lims)
  }
  test<-lm(y~x-1)
  R2<-summary(test)$r.squared
  cond<-!is.na(x)
  if(nice){
    textplot(x[cond],y[cond],obj@longnam[cond],col="#0000ff90",cex=lcex,new=F,xlim=lims,ylim=lims)
  }else{
    text(x[cond],y[cond],obj@longnam[cond],col="#0000ff90",cex=lcex)
  }         
  lines(c(-10e10,10e10),c(-10e10,10e10),col='blue')
  lines(seq(0,max(x,na.rm=T),length.out=100),predict(test,newdat=data.frame(x=seq(0,max(x,na.rm=T),length.out=100))),col='red')
  legend('bottomright',legend=c(paste("R2 =",round(R2,3)),paste("f =",round(funcy,3))),text.col="red",bty='n')
}

CRcomp<-function(obj,funcy=NA,lcex=0.7,pcex=1,pcol=NA,log=T,nice=T,forpaper=F,plottext=F){
  
  crpred<-obj@cr[1,1,,]*apply(obj@eff[1,1,,,],2:3,sum)
  crpred<-apply(crpred,1,sum)/apply(obj@eff[1,1,,,],2,sum)
  
  x<-obj@excr[1,]
  y<-crpred
  
  cond<-!is.na(x)&!is.na(y)
  x<-x[cond]
  y<-y[cond]
  
  if(forpaper){
    if(is.na(pcol))pcol=makeTransparent('black',200)
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
    x<-log(x+tiny)
    y<-log(y+tiny)
    if(nice)plot(x,y,xlab="Observed catch rate (log of fish per hour)",ylab="Predicted catch rate (log of fish per hour)",col=pcol)
    if(!nice)plot(x,y,xlab="Observed catch rate (log of fish per hour)",ylab="Predicted catch rate (log of fish per hour)",col=pcol,pch=19)
  }else{
    if(nice)plot(x,y,xlab="Observed catch rate (fish per hour)",ylab="Predicted catch rate (fish per hour)",pch=19,col=pcol)
    if(!nice)plot(x,y,xlab="Observed catch rate (fish per hour)",ylab="Predicted catch rate (fish per hour) ",pch=19,col=pcol)
  }
  
  test<-lm(y~x-1)
  R2<-summary(test)$r.squared
  if(plottext){
   if(nice){
    ttp(x,y,obj@longnam[cond],col=textcol,pcex=pcex,pcol=pcol,cex=lcex,new=F)
   }else{
    text(x,y,obj@longnam[cond],col=textcol,cex=lcex)
   }
  }  
  lines(c(-10e10,10e10),c(-10e10,10e10),col=one2one,lty=2)
  lines(seq(-2,max(x),length.out=100),predict(test,newdat=data.frame(x=seq(-2,max(x),length.out=100))),col=fitcol)
  texty=bquote(bold(R^2 == .(round(R2,3))))
  if(!is.na(funcy))legend('bottomright',legend=c(texty,paste("f =",round(funcy,3))),text.col=fitcol,bty='n')
  if(is.na(funcy))legend('bottomright',legend=texty,text.col=fitcol,bty='n')
  
}


AVScomp<-function(obj,funcy=NA,lcex=0.7,log=T,nice=T){
  
  x<-obj@exavs[1,]
  y<-obj@avs[1,1,]
  cond<-!is.na(x)&!is.na(y)
  x<-x[cond]
  y<-y[cond]
  
  if(log){
    x<-log(x+tiny)
    y<-log(y+tiny)
    if(nice)plot(x,y,xlab="Observed average size (log)",ylab="Predicted average size (log)",col="white")
    if(!nice)plot(x,y,xlab="Observed average size (log)",ylab="Predicted average size (log)",col="#ff000080",pch=19)
  }else{
    if(nice)plot(x,y,xlab="Observed average size",ylab="Predicted average size",pch=19,col="white")
    if(!nice)plot(x,y,xlab="Observed average size",ylab="Predicted average size",pch=19,col="#ff000080")
  }
  test<-lm(y~x)
  R2<-summary(test)$r.squared
  if(nice){
    textplot(x,y,obj@longnam[cond],col="#99999998",cex=lcex,new=F)
  }else{
    text(x,y,obj@longnam[cond],col="#99999998",cex=lcex)
  }   
  
  lines(c(-10e10,10e10),c(-10e10,10e10),col='blue')
  lines(seq(-2,max(x),length.out=100),predict(test,newdat=data.frame(x=seq(-2,max(x),length.out=100))),col='red')
  legend('bottomright',legend=c(paste("R2 =",round(R2,3)),paste("f =",round(funcy,3))),text.col="red",bty='n')
}

optEfun<-function(par,obj,nits=15,opt=T,ploty=F,varind=0.15,uprat=0.05){
  totmulti<-exp(par[1])
  obj2<-obj
  obj2@apr<-obj2@apr*totmulti*regmulti[region]
  obj2<-calceff(obj2,nits=nits,varind=varind,uprat=uprat,startE=T,quiet=T) 
  obj2@exeff[obj2@exeff<0.01]<-0.01
  dif<-log(obj2@exeff[1,]+tiny)-log(apply(obj2@eff[1,1,,,],2,sum)+tiny)
  print(exp(par))
  funcy<--sum(dnorm(dif,0,0.5,log=T),na.rm=T) # negative log likelihood of log effort difference
  if(ploty)Ecomp(obj2,funcy)
  print(funcy)
  if(opt)return(funcy)
  if(!opt)return(obj2)
}

optEfunA<-function(par,obj,nits=15,opt=T,ploty=F,varind=0.15,uprat=0.05,log=T,quiet=T){
  
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



optEfunB<-function(par,obj,nits=200,opt=T,ploty=F,varind=0.15,uprat=0.05){
  
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


optEfun1b<-function(par,obj,nits=15,opt=T,ploty=F,varind=0.15,uprat=0.05,log=T,quiet=T){
  
  pcmulti<-exp(par[1])
  obj2<-obj
  obj2@apr<-obj2@apr*pcmulti
 
  obj2<-calceff(obj2,nits=nits,startE=T,quiet=quiet) 
  obj2@exeff[obj2@exeff<0.01]<-0.01
  dif<-log(obj2@exeff[1,]+tiny)-log(apply(obj2@eff[1,1,,,],2,sum)+tiny)
  
  #print(exp(par))
  print(paste("pcmulti =",pcmulti))
  #print(rbind(obj@longnam[!is.na(obj@exeff[1,])],dif[!is.na(dif)]))
  #print(mean(dif,na.rm=T))
  #funcy<-sum(dif^2,na.rm=T)
  funcy<--sum(dnorm(dif,0,0.5,log=T),na.rm=T) # negative log likelihood of log effort difference
  #funcy<-funcy-sum(dlnorm(exp(par),0,2,log=T)) # negative log likelihood of multiplicative deviation from couch effect
  if(ploty)Ecomp(obj2,funcy,nice=F,plottext=F,lims=c(1,10))
  print(funcy)
  if(opt)return(funcy)
  if(!opt)return(obj2)
} 

optEfun2<-function(par,obj,nits=15,opt=T,ploty=F,varind=0.15,uprat=0.05,log=T){
  
  pcmulti<-c(1,exp(par[2:length(par)]))
  #pcmulti<-c(1,exp(par))
  totmulti<-exp(par[1])
  obj2<-obj
  obj2@apr<-obj2@apr*totmulti*pcmulti
  obj2<-calceff(obj2,nits=nits,startE=T,varind=varind,uprat=uprat) 
  obj2@exeff[obj2@exeff<0.01]<-0.01
  dif<-log(obj2@exeff[1,]+tiny)-log(apply(obj2@eff[1,1,,,],2,sum)+tiny)
  
  #dif<-(obj2@exeff[1,]-apply(obj2@eff[1,1,,,],2,sum))
  dif<-(dif^2)^0.5
  #print(totmulti)
  #print(pcmulti)
  cat("\n")
  cat("--------------------")
  cat("\n")
  print(rbind(substr(obj@pcnam,1,6) ,round(totmulti*pcmulti*100,2)))
  #print(rbind(obj@longnam[!is.na(obj@exeff[1,])],dif[!is.na(dif)]))
  #print(round(mean(dif,na.rm=T)),3)
  #f#uncy<-sum(dif^2,na.rm=T)
  funcy<--sum(dnorm(dif,0,0.5,log=T),na.rm=T) # negative log likelihood of log effort difference
  #funcy<-funcy-sum(dlnorm(exp(par),0,2,log=T)) # negative log likelihood of multiplicative deviation from couch effect
  if(ploty)Ecomp(obj2,funcy,log=log,nice=F)
  cat(funcy)
  cat("\n")
  if(opt)return(funcy)
  if(!opt)return(obj2)
}

optEfun2b<-function(par,obj,nits=15,opt=T,ploty=F,varind=0.15,uprat=0.05,log=T){
  
  pcmulti<-exp(par)
  obj2<-obj
  obj2@apr<-obj2@apr*pcmulti
  obj2<-calceff(obj2,nits=nits,startE=T,varind=varind,uprat=uprat) 
  obj2@exeff[obj2@exeff<0.01]<-0.01
  dif<-((obj2@exeff[1,]-apply(obj2@eff[1,1,,,],2,sum))^2)^0.5
  
  #dif<-(obj2@exeff[1,]-apply(obj2@eff[1,1,,,],2,sum))
  #dif<-(dif^2)^0.5
  #print(totmulti)
  #print(pcmulti)
  cat("\n")
  cat("--------------------")
  cat("\n")
  print(rbind(substr(obj@pcnam,1,6) ,round(pcmulti*100,2)))
  #print(rbind(obj@longnam[!is.na(obj@exeff[1,])],dif[!is.na(dif)]))
  #print(round(mean(dif,na.rm=T)),3)
  #f#uncy<-sum(dif^2,na.rm=T)
  funcy<-sum(dif)#-sum(dnorm(dif,0,100,log=T),na.rm=T) # negative log likelihood of log effort difference
  funcy<-funcy+sum((par^2)^0.5*1000)#sum(dlnorm(exp(par),0,2,log=T)) # negative log likelihood of multiplicative deviation from couch effect
  if(ploty)Ecomp(obj2,funcy,log=log,nice=F)
  cat(funcy)
  cat("\n")
  if(opt)return(funcy)
  if(!opt)return(obj2)
}


optEfun3<-function(par,obj,nits,opt=T,ploty=F){
  
  totmulti<-exp(par)
  obj2<-obj
  obj2@apr<-obj2@apr*totmulti
  obj2<-calceff(obj2,nits=nits,startE=T) 
  #obj2@exeff[obj2@exeff<0.01]<-0.01
  #dif<-log(obj2@exeff[1,]+tiny)-log(apply(obj2@eff[1,1,,,],2,sum)+tiny)
  
  dif<-(obj2@exeff[1,]-apply(obj2@eff[1,1,,,],2,sum))
  dif<-(dif^2)^0.5
  print(totmulti)
  print(rbind(obj@longnam[!is.na(obj@exeff[1,])],dif[!is.na(dif)]))
  print(round(mean(dif,na.rm=T)),3)
  #f#uncy<-sum(dif^2,na.rm=T)
  funcy<-sum(dif)#-sum(dnorm(dif,0,1,log=T),na.rm=T) # negative log likelihood of log effort difference
  #funcy<-funcy-sum(dlnorm(exp(par),0,2,log=T)) # negative log likelihood of multiplicative deviation from couch effect
  if(ploty)Ecomp(obj2,funcy,nice=F)
  print(funcy)
  if(opt)return(funcy)
  if(!opt)return(obj2)
}


optEfun4<-function(par,obj,nits=15,opt=T,ploty=F,varind=0.15,uprat=0.05,log=T){
  
  obj2<-obj
  obj2@acc_a<-par
  obj2<-calceff(obj2,nits=nits,startE=T,varind=varind,uprat=uprat) 
  obj2@exeff[obj2@exeff<0.01]<-0.01
  dif<-obj2@exeff[1,]-apply(obj2@eff[1,1,,,],2,sum)
  dif<-(dif^2)^0.5
  print(par)
  funcy<-sum(dif)#-sum(dnorm(dif,0,0.5,log=T),na.rm=T) # negative log likelihood of log effort difference
  if(ploty)Ecomp(obj2,funcy,log=log,nice=F)
  print(funcy)
  if(opt)return(funcy)
  if(!opt)return(obj2)
}


optEfun5<-function(par,obj,nits,opt=T,ploty=F,varind=0.15,uprat=0.05){
  
  qmulti<-exp(par)
  obj2<-obj
  obj2@aq<-obj2@aq*qmulti
  obj2<-calceff(obj2,nits=nits,startE=T) 
  #obj2@exeff[obj2@exeff<0.01]<-0.01
  #dif<-log(obj2@exeff[1,]+tiny)-log(apply(obj2@eff[1,1,,,],2,sum)+tiny)
  
  dif<-(obj2@exeff[1,]-apply(obj2@eff[1,1,,,],2,sum))
  dif<-(dif^2)^0.5
  print(qmulti)
  print(rbind(obj@longnam[!is.na(obj@exeff[1,])],dif[!is.na(dif)]))
  print(round(mean(dif,na.rm=T)),3)
  #f#uncy<-sum(dif^2,na.rm=T)
  funcy<-sum(dif)#-sum(dnorm(dif,0,1,log=T),na.rm=T) # negative log likelihood of log effort difference
  #funcy<-funcy-sum(dlnorm(exp(par),0,2,log=T)) # negative log likelihood of multiplicative deviation from couch effect
  if(ploty)Ecomp(obj2,funcy,nice=F,log=F)
  print(funcy)
  if(opt)return(funcy)
  if(!opt)return(obj2)
}

optEfun6<-function(par,obj,nits=200,opt=T,ploty=F,varind=0.15,uprat=0.05){
  
  DR<-exp(par)/(1+exp(par))
  obj2<-obj
  obj2@DR<-matrix(rep(DR,obj@na),nrow=1)
  obj2<-calceff(obj2,nits=nits,startE=T,quiet=T) 
  dif<-obj2@exeff[1,]-apply(obj2@eff[1,1,,,],2,sum)
  dif<-(dif^2)^0.5
  print(DR)
  #print(rbind(obj@longnam[!is.na(obj@exeff[1,])],dif[!is.na(dif)]))
  #print(round(mean(dif,na.rm=T)),3)
  #f#uncy<-sum(dif^2,na.rm=T)
  funcy<-sum(dif,na.rm=T)#-sum(dnorm(dif,0,1,log=T),na.rm=T) # negative log likelihood of log effort difference
  #funcy<-funcy-sum(dlnorm(exp(par),0,2,log=T)) # negative log likelihood of multiplicative deviation from couch effect
  if(ploty)Ecomp(obj2,funcy,nice=F,log=T,plottext=F,lim=c(3,10))
  print(funcy)
  if(opt)return(funcy)
  if(!opt)return(obj2)
  
}

optEfun7<-function(par,obj,nits,opt=T,ploty=F,varind=0.15,uprat=0.05){
  
  #if(ploty)par(mfrow=c(2,1))
  DR<-exp(par)/(1+exp(par))
  obj2<-obj
  obj2@DR<-matrix(rep(DR,obj@na),nrow=1)
  obj2<-calceff(obj2,nits=nits,startE=T) 
  
  dif<-obj2@exeff[1,]-apply(obj2@eff[1,1,,,],2,sum)
  dif<-(dif^2)^0.5
  
  crpred<-obj@cr[1,1,,]*apply(obj@eff[1,1,,,],2:3,sum)
  crpred<-apply(crpred,1,sum)/apply(obj@eff[1,1,,,],2,sum)
  x<-obj@excr[1,]
  y<-crpred
  cond<-!is.na(x)&!is.na(y)
  x<-x[cond]
  y<-y[cond]
  
  dif2<-((x-y)^2)^0.5
  dif2<-dif2*200
  
  print(DR)
  print(rbind(obj@longnam[!is.na(obj@exeff[1,])],dif[!is.na(dif)]))
  print(round(mean(dif,na.rm=T)),3)
  #f#uncy<-sum(dif^2,na.rm=T)
  funcy<-sum(dif,dif2)#-sum(dnorm(dif,0,1,log=T),na.rm=T) # negative log likelihood of log effort difference
  #funcy<-funcy-sum(dlnorm(exp(par),0,2,log=T)) # negative log likelihood of multiplicative deviation from couch effect
  if(ploty){
    Ecomp(obj2,funcy,nice=F,log=F)
    CRcomp(obj2,log=F)  
  }
  print(funcy)
  if(opt)return(funcy)
  if(!opt)return(obj2)
  
}


optEfun8<-function(par,obj,nits=15,opt=T,ploty=F,varind=0.15,uprat=0.05,log=T){
  
  obj2<-obj
  obj2@acc_a<-par
  obj2<-calceff(obj2,nits=nits,startE=T,varind=varind,uprat=uprat) 
  obj2@exeff[obj2@exeff<0.01]<-0.01
  dif<-obj2@exeff[1,]-apply(obj2@eff[1,1,,,],2,sum)
  dif<-(dif^2)^0.5
  
  crpred<-obj@cr[1,1,,]*apply(obj@eff[1,1,,,],2:3,sum)
  crpred<-apply(crpred,1,sum)/apply(obj@eff[1,1,,,],2,sum)
  x<-obj@excr[1,]
  y<-crpred
  cond<-!is.na(x)&!is.na(y)
  x<-x[cond]
  y<-y[cond]
  
  dif2<-((x-y)^2)^0.5
  dif2<-dif2*200
  
  print(par)
  funcy<-sum(dif,dif2)#-sum(dnorm(dif,0,0.5,log=T),na.rm=T) # negative log likelihood of log effort difference
  if(ploty){
    Ecomp(obj2,funcy,log=log,nice=F)
    CRcomp(obj,log=F)
  }
  print(funcy)
  if(opt)return(funcy)
  if(!opt)return(obj2)
}




rres<-function(obj){
  #obj@acc[1,]<-log(obj@exeff[1,])-log(obj@sigeff[1,1,])
}

comp2<-function(obj,i){
  effa<-apply(obj@eff[1,1,,i,],2,sum)
  cra<-obj@cr[1,1,i,]
  mat<-data.frame(t(matrix(
    c(obj@exeff[1,i],obj@sigeff[1,1,i]     ,effa,
      obj@excr[1,i],sum(cra*effa)/sum(effa),cra,
      obj@exavs[1,i],obj@avs[1,1,i],rep(obj@avs[1,1,i],obj@na)),nrow=2+obj@na)))
  row.names(mat)<-c("Effort","Catch rate","Size")
  names(mat)<-c("Observed","Predicted",obj@anam)
  print(mat)
}


makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

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



ERedist<-function(obj1,obj2,lakeno,box=1,maxcex=5){
  
  xlim=obj1@lakex[lakeno]+c(-box/2,box/2)
  ylim=obj1@lakey[lakeno]+c(-box/2,box/2)
  map(database = "worldHires", xlim=xlim, ylim=ylim,resolution = 0,fill=T,col="light grey",mar=rep(0.3,4),add=F,lwd=1)#mak
  dif<-apply(obj2@eff[1,1,,,],2,sum)-apply(obj1@eff[1,1,,,],2,sum)
  
  print(sum(obj2@eff))
  print(sum(obj1@eff))
  
  cond<-obj1@lakex>xlim[1]&obj1@lakex<xlim[2]&obj1@lakey>ylim[1]&obj1@lakey<ylim[2]
  points<-(dif[cond]^2)^0.5
  #points[match(obj1@longnam[lakeno],obj1@longnam[cond])]<-0
  points<-points*(maxcex/mean(points))
  col<-rep('#ffffff98',length(points))
  col[dif[cond]>0]<-'#99999998'
  col[match(obj1@longnam[lakeno],obj1@longnam[cond])]<-"#ff000098"
  points(obj1@lakex[cond],obj1@lakey[cond],col=col,cex=points^0.5,pch=19)
  
}

ERedist2<-function(obj1,obj2,obj3,obj4,lakenos,box=1){
  
  bcol<-'black'
  bw<-2
  pcol<-'grey60'   #rgb(0,0,0,0.45)
  par(mfrow=c(2,2),mai=c(0.4,0.4,0.2,0.01),omi=c(0.4,0.4,0.01,0.01))
    
  lonlim<-c(-125,-118)#range(c(obj@lakex,obj@pcx))+c(0.5,-0.5)
  latlim<-c(49,54)#range(c(obj@lakey,obj@pcy))+c(-0.5,0.5)
  
  topcols<-gray.colors(length(obj@topcols), start = 0, end = 0.7, gamma = 2.2, alpha = NULL)
  topcols[1]<-'black'
  topcols[2:length(topcols)]<-"grey92"#topcols[length(topcols)]
  topcols2<-topcols
  topcols2[2:length(topcols)]<-"grey85"#topcols[length(topcols)]
  
  arrowplot(obj1,obj2,lakeno=lakenos[1],topcols2)
  mtext("(a) Kestrel Lake",3,line=0.02,adj=0.05)
  
  arrowplot(obj1,obj4,lakenos[3],topcols2)
  mtext("(b) Sheridan Lake",3,line=0.02,adj=0.05)
  
  
  plot(lonlim,latlim,col='white',axes=F)
  #map(database = "worldHires", xlim=lonlim, ylim=latlim,resolution = 0,fill=T,col="white",mar=rep(0.3,4),add=F,lwd=1)#mak
  for(i in 1:length(obj@cont))lines(obj@cont[[i]]$x,obj@cont[[i]]$y,type="l",col=topcols[(obj@cont[[i]]$level/500)+1],lwd=0.75)#lapply(bc@cont,plottop,cols=bc@topcols)
  #axis(1,seq(lonlim[1],lonlim[2],by=1),seq(lonlim[1],lonlim[2],by=1))
  #axis(2,seq(latlim[1],latlim[2],by=1),seq(latlim[1],latlim[2],by=1))
  axis(1,-(150:90),-(150:90))
  axis(2,0:90,0:90)
  
  points(obj@lakex,obj@lakey,pch=16,cex=0.6,col=pcol)
  
  xlim2=obj2@lakex[lakenos[1]]+c(-box/2,box/2)
  ylim2=obj2@lakey[lakenos[1]]+c(-box/2.5,box/2.5)
 
  polygon(c(xlim2[1],xlim2[1],xlim2[1]+0.32,xlim2[1]+0.32),c(ylim2[2]-0.2,ylim2[2],ylim2[2],ylim2[2]-0.2),border=NA,col=rgb(1,1,1,0.5))
  polygon(xlim2[c(1,1,2,2)],ylim2[c(1,2,2,1)],border=bcol,lwd=bw)
  text(xlim2[1]+0.16,ylim2[2]-0.1,"a",cex=0.75,font=2)
  
  xlim3=obj3@lakex[lakenos[2]]+c(-box/2,box/2)
  ylim3=obj3@lakey[lakenos[2]]+c(-box/2.5,box/2.5)
  
  polygon(c(xlim3[1],xlim3[1],xlim3[1]+0.32,xlim3[1]+0.32),c(ylim3[2]-0.2,ylim3[2],ylim3[2],ylim3[2]-0.2),border=NA,col=rgb(1,1,1,0.5))
  polygon(xlim3[c(1,1,2,2)],ylim3[c(1,2,2,1)],border=bcol,lwd=bw)
  text(xlim3[1]+0.16,ylim3[2]-0.1,"d",cex=0.75,font=2)
  
  xlim4=obj4@lakex[lakenos[3]]+c(-box/2,box/2)
  ylim4=obj4@lakey[lakenos[3]]+c(-box/2.5,box/2.5)
  
  polygon(c(xlim4[1],xlim4[1],xlim4[1]+0.32,xlim4[1]+0.32),c(ylim4[2]-0.2,ylim4[2],ylim4[2],ylim4[2]-0.2),border=NA,col=rgb(1,1,1,0.5))
  polygon(xlim4[c(1,1,2,2)],ylim4[c(1,2,2,1)],border=bcol,lwd=bw)
  text(xlim4[1]+0.16,ylim4[2]-0.1,"b",cex=0.75,font=2)
  
  
  points(obj@lakex[lakenos],obj@lakey[lakenos],pch=16,cex=1.1,col=bcol)
  
  mtext("(c) Location of lakes",3,line=0.02,adj=0.05)
  
  #lonlim<-c(-123,-120)#range(c(obj@lakex,obj@pcx))+c(0.5,-0.5)
  #latlim<-c(51,53.5)#range(c(obj@lakey,obj@pcy))+c(-0.5,0.5)
  #map(database = "worldHires", xlim=lonlim, ylim=latlim,resolution = 0,fill=T,col="light grey",mar=rep(0.3,4),add=F,lwd=1)#mak
  #text(obj@lakex,obj@lakey,obj@longnam,col="#99000050",cex=0.5)
  

  arrowplot(obj1,obj3,lakenos[2],topcols2)
  mtext("(d) Roche Lake",3,line=0.02,adj=0.05)
 
  mtext(expression(paste("Longitude ",~degree~W,sep="")),side=1,line=0.9,outer=T,font=2,cex=1)
  mtext(expression(paste("Latitude ",~degree~N,sep="")),side=2,line=0.7,outer=T,font=2,cex=1)
  
}



arrowplot<-function(obj1,obj2,lakeno,topcols,box=1,awd=6,acol='black'){
 
  acol=rgb(0, 0, 0, 0.45)
  xlim=obj1@lakex[lakeno]+c(-box/2,box/2)
  ylim=obj1@lakey[lakeno]+c(-box/2.5,box/2.5)
  #map(database = "worldHires", xlim=xlim, ylim=ylim,resolution = 0,fill=T,col="light grey",mar=rep(0.3,4),add=F,lwd=1)#mak
  plot(xlim,ylim,col='white',axes=F)
  #map(database = "worldHires", xlim=lonlim, ylim=latlim,resolution = 0,fill=T,col="white",mar=rep(0.3,4),add=F,lwd=1)#mak
  for(i in 1:length(obj1@cont))lines(obj1@cont[[i]]$x,obj1@cont[[i]]$y,type="l",col=topcols[(obj1@cont[[i]]$level/500)+1],lwd=0.75)#lapply(bc@cont,plottop,cols=bc@topcols)
  axis(1,seq(-150,-90,by=0.5),seq(-150,-90,by=0.5))
  axis(2,seq(0,90,by=0.5),seq(0,90,by=0.5))
  
  dif<--(apply(obj2@eff[1,1,,,],2,sum)-apply(obj1@eff[1,1,,,],2,sum))
  dif[lakeno]<-0
  
  percrdon<-(sum(obj2@cr[1,1,(1:obj@nl)!=lakeno,])-sum(obj1@cr[1,1,(1:obj@nl)!=lakeno,]))/sum(obj1@cr[1,1,(1:obj@nl)!=lakeno,])*100
  percrrec<-(sum(obj2@cr[1,1,lakeno,])-sum(obj1@cr[1,1,lakeno,]))/sum(obj1@cr[1,1,lakeno,])*100
  
  
  perchange<-round((sum(dif)/sum(obj1@eff[1,1,,lakeno,]))*100,0)
  
  
  cond<-obj1@lakex>xlim[1]&obj1@lakex<xlim[2]&obj1@lakey>ylim[1]&obj1@lakey<ylim[2]&(1:obj1@nl)!=lakeno
  lno<-(1:obj1@nl)[cond]
  
  arrowwd<-0.8+awd*dif[cond]/(max(dif[cond]))
  
  tox<-obj1@lakex[lakeno]
  toy<-obj1@lakey[lakeno]
  
  for(i in 1:length(lno)){
    
    if(lno[i]!=lakeno){
      fromx<-obj@lakex[lno[i]]
      fromy<-obj@lakey[lno[i]]
      Arrows(fromx,fromy,tox+(fromx-tox)*0.3,toy+(fromy-toy)*0.3,lwd=arrowwd[i],col=acol,arr.type='triangle',arr.width=arrowwd[i]/20,arr.length=arrowwd[i]/20)
    } 
    
  }
  minE<-min(dif[cond],na.rm=T)
  maxE<-max(dif[cond],na.rm=T)
  intE<-mean(c(minE,maxE))
  
  legend('topright',legend=ceiling(c(minE,intE,maxE)),title="Angler days",
                    lwd=c(min(arrowwd),mean(c(min(arrowwd),max(arrowwd))),max(arrowwd)),
                    col=acol,box.col=NA,bg=rgb(1,1,1,0.7))
  
  legend('bottomright',title="Effort for receiving lake",legend=c(paste0(round(sum(dif),0)," day increase"),
                                paste0(perchange,"% increase")),
                                box.col=NA,bg=rgb(1,1,1,0.7),cex=0.95)
  
  legend('topleft',title="Change in catch rate",legend=c(paste0("Donating lakes: ",round(percrdon,1),"%"),
                                paste0("Receiving lake: ",round(percrrec,1),"%")),
                                box.col=NA,bg=rgb(1,1,1,0.7),cex=0.95)
  
}

sdconv <- function(m,sd)(log(1+((sd^2)/(m^2))))^0.5  
# get log normal mean from transformed space mean and standard deviation      
mconv<-function(m,sd)log(m)-0.5*log(1+((sd^2)/(m^2)
))    
alphaconv<-function(m,sd)m*(((m*(1-m))/(sd^2))-1)

betaconv<-function(m,sd)(1-m)*(((m*(1-m))/(sd^2))-1)

trlnorm<-function(reps,mu,cv) {
  if (all(is.na(mu))) return(rep(NA, reps))
  if (all(is.na(cv))) return(rep(NA, reps))
  return(rlnorm(reps,mconv(mu,mu*cv),sdconv(mu,mu*cv)))
}




print("Source code for landscape-scale simulator loaded (v.1.24 beta, Sept 2015)")
