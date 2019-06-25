# ==============================================================================================================================================================
# === Landscape object =========================================================================================================================================
# ==============================================================================================================================================================


# ---- Data Class ----

#' Class \code{'Landscape'}
#'
#' An object for storing all attributes of the landscape including anglers, population centres and lakes
#'
#'
#' @name Data-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new('Data', stock)}
#' @slot Name Character string. The name of the Data object. Single value. Character string
#' @slot Note Character string. Any relevant information
#' @slot npc Integer. The number of population centres
#' @slot nl Integer. The number of lakes
#' @slot na Integer. The number of angler classes
#' @slot nattr Integer. The number of lake attributes.
#' @slot ncat Integer. The number of categorical lake attributes.
#' @slot nsim Integer. The number of simulations (landscape simulations per management mode).
#' @slot nmanage Integer. The number of management modes.
#' @slot nage Integer. The maximum number of fish age classes modelled
#' @slot nst Integer. The number of stocking types (e.g. fry, fingerlings etc)
#' @slot nattrvals Integer. The maximum number of attribute values for each categorical lake attribute
#' @slot pcnam Character value. The name of each population centre [population centre].
#' @slot pcsize Array. The number of anglers in each population [sim, population centre].
#' @slot pcx Numeric. The longitude of each population centre [population centre].
#' @slot pcy Numeric. The latitude of each population centre [population centre].
#' @slot lakenam Character. The name of each lake (short form lake code) [lake].
#' @slot longnam Character. The long name of each lake [lake].
#' @slot lakex Numeric. The longitude of each lake [lake].
#' @slot lakey Numeric. The latitute of each lake [lake].
#' @slot lakearea Numeric. The surface area of the lake in hectares [lake].
#' @slot lxslev 3D Array. The stocking level [management mode, lake, stocking type].
#' @slot GDD  Array. Growing degree days for each lake [sim, lake].
#' @slot TDS Array Total disolved solids for each lake [sim, lake].
#' @slot stockable NOT IN USE. Whether the lake is stocked [lake].
#' @slot anam Character. The name of the angler classes [angler class].
#' @slot apr 3D Array. The angler participation rate - mean effort per license [simulation, population centre, angler class].
#' @slot aq Array. The angler catchabilty coefficient [sim, angler class].
#' @slot attr data.frame. The attributes of each angler class.
#' @slot pcxl 3D array. The distance of each population centre to each lake, pc x l [management mode, population centre, lake].
#' @slot pcxa 3D array. The fraction of each angler class in each population centre, pc x a [simulation, population centre, angler class].
#' @slot lxattr 3D array. Lake attributes l x attr [management mode, lake, lake attribute].
#' @slot axattr 3D array. Angler attributes a x attr [sim, population centre, angler class].
#' @slot eff 5D array. Output. The calculated effort on each lake [management option, simulation, population centre, lake, angler class].
#' @slot avs Array. Output. Average size of fish caught on each lake.
#' @slot cr Array. Output. Catch rate on each lake.
#' @slot exeff Array. Input. Experimental observed effort.
#' @slot exavs Array. Input. Experimental average size of fish caught.
#' @slot excr Array. Input. Experimental observed catch rate.
#' @slot sigeff 3D array. Effort summed [management mode, simulation, lake].
#' @slot sigexeff 3D array. Observed experimental effort summed [management mode, simulation, lake].
#' @slot cont List. List of polygons that are map contours or other map features to be plotted.
#' @slot topcols Character. A vector of colors matching each item in the list cont.
#' @slot costs 4D Array. Management costs [simulation, lake, attribute, attribute level].
#' @slot Scosts 3D Array. Stocking costs per fish [simulation, lake, stocking type].
#' @slot totcost 3D Array. Total current stocking costs per lake [management, simulation, lake].
#' @slot effval 2D Array. The current value of a unit of effort [simululation, angler class].
#' @slot licval 2D Arrayl. The value of a license sale [simulation, 1].
#' @slot stnam Character. The name of each stocking type [stocking type].
#' @slot aclass Integer. The age class of each stocking type [stocking type].
#' @slot stwt Numeric. The weight (gms) that each stocking type is stocked at [stocking type].
#' @slot stlen Numeric. The lenght (mm) that each stocking type is stocked at [stocking type].
#' @slot popval Array. Population dynamics parameters [sim, population parameter, stocking type].
#' @slot poperr Array. The error structure and variance for each population parameter [population parameter, (type, CV)]
#' @slot errs Data.frame. A set of errors (log normal CVs) for various other attribtues of the model such as GDD and population size
#' @slot Mage 3D array. Mortality at age [simulation, age class, stocking type]
#' @slot CB 2D array. Output. Total Benefit / Cost. Total effort per $ stocking [management mode, simulation]
#' @slot conv 2D array. Output. The total effort convergence information [lake, iteration of algorithm]
#' @slot Econv 2D array. Output. The effort convergence information [lake, iteration of algorithm]
#' @slot lm Hierarchical list. R linear models for the continuous variables [continuous variable][angler class]
#' @slot U Array. A record of utility for the first management mode and simulation [population centre, angler class]
#' @slot FTroutAng Numeric. The fraction of licensed anglers that are trout fishers [population centre]
#' @slot acc Array. Access metric [simulation, lake]
#' @slot acc_a Numeric. The optimized slope parameter converting the access metric into a lake gravity term
#' @slot misc data.frame. Somewhere to store stuff
#' @slot sel Array. Age selectivity of anglers [simulation, age]
#' @slot GModel Character. Growth model used (Lester1, Lester2, New Lester)
#' @slot couch 4D Array. A specified couch effect [managment mode, simulation, population centre, angler class]
#' @slot fac DISUSED
#' @slot fac_a DISUSED
#' @slot DR Numeric. Discard rate. The fraction of fish that are discarded.
#' @slot DD Numeric. Dead Discarding. The fraction of fish that are discarded that die [simulation]
#' @slot BagLim Array. Bag limit. The maximum number of fish that may be retained per day on each lake  [management mode, lake]
#' @slot maxdays Integer. Maximum days per licence if doing calculations with couch effect = TRUE
#'
#' @author T. Carruthers
#' @export
#' @keywords classes
#' @examples
#'
#' myland<-new('Landscape')
#'
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
