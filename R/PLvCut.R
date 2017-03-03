PLvCut <- function (x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(PLvCut) Error: x must contain only reals or only integers.") }  
  
  if( fdattype=="real" ){
    #source('pareto.R')
    #source('powerexp-exponential-integral.R')
    #source('powerexp.R')
    #source('exp.R')
    #source('power-law-test.R')
    pareto.d<-pareto.fit(x,Xmin,method="ml") #Use pareto.R
    powerexp.d<-powerexp.fit(x,Xmin,method="constrOptim",initial_rate=-1) #Use powerexp.R
    PLvCut_results<-power.powerexp.lrt(pareto.d,powerexp.d) #Use power-law-test.R 
  } #end continuous case
  
  if( fdattype=="integer" ){
    #source('zeta.R')
    #source('discpowerexp.R')
    #source('discexp.R')
    #source('power-law-test.R')
    zeta.d<-zeta.fit(x,Xmin,method="ml.direct") #Use zeta.R
    discpowerexp.d<-discpowerexp.fit(x,Xmin) #Use discpowerexp.R
    PLvCut_results<-power.powerexp.lrt(zeta.d,discpowerexp.d) #Use power-law-test.R 
  } #end discrete case
  
  return(PLvCut_results)    
} #end PLvCut function