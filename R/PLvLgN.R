PLvLgN <- function (x,Xmin){ 
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(PLvLgN) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){
    #source('pareto.R')
    #source('lnorm.R')
    #source('power-law-test.R')
    pareto.d<-pareto.fit(x,Xmin,method="ml") #Use pareto.R
    lnorm.d<-lnorm.fit(x,Xmin,method="tail") #Use lnorm.R
    pareto.lnorm.llr<-pareto.lnorm.llr(x,pareto.d,lnorm.d) #Use power-law-test.R
    PLvLgN_results<-vuong(pareto.lnorm.llr) #Use power-law-test.R
  } #end continuous case
  
  if( fdattype=="integer" ){
    #source('zeta.R')
    #source('disclnorm.R')
    #source('power-law-test.R')
    zeta.d<-zeta.fit(x,Xmin,method="ml.direct") #Use zeta.R
    lnorm.d<-fit.lnorm.disc(x,Xmin) #Use disclnorm.R
    zeta.lnorm.llr<-zeta.lnorm.llr(x,zeta.d,lnorm.d) #Use power-law-test.R
    PLvLgN_results<-vuong(zeta.lnorm.llr) #Use power-law-test.R  
  } #end discrete case
 
  return(PLvLgN_results)     
} #end PLvLgN function