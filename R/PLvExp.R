PLvExp <- function (x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(PLvExp) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){
    #source('pareto.R')
    #source('exp.R')
    #source('power-law-test.R')
    pareto.d<-pareto.fit(x,Xmin,method="ml") #Use pareto.R
    exp.d<-exp.fit(x,Xmin,method="tail") #Use exp.R 
    pareto.exp.llr<-pareto.exp.llr(x,pareto.d,exp.d) #Use power-law-test.R
    PLvExp_results<-vuong(pareto.exp.llr) #Use power-law-test.R    
  } #end continuous case
  
  if( fdattype=="integer" ){
    #source('zeta.R')
    #source('discexp.R')
    #source('power-law-test.R')
    zeta.d<-zeta.fit(x,Xmin,method="ml.direct") #Use zeta.R
    exp.d<-discexp.fit(x,Xmin) #Use discexp.R
    zeta.exp.llr<-zeta.exp.llr(x,zeta.d,exp.d) #Use power-law-test.R
    PLvExp_results<-vuong(zeta.exp.llr) #Use power-law-test.R
  } #end discrete case
  
  return(PLvExp_results)     
} #end PLvExp function