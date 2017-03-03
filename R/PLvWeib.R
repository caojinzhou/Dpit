PLvWeib <- function (x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(PLvWeib) Error: x must contain only reals or only integers.") } 
  
  if( fdattype=="real" ){
    #source('pareto.R')
    #source('weibull.R')
    #source('power-law-test.R')
    pareto.d<-pareto.fit(x,Xmin,method="ml") #Use pareto.R
    weibull.d<-weibull.fit(x,Xmin,method="tail") #Use weibull.R
    pareto.weibull.llr<-pareto.weibull.llr(x,pareto.d,weibull.d) #Use power-law-test.R
    PLvWeib_results<-vuong(pareto.weibull.llr) #Use power-law-test.R   
  } #end continuous case
  
  if( fdattype=="integer" ){
    #source('zeta.R')
    #source('weibull.R')
    #source('discweib.R')
    #source('power-law-test.R')
    zeta.d<-zeta.fit(x,Xmin,method="ml.direct") #Use zeta.R
    weib.d<-discweib.fit(x,Xmin) #Use discweib.R
    zeta.weib.llr<-zeta.weib.llr(x,zeta.d,weib.d) #Use power-law-test.R
    PLvWeib_results<-vuong(zeta.weib.llr) #Use power-law-test.R   
  } #end discrete case
  
  return(PLvWeib_results)     
} #end PLvWeib function