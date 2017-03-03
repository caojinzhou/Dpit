PLvPoi <- function(x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(PLv.Poi) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){ 
    PLvPoi_results<-matrix('Unavailable for continuous',nrow=1,ncol=3)    
  } #end continuous case

  if( fdattype=="integer" ){
    #source('zeta.R')
    #source('poisson.R')
    #source('power-law-test.R')
    zeta.d<-zeta.fit(x,Xmin,method="ml.direct") #Use zeta.R
    pois.d<-pois.tail.fit(x,Xmin)  #Use poisson.R
    zeta.poisson.llr<-zeta.poisson.llr(x,zeta.d,pois.d) #Use power-law-test.R
    PLvPoi_results<-vuong(zeta.poisson.llr) #Use power-law-test.R
  } #end discrete case

  return(PLvPoi_results)     
} #end PLvPoi function