NvPoi <- function(x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(NvPoi) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){ 
    NvPoi_results<-matrix('Unavailable for continuous',nrow=1,ncol=3)    
  } #end continuous case
  
  if( fdattype=="integer" ){
    #source('poisson.R')
    norm.d<-fitdistrplus::fitdist(x,"norm")
    pois.d<-pois.tail.fit(x,Xmin)  #Use poisson.R
    
    norm.poisson.llr <- function(x,norm.d,pois.d) {
      xmin <- Xmin
      mean<-norm.d$estimate[1]
      sd<-norm.d$estimate[2]
      rate <- pois.d$rate
      x <- x[x>=xmin]
      suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(dpois.tail(x,threshold=xmin,rate=rate,log=TRUE))
      #suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(pnorm(xmin,mean=mean,sd=sd,lower.tail=FALSE,log.p=TRUE)) - dpois.tail(x,threshold=xmin,rate=rate,log=TRUE) + ppois(xmin,lambda=rate,lower.tail=FALSE,log.p=TRUE)
     }
    
    norm.poisson.llr<-norm.poisson.llr(x,norm.d,pois.d)
    
    vuong <- function(x) {
      n <- length(x)
      R <- sum(x)
      m <- mean(x)
      s <- sd(x)
      v <- sqrt(n)*m/s
      p1 <- pnorm(v)
      if (p1 < 0.5) {p2 <- 2*p1} else {p2 <- 2*(1-p1)}
      #list(loglike.ratio=R,mean.LLR = m, sd.LLR = s, Vuong=v, p.one.sided=p1, p.two.sided=p2)
      list(loglike.ratio=R,Vuong=v,p.two.sided=p2) 
    }
    
    NvPoi_results<-vuong(norm.poisson.llr)
    
  } #end discrete case
  
  return(NvPoi_results)     
} #end NvPoi function 
