CutvPoi <- function(x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(Cutv.Poi) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){ 
    CutvPoi_results<-matrix('Unavailable for continuous',nrow=1,ncol=3)    
  } #end continuous case
  
  if( fdattype=="integer" ){
    #source('zeta.R') #Called upon in discpowerexp.R
    #source('discexp.R') # Also called upon in discpowerexp.R
    #source('discpowerexp.R')
    #source('poisson.R')
    discpowerexp.d<-discpowerexp.fit(x,Xmin) #Use discpowerexp.R
    pois.d<-pois.tail.fit(x,Xmin)  #Use poisson.R
    
    cut.poisson.llr <- function(x,discpowerexp.d,pois.d) {
      xmin <- Xmin
      alpha <- discpowerexp.d$exponent
      lambda <- discpowerexp.d$rate
      mean <- pois.d$rate
      x <- x[x>=xmin]
      suppressWarnings(ddiscpowerexp(x,exponent=alpha,rate=lambda,threshold=xmin,log=TRUE)) - dpois.tail(x,threshold=xmin,rate=mean,log=TRUE)
    }
    
    cut.poisson.llr<-cut.poisson.llr(x,discpowerexp.d,pois.d)
    
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
    
    CutvPoi_results<-vuong(cut.poisson.llr)
    
  } #end discrete case
  
  return(CutvPoi_results)     
} #end CutvPoi function