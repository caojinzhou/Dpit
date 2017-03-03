WeibvPoi <- function(x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(WeibvPoi) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){ 
    WeibvPoi_results<-matrix('Unavailable for continuous',nrow=1,ncol=3)    
  } #end continuous case
    
  if( fdattype=="integer" ){
    #source('weibull.R') #Called upon in discweib.R
    #source('discweib.R')
    #source('poisson.R')
    discweib.d<-discweib.fit(x,Xmin) #Use discweib.R
    pois.d<-pois.tail.fit(x,Xmin)  #Use poisson.R
    
    discweib.poisson.llr <- function(x,discweib.d,pois.d) {
      xmin <- Xmin
      shape <- discweib.d$shape
      scale <- discweib.d$scale
      mean <- pois.d$rate
      x <- x[x>=xmin]
      suppressWarnings(ddiscweib(x,shape,scale,xmin,log=TRUE)) - dpois.tail(x,threshold=xmin,rate=mean,log=TRUE)
    }
    
    discweib.poisson.llr<-discweib.poisson.llr(x,discweib.d,pois.d)
    
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
    
    WeibvPoi_results<-vuong(discweib.poisson.llr)
    
  } #end discrete case
  
  return(WeibvPoi_results)     
} #end WeibvPoi function