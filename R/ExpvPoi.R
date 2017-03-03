ExpvPoi <- function (x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(PLvExp) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){ 
    ExpvPoi_results<-matrix('Unavailable for continuous',nrow=1,ncol=3)    
  } #end continuous case
  
  if( fdattype=="integer" ){
    #source('discexp.R')
    #source('poisson.R')
    discexp.d<-discexp.fit(x,Xmin) #Use discexp.R
    pois.d<-pois.tail.fit(x,Xmin)  #Use poisson.R
    
    discexp.poisson.llr <- function(x,discexp.d,pois.d) {
      xmin <- Xmin
      rate <- discexp.d$lambda
      mean <- pois.d$rate
      x <- x[x>=xmin]
      suppressWarnings(ddiscexp(x,rate,xmin,log=TRUE)) - dpois.tail(x,threshold=xmin,rate=mean,log=TRUE)
    }
    
    discexp.poisson.llr<-discexp.poisson.llr(x,discexp.d,pois.d)
    
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
    
    ExpvPoi_results<-vuong(discexp.poisson.llr) 
  } #end discrete case 
  
  return(ExpvPoi_results)     
} #end ExpvPoi function
    