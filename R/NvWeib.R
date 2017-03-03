NvWeib <- function(x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(NvWeib) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){
    #source('weibull.R')
    norm.d<-fitdistrplus::fitdist(x,"norm")
    weibull.d<-weibull.fit(x,Xmin,method="tail") #Use weibull.R
    
    norm.weibull.llr <- function(x,norm.d,weibull.d) {
      xmin <- Xmin
      mean<-norm.d$estimate[1]
      sd<-norm.d$estimate[2]
      shape <- weibull.d$shape
      scale <- weibull.d$scale 
      x<-x[x>=xmin]
      suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(dweibull(x,shape=shape,scale=scale,log=TRUE))
    }
    
    norm.weibull.llr<-norm.weibull.llr(x,norm.d,weibull.d) 
    
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
    
    NvWeib_results<-vuong(norm.weibull.llr) #Use power-law-test.R
  } #end continuous case
  
  if( fdattype=="integer" ){
    #source('weibull.R') #Called upon in discweib.R
    #source('discweib.R')
    norm.d<-fitdistrplus::fitdist(x,"norm")
    discweib.d<-discweib.fit(x,Xmin) #Use discweib.R
    
    norm.discweib.llr <- function(x,norm.d,discweib.d) {
      xmin <- Xmin
      mean<-norm.d$estimate[1]
      sd<-norm.d$estimate[2]
      shape <- discweib.d$shape
      scale <- discweib.d$scale
      x<-x[x>=xmin]
      suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(ddiscweib(x,shape,scale,xmin,log=TRUE))
    }
    
    norm.discweib.llr<-norm.discweib.llr(x,norm.d,discweib.d)
    
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
    
    NvWeib_results<-vuong(norm.discweib.llr) #Use power-law-test.R   
  } #end discrete case 
  
  return(NvWeib_results)     
} #end NvWeib function
