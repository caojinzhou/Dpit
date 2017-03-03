CutvWeib <- function(x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(CutvWeib) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){
    #source('pareto.R') #Called upon in powerexp.R
    #source('exp.R') # Also called upon in powerexp.R
    #source('powerexp.R')
    #source('powerexp-exponential-integral.R')
    #source('weibull.R')
    powerexp.d<-powerexp.fit(x,Xmin,method="constrOptim",initial_rate=-1) #Use powerexp.R
    weibull.d<-weibull.fit(x,Xmin,method="tail") #Use weibull.R
    
    cut.weibull.llr <- function(x,powerexp.d,weibull.d) {
      xmin <- Xmin
      alpha <- powerexp.d$exponent
      lambda <- powerexp.d$rate
      shape <- weibull.d$shape
      scale <- weibull.d$scale
      x <- x[x>=xmin]
      #suppressWarnings(dpowerexp(x,threshold=xmin,exponent=alpha,rate=lambda,log=TRUE)) + suppressWarnings(ppowerexp(x,threshold=xmin,exponent=alpha,rate=lambda,lower.tail=FALSE,log.p=TRUE)) - suppressWarnings(dweibull(x,shape=shape,scale=scale,log=TRUE)) + suppressWarnings(pweibull(xmin,shape=shape,scale=scale,lower.tail=FALSE,log.p=TRUE))
      suppressWarnings(dpowerexp(x,threshold=xmin,exponent=alpha,rate=lambda,log=TRUE)) - suppressWarnings(dweibull(x,shape=shape,scale=scale,log=TRUE))
    }
      
  cut.weibull.llr<-cut.weibull.llr(x,powerexp.d,weibull.d) #Use power-law-test.R
  
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
    CutvWeib_results<-vuong(cut.weibull.llr) #  
  } #end continuous case
  
  if( fdattype=="integer" ){
    #source('zeta.R') #Called upon in discpowerexp.R
    #source('discexp.R') # Also called upon in discpowerexp.R
    #source('discpowerexp.R')
    #source('weibull.R') #Called upon in discweib.R
    #source('discweib.R')
    discpowerexp.d<-discpowerexp.fit(x,Xmin) #Use discpowerexp.R
    discweib.d<-discweib.fit(x,Xmin) #Use discweib.R
    
    disccut.weib.llr <- function(x,discpowerexp.d,discweib.d) {
      xmin <- Xmin
      alpha <- discpowerexp.d$exponent
      lambda <- discpowerexp.d$rate
      shape <- discweib.d$shape
      scale <- discweib.d$scale
      x <- x[x>=xmin]
      suppressWarnings(ddiscpowerexp(x,exponent=alpha,rate=lambda,threshold=xmin,log=TRUE)) - suppressWarnings(ddiscweib(x,shape,scale,xmin,log=TRUE))
    }
    
    disccut.weib.llr<-disccut.weib.llr(x,discpowerexp.d,discweib.d) #Use power-law-test.R
    
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
    
    CutvWeib_results<-vuong(disccut.weib.llr) #Use power-law-test.R   
  } #end discrete case
  
  return(CutvWeib_results)     
} #end CutvWeib function