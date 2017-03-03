WeibvExp <- function(x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(WeibvExp) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){
    #source('weibull.R')
    #source('exp.R')
    weibull.d<-weibull.fit(x,Xmin,method="tail") #Use weibull.R
    exp.d<-exp.fit(x,Xmin,method="tail") #Use exp.R 
    
    weib.exp.llr <- function(x,weibull.d,exp.d) {
      xmin <- Xmin
      shape <- weibull.d$shape
      scale <- weibull.d$scale
      rate <- exp.d$rate
      x <- x[x>=xmin]
      suppressWarnings(dweibull(x,shape=shape,scale=scale,log=TRUE)) - suppressWarnings(dexp(x,rate,log=TRUE))
    }
    
    weib.exp.llr<-weib.exp.llr(x,weibull.d,exp.d)
    
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
    
    WeibvExp_results<-vuong(weib.exp.llr)
  } #end continuous case
  
  if( fdattype=="integer" ){
    #source('weibull.R') #Called upon in discweib.R
    #source('discweib.R')
    #source('discexp.R')
    discweib.d<-discweib.fit(x,Xmin) #Use discweib.R
    discexp.d<-discexp.fit(x,Xmin) #Use discexp.R
    
    discweib.exp.llr <- function(x,discweib.d,discexp.d) {
      xmin <- Xmin
      shape <- discweib.d$shape
      scale <- discweib.d$scale
      rate <- discexp.d$lambda
      x <- x[x>=xmin]
      suppressWarnings(ddiscweib(x,shape,scale,xmin,log=TRUE)) - suppressWarnings(ddiscexp(x,rate,xmin,log=TRUE))
    }
    
    discweib.exp.llr<-discweib.exp.llr(x,discweib.d,discexp.d) 
    
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
    
    WeibvExp_results<-vuong(discweib.exp.llr) 
  } #end discrete case 
  
  return(WeibvExp_results)     
} #end WeibvExp function
    