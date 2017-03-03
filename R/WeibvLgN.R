WeibvLgN <- function(x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(CutvWeib) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){
    #source('weibull.R')
    #source('lnorm.R')
    weibull.d<-weibull.fit(x,Xmin,method="tail") #Use weibull.R
    lnorm.d<-lnorm.fit(x,Xmin,method="tail") #Use lnorm.R
    
    weib.lnorm.llr <- function(x,weibull.d,lnorm.d) {
      xmin <- Xmin
      shape <- weibull.d$shape
      scale <- weibull.d$scale
      m <- lnorm.d$meanlog
      s <- lnorm.d$sdlog
      x <- x[x>=xmin]
      #( suppressWarnings(dweibull(x,shape=shape,scale=scale,log=TRUE)) - suppressWarnings(pweibull(xmin,shape=shape,scale=scale,lower.tail=FALSE,log.p=TRUE)) ) + ( -suppressWarnings(dlnorm(x,meanlog=m,sdlog=s,log=TRUE)) + suppressWarnings(plnorm(xmin,meanlog=m,sdlog=s,lower.tail=FALSE,log.p=TRUE)) )
      suppressWarnings(dweibull(x,shape=shape,scale=scale,log=TRUE)) - suppressWarnings(dlnorm(x,meanlog=m,sdlog=s,log=TRUE))
    }
    
    weib.lnorm.llr<-weib.lnorm.llr(x,weibull.d,lnorm.d)
    
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
    
    WeibvLgN_results<-vuong(weib.lnorm.llr)
  } #end continuous case
  
  if( fdattype=="integer" ){
    #source('weibull.R') #Called upon in discweib.R
    #source('discweib.R')
    #source('disclnorm.R')
    discweib.d<-discweib.fit(x,Xmin) #Use discweib.R
    disclnorm.d<-fit.lnorm.disc(x,Xmin) #Use disclnorm.R
    
    discweib.lnorm.llr <- function(x,discweib.d,disclnorm.d) {
      xmin <- Xmin
      shape <- discweib.d$shape
      scale <- discweib.d$scale
      meanlog <- disclnorm.d$meanlog
      sdlog <- disclnorm.d$sdlog
      x <- x[x>=xmin]
      suppressWarnings(ddiscweib(x,shape,scale,xmin,log=TRUE)) - dlnorm.tail.disc(x,meanlog,sdlog,xmin,log=TRUE)
    }
    
    discweib.lnorm.llr<-discweib.lnorm.llr(x,discweib.d,disclnorm.d) 
    
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
    
    WeibvLgN_results<-vuong(discweib.lnorm.llr) 
  } #end discrete case 
  
  return(WeibvLgN_results)     
} #end WeibvLgN function