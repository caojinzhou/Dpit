NvLgN <- function(x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(NvLgN) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){
    #source('lnorm.R')
    norm.d<-fitdistrplus::fitdist(x,"norm")
    lnorm.d<-lnorm.fit(x,Xmin,method="tail") #Use lnorm.R
    
    norm.lnorm.llr <- function(x,norm.d,lnorm.d) {
      xmin <- Xmin
      mean<-norm.d$estimate[1]
      sd<-norm.d$estimate[2]
      m <- lnorm.d$meanlog
      s <- lnorm.d$sdlog
      x <- x[x>=xmin]
      suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(dlnorm(x,meanlog=m,sdlog=s,log=TRUE))
    }
    
    norm.lnorm.llr<-norm.lnorm.llr(x,norm.d,lnorm.d) 
    
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
    
    NvLgN_results<-vuong(norm.lnorm.llr) #Use power-law-test.R
  } #end continuous case
    
  if( fdattype=="integer" ){
    #source('disclnorm.R')
    norm.d<-fitdistrplus::fitdist(x,"norm")
    disclnorm.d<-fit.lnorm.disc(x,Xmin) #Use disclnorm.R
    
    norm.disclnorm.llr <- function(x,norm.d,disclnorm.d) {
      xmin <- Xmin
      mean<-norm.d$estimate[1]
      sd<-norm.d$estimate[2]
      meanlog <- disclnorm.d$meanlog
      sdlog <- disclnorm.d$sdlog
      x <- x[x>=xmin]
      suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - dlnorm.tail.disc(x,meanlog,sdlog,xmin,log=TRUE)
    }
    
    norm.disclnorm.llr<-norm.disclnorm.llr(x,norm.d,disclnorm.d) 
    
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
    
    NvLgN_results<-vuong(norm.disclnorm.llr) 
  } #end discrete case 
  
  return(NvLgN_results)     
} #end NvLgN function
