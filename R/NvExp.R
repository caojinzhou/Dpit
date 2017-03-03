NvExp <- function(x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(NvExp) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){
    #source('exp.R')
    norm.d<-fitdistrplus::fitdist(x,"norm")
    exp.d<-exp.fit(x,Xmin,method="tail") #Use exp.R 
    
    norm.exp.llr <- function(x,norm.d,exp.d) {
      xmin <- Xmin
      mean<-norm.d$estimate[1]
      sd<-norm.d$estimate[2]
      rate <- exp.d$rate
      x <- x[x>=xmin]
      suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(dexp(x,rate,log=TRUE))
      #suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(pnorm(xmin,mean=mean,sd=sd,lower.tail=FALSE,log.p=TRUE)) - suppressWarnings(dexp(x,rate,log=TRUE))
      #suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(dexp(x,rate,log=TRUE))
      #suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(dexp(x,rate,log=TRUE)) + suppressWarnings(pexp(xmin,rate,lower.tail=FALSE,log.p=TRUE))
    }
    
    norm.exp.llr<-norm.exp.llr(x,norm.d,exp.d)
    
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
    
    NvExp_results<-vuong(norm.exp.llr)
  } #end continuous case
  
  if( fdattype=="integer" ){
    #source('discexp.R')
    norm.d<-fitdistrplus::fitdist(x,"norm")
    discexp.d<-discexp.fit(x,Xmin) #Use discexp.R
    
    norm.discexp.llr <- function(x,norm.d,discexp.d) {
      xmin <- Xmin
      mean<-norm.d$estimate[1]
      sd<-norm.d$estimate[2]
      rate <- discexp.d$lambda
      x <- x[x>=xmin]
      #suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(pnorm(xmin,mean=mean,sd=sd,lower.tail=FALSE,log.p=TRUE)) - suppressWarnings(ddiscexp(x,rate,xmin,log=TRUE))
      suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(ddiscexp(x,rate,xmin,log=TRUE))
    }
    
    norm.discexp.llr<-norm.discexp.llr(x,norm.d,discexp.d) 
    
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
    
    NvExp_results<-vuong(norm.discexp.llr) 
  } #end discrete case 
  
  return(NvExp_results)     
} #end NvExp function
    
