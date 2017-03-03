NvPL <- function(x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(NvPL) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){
    #source('pareto.R')
    norm.d<-fitdistrplus::fitdist(x,"norm")
    pareto.d<-pareto.fit(x,Xmin,method="ml") #Use pareto.R
    
    norm.pareto.llr <- function(x,norm.d,pareto.d) {
      xmin <- Xmin
      mean<-norm.d$estimate[1]
      sd<-norm.d$estimate[2]
      alpha <- pareto.d$exponent
      x <- x[x>=xmin]
      #suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(pnorm(xmin,mean=mean,sd=sd,lower.tail=FALSE,log.p=TRUE)) - suppressWarnings(dpareto(x,threshold=xmin,exponent=alpha,log=TRUE)) 
      suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(dpareto(x,threshold=xmin,exponent=alpha,log=TRUE)) 
    }
    
    norm.pareto.llr<-norm.pareto.llr(x,norm.d,pareto.d)
    
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
    
    NvPL_results<-vuong(norm.pareto.llr)
  } #end continuous case
    
  if( fdattype=="integer" ){
    #source('zeta.R')
    norm.d<-fitdistrplus::fitdist(x,"norm")
    zeta.d<-zeta.fit(x,Xmin,method="ml.direct") #Use zeta.R
    
    norm.zeta.llr <- function(x,norm.d,zeta.d) {
      xmin <- Xmin
      mean<-norm.d$estimate[1]
      sd<-norm.d$estimate[2]
      alpha <- zeta.d$exponent
      x <- x[x>=xmin]
      #suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(pnorm(xmin,mean=mean,sd=sd,lower.tail=FALSE,log.p=TRUE)) - suppressWarnings(dzeta(x,xmin,alpha,log=TRUE))
      suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(dzeta(x,xmin,alpha,log=TRUE))
    }
    
    norm.zeta.llr<-norm.zeta.llr(x,norm.d,zeta.d)
    
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
    
    NvPL_results<-vuong(norm.zeta.llr) 
  } #end discrete case 
  
  return(NvPL_results)     
} #end NvPL function
