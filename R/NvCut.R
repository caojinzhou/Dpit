NvCut <- function(x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(NvCut) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){
    #source('pareto.R') #Called upon in powerexp.R
    #source('exp.R') # Also called upon in powerexp.R
    #source('powerexp.R')
    #source('powerexp-exponential-integral.R')
    norm.d<-fitdistrplus::fitdist(x,"norm")
    powerexp.d<-powerexp.fit(x,Xmin,method="constrOptim",initial_rate=-1) #Use powerexp.R
    
    norm.cut.llr <- function(x,norm.d,powerexp.d) {
      xmin <- Xmin
      mean<-norm.d$estimate[1]
      sd<-norm.d$estimate[2]
      alpha <- powerexp.d$exponent
      lambda <- powerexp.d$rate
      x <- x[x>=xmin]
      #suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(pnorm(xmin,mean=mean,sd=sd,lower.tail=FALSE,log.p=TRUE)) - suppressWarnings(dpowerexp(x,threshold=xmin,exponent=alpha,rate=lambda,log=TRUE))
      #suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(dpowerexp(x,threshold=xmin,exponent=alpha,rate=lambda,log=TRUE)) + ppowerexp(x,threshold=xmin,exponent=alpha,rate=lambda,lower.tail=FALSE,log.p=TRUE)
      suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(dpowerexp(x,threshold=xmin,exponent=alpha,rate=lambda,log=TRUE))
    }
    
    norm.cut.llr<-norm.cut.llr(x,norm.d,powerexp.d)
    
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
    
    NvCut_results<-vuong(norm.cut.llr)
  } #end continuous case
  
  if( fdattype=="integer" ){
    #source('zeta.R') #Called upon in discpowerexp.R
    #source('discexp.R') # Also called upon in discpowerexp.R
    #source('discpowerexp.R')
    norm.d<-fitdistrplus::fitdist(x,"norm")
    discpowerexp.d<-discpowerexp.fit(x,Xmin) #Use discpowerexp.R
    
    norm.disccut.llr <- function(x,norm.d,discpowerexp.d) {
      xmin <- Xmin
      mean<-norm.d$estimate[1]
      sd<-norm.d$estimate[2]
      alpha <- discpowerexp.d$exponent
      lambda <- discpowerexp.d$rate
      x <- x[x>=xmin]
      suppressWarnings(dnorm(x,mean=mean,sd=sd,log=TRUE)) - suppressWarnings(ddiscpowerexp(x,exponent=alpha,rate=lambda,threshold=xmin,log=TRUE))
    }
    
    norm.disccut.llr <- norm.disccut.llr(x,norm.d,discpowerexp.d)
    
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
    
    NvCut_results<-vuong(norm.disccut.llr) 
  } #end discrete case 
  
  return(NvCut_results)     
} #end NvCut function
