CutvExp <- function (x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(CutvExp) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){
    #source('pareto.R')
    #source('exp.R')
    #source('powerexp.R')
    #source('powerexp-exponential-integral.R')
    powerexp.d<-powerexp.fit(x,Xmin,method="constrOptim",initial_rate=-1) #Use powerexp.R
    exp.d<-exp.fit(x,Xmin,method="tail") #Use exp.R
    
    cut.exp.llr <- function(x,powerexp.d,exp.d) { #Adapted from power-law-test.R
      xmin <- Xmin
      alpha <- powerexp.d$exponent
      lambda <- powerexp.d$rate
      rate <- exp.d$rate
      x <- x[x>=xmin]
      suppressWarnings(dpowerexp(x,threshold=xmin,exponent=alpha,rate=lambda,log=TRUE)) - suppressWarnings(dexp(x,rate,log=TRUE))
    }
    
    cut.exp.llr<-cut.exp.llr(x,powerexp.d,exp.d) 
    
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
    
    CutvExp_results<-vuong(cut.exp.llr) #Use power-law-test.R    
  } #end continuous case
  
  if( fdattype=="integer" ){
    #source('zeta.R') #Called upon in discpowerexp.R
    #source('discexp.R') # Also called upon in discpowerexp.R
    #source('discpowerexp.R')
    discpowerexp.d<-discpowerexp.fit(x,Xmin) #Use discpowerexp.R
    discexp.d<-discexp.fit(x,Xmin) #Use discexp.R
    
    disccut.exp.llr <- function(x,discpowerexp.d,discexp.d) { #Adapted from power-law-test.R
      xmin <- Xmin
      alpha <- discpowerexp.d$exponent
      lambda <- discpowerexp.d$rate
      rate <- discexp.d$lambda
      x <- x[x>=xmin]
      suppressWarnings(ddiscpowerexp(x,exponent=alpha,rate=lambda,threshold=xmin,log=TRUE)) - suppressWarnings(ddiscexp(x,rate,xmin,log=TRUE))
    }
    
    disccut.exp.llr<-disccut.exp.llr(x,discpowerexp.d,discexp.d) #Use power-law-test.R
    
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
    
    CutvExp_results<-vuong(disccut.exp.llr) #Use power-law-test.R
  } #end discrete case
  
  return(CutvExp_results)     
} #end CutvExp function