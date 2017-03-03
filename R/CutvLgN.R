CutvLgN <- function (x,Xmin){ 
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(CutvLgN) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){
    #source('pareto.R') #Called upon in powerexp.R
    #source('exp.R') # Also called upon in powerexp.R
    #source('powerexp.R')
    #source('powerexp-exponential-integral.R')
    #source('lnorm.R')
    powerexp.d<-powerexp.fit(x,Xmin,method="constrOptim",initial_rate=-1) #Use powerexp.R
    lnorm.d<-lnorm.fit(x,Xmin,method="tail") #Use lnorm.R
    
    cut.lnorm.llr <- function(x,powerexp.d,lnorm.d) {
      xmin <- Xmin
      alpha <- powerexp.d$exponent
      lambda <- powerexp.d$rate
      m <- lnorm.d$meanlog
      s <- lnorm.d$sdlog
      x <- x[x>=xmin]
      suppressWarnings(dpowerexp(x,threshold=xmin,exponent=alpha,rate=lambda,log=TRUE)) - suppressWarnings(dlnorm(x,meanlog=m,sdlog=s,log=TRUE))
    }
    
    cut.lnorm.llr<-cut.lnorm.llr(x,powerexp.d,lnorm.d)
    
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
    
    CutvLgN_results<-vuong(cut.lnorm.llr)
  } #end continuous case
  
  if( fdattype=="integer" ){
    #source('zeta.R') #Called upon in discpowerexp.R
    #source('discexp.R') # Also called upon in discpowerexp.R
    #source('discpowerexp.R')
    #source('disclnorm.R')
    discpowerexp.d<-discpowerexp.fit(x,Xmin) #Use discpowerexp.R
    disclnorm.d<-fit.lnorm.disc(x,Xmin) #Use disclnorm.R
    
    disccut.lnorm.llr <- function(x,discpowerexp.d,disclnorm.d) {
      xmin <- Xmin
      alpha <- discpowerexp.d$exponent
      lambda <- discpowerexp.d$rate
      meanlog <- disclnorm.d$meanlog
      sdlog <- disclnorm.d$sdlog
      x <- x[x>=xmin]
      suppressWarnings(ddiscpowerexp(x,exponent=alpha,rate=lambda,threshold=xmin,log=TRUE)) - dlnorm.tail.disc(x,meanlog,sdlog,xmin,log=TRUE)
    }
    
    disccut.lnorm.llr<-disccut.lnorm.llr(x,discpowerexp.d,disclnorm.d) 
    
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
  
    CutvLgN_results<-vuong(disccut.lnorm.llr) 
  } #end discrete case
  
  return(CutvLgN_results)     
} #end CutvLgN function