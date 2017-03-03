LgNvExp <- function (x,Xmin){
  
  fdattype<-"unknow"  #First, select method (discrete or continuous) for fitting and test if x is a vector
  if( is.vector(x,"numeric") ){ fdattype<-"real" }
  if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
  if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
  if( fdattype=="unknow" ){ stop("(PLvExp) Error: x must contain only reals or only integers.") }
  
  if( fdattype=="real" ){
    #source('lnorm.R')
    #source('exp.R')
    lnorm.d<-lnorm.fit(x,Xmin,method="tail") #Use lnorm.R
    exp.d<-exp.fit(x,Xmin,method="tail") #Use exp.R 
    
    lnorm.exp.llr <- function(x,lnorm.d,exp.d) {
      xmin <- Xmin
      m <- lnorm.d$meanlog
      s <- lnorm.d$sdlog
      rate <- exp.d$rate
      x <- x[x>=xmin]
      suppressWarnings(dlnorm(x,meanlog=m,sdlog=s,log=TRUE)) - suppressWarnings(dexp(x,rate,log=TRUE))
    }
    
    lnorm.exp.llr<-lnorm.exp.llr(x,lnorm.d,exp.d)
    
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
    
    LgNvExp_results<-vuong(lnorm.exp.llr)
  } #end continuous case
  
  if( fdattype=="integer" ){
    #source('disclnorm.R')
    #source('discexp.R')
    disclnorm.d<-fit.lnorm.disc(x,Xmin) #Use disclnorm.R
    discexp.d<-discexp.fit(x,Xmin) #Use discexp.R
    
    disclnorm.exp.llr <- function(x,disclnorm.d,discexp.d) {
      xmin <- Xmin
      meanlog <- disclnorm.d$meanlog
      sdlog <- disclnorm.d$sdlog
      rate <- discexp.d$lambda
      x <- x[x>=xmin]
      dlnorm.tail.disc(x,meanlog,sdlog,xmin,log=TRUE) - suppressWarnings(ddiscexp(x,rate,xmin,log=TRUE))
    }
    
    disclnorm.exp.llr<-disclnorm.exp.llr(x,disclnorm.d,discexp.d)
    
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
    
    LgNvExp_results<-vuong(disclnorm.exp.llr) 
  } #end discrete case 
  
  return(LgNvExp_results)     
} #end LgNvExp function