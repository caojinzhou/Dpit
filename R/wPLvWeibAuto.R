#<wPLvWeibAuto: automate PL v. Weib per sample>

wPLvWeibAuto <- function(x){
  
  x<-data.frame(x)
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('PLvWeib.R')
  
  wPLvWeibAutoResults<-matrix(,nrow=length(x),ncol=3)
  wPLvWeibAutoResults<-data.frame(wPLvWeibAutoResults)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Power law v. Weibull per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      wPLvWeibAutoResults[i,] <- PLvWeib( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with power law v. Weibull")) }
    )
    print(paste("Power law v. Weibull:","Sample",i,"completed on",Sys.time() ))
    utils::setTxtProgressBar(pb, i)
    #flush.console()
  }
  close(pb)
  
  #colnames(wPLvWeibAutoResults) <- c("wPLvWeib.rawLR","wPLvWeib.normLR","wPLvWeib.p")
  colnames(wPLvWeibAutoResults) <- c("PLvWeib.rawLR","PLvWeib.normLR","PLvWeib.p")
  return(wPLvWeibAutoResults)
  
} #End wPLvWeibAuto function
