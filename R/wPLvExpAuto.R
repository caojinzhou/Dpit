#<wPLvExpAuto: automate PL v. Exp per sample>

wPLvExpAuto <- function(x){
  
  x<-data.frame(x)
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('PLvExp.R')
  
  wPLvExpAutoResults<-matrix(,nrow=length(x),ncol=3)
  wPLvExpAutoResults<-data.frame(wPLvExpAutoResults)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Power law v. exponential per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      wPLvExpAutoResults[i,] <- PLvExp( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with power law v. exponential")) }
    )
    print(paste("Power law v. exponential:","Sample",i,"completed on",Sys.time() ))
    utils::setTxtProgressBar(pb, i)
    #flush.console() 
  }
  close(pb)
  
  #colnames(wPLvExpAutoResults) <- c("wPLvExp.rawLR","wPLvExp.normLR","wPLvExp.p")
  colnames(wPLvExpAutoResults) <- c("PLvExp.rawLR","PLvExp.normLR","PLvExp.p")
  return(wPLvExpAutoResults)
  
} #End wPLvExpAuto function
