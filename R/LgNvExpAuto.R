#<LgNvExpAuto: automate lognormal v. exponential per sample>

LgNvExpAuto <- function(x){
  
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('LgNvExp.R')
  
  LgNvExp_results<-matrix(,nrow=length(x),ncol=3)
  LgNvExp_results<-data.frame(LgNvExp_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Lognormal v. exponential per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      LgNvExp_results[i,] <- LgNvExp( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with lognormal v. exponential")) }
    )
    utils::setTxtProgressBar(pb, i)
    print(paste("Lognormal v. exponential:","Sample",i,"completed on",Sys.time() ))
  }
  close(pb)
  
  colnames(LgNvExp_results) <- c("LgNvExp.rawLR","LgNvExp.normLR","LgNvExp.p")
  return(LgNvExp_results)
}
