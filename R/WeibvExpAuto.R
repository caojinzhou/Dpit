#<WeibvExpAuto: automate Weibull v. Exponential per sample>

WeibvExpAuto <- function(x){
  
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('WeibvExp.R')
  
  WeibvExp_results<-matrix(,nrow=length(x),ncol=3)
  WeibvExp_results<-data.frame(WeibvExp_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Weibull v. exponential per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      WeibvExp_results[i,] <- WeibvExp( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with Weibull v. exponential")) }
    )
    utils::setTxtProgressBar(pb, i)
    print(paste("Weibull v. exponential:","Sample",i,"completed on",Sys.time() ))
  }
  close(pb)
  
  colnames(WeibvExp_results) <- c("WeibvExp.rawLR","WeibvExp.normLR","WeibvExp.p")
  return(WeibvExp_results)
}
