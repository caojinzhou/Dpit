#<WeibvLgNAuto: automate Weibull v. lognormal per sample>

WeibvLgNAuto <- function(x){
  
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('WeibvLgN.R')
  
  WeibvLgN_results<-matrix(,nrow=length(x),ncol=3)
  WeibvLgN_results<-data.frame(WeibvLgN_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Weibull v. lognormal per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      WeibvLgN_results[i,] <- WeibvLgN( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with Weibull v. lognormal")) }
    )
    utils::setTxtProgressBar(pb, i)
    print(paste("Weibull v. lognormal:","Sample",i,"completed on",Sys.time() ))
  }
  close(pb)

  colnames(WeibvLgN_results) <- c("WeibvLgN.rawLR","WeibvLgN.normLR","WeibvLgN.p")
  return(WeibvLgN_results)
}
