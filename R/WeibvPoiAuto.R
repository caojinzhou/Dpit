#<WeibvPoiAuto: automate Weibull v. Poisson per sample>

WeibvPoiAuto <- function(x){
  
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('WeibvPoi.R')
  
  WeibvPoi_results<-matrix(,nrow=length(x),ncol=3)
  WeibvPoi_results<-data.frame(WeibvPoi_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Weibull v. Poisson per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      WeibvPoi_results[i,] <- WeibvPoi( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with Weibull v. Poisson")) }
    )
    utils::setTxtProgressBar(pb, i)
    print(paste("Weibull v. Poisson:","Sample",i,"completed on",Sys.time() ))
  }
  close(pb)
  
  colnames(WeibvPoi_results) <- c("WeibvPoi.rawLR","WeibvPoi.normLR","WeibvPoi.p")
  return(WeibvPoi_results)
}
