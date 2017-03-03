#<LgNvPoiAuto: automate lognormal v. Poisson per sample>

LgNvPoiAuto <- function(x){
  
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('LgNvPoi.R')
  
  LgNvPoi_results<-matrix(,nrow=length(x),ncol=3)
  LgNvPoi_results<-data.frame(LgNvPoi_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Lognormal v. Poisson per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      LgNvPoi_results[i,] <- LgNvPoi( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with lognormal v. Poisson")) }
    )
    utils::setTxtProgressBar(pb, i)
    print(paste("Lognormal v. Poisson:","Sample",i,"completed on",Sys.time() ))
  }
  close(pb)
  
  colnames(LgNvPoi_results) <- c("LgNvPoi.rawLR","LgNvPoi.normLR","LgNvPoi.p")
  return(LgNvPoi_results)
}
