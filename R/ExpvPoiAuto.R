#<ExpvPoiAuto: automate exponential v. Poisson per sample>

ExpvPoiAuto <- function(x){
  
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('ExpvPoi.R')
  
  ExpvPoi_results<-matrix(,nrow=length(x),ncol=3)
  ExpvPoi_results<-data.frame(ExpvPoi_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Exponential v. Poisson per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      ExpvPoi_results[i,] <- ExpvPoi( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with exponential v. Poisson")) }
    )
    utils::setTxtProgressBar(pb, i)
    print(paste("Exponential v. Poisson:","Sample",i,"completed on",Sys.time() ))
  }
  close(pb)
  
  colnames(ExpvPoi_results) <- c("ExpvPoi.rawLR","ExpvPoi.normLR","ExpvPoi.p")
  return(ExpvPoi_results)
}
