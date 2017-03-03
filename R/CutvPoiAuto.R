#<CutvPoiAuto: automate PL w cutoff v. Poisson per sample>

CutvPoiAuto <- function(x){
  
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('CutvPoi.R')
  
  CutvPoi_results<-matrix(,nrow=length(x),ncol=3)
  CutvPoi_results<-data.frame(CutvPoi_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<PL w cutoff v. Poisson per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      CutvPoi_results[i,] <- CutvPoi( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with PL w cutoff v. Poisson")) }
    )
    utils::setTxtProgressBar(pb, i)
    print(paste("PL w cutoff v. Poisson:","Sample",i,"completed on",Sys.time() ))
    }
  close(pb)
  
  colnames(CutvPoi_results) <- c("CutvPoi.rawLR","CutvPoi.normLR","CutvPoi.p")
  return(CutvPoi_results)
}
