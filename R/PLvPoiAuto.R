#<PLvPoiAuto: automate PL v. Poisson per sample>

PLvPoiAuto <- function(x){
  
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('PLvPoi.R')
  
  PLvPoi_results<-matrix(,nrow=length(x),ncol=3)
  PLvPoi_results<-data.frame(PLvPoi_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Power law v. Poisson per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      PLvPoi_results[i,] <- PLvPoi( nomiss(x[[i]])[nomiss(x[[i]])>0], plfit(nomiss(x[[i]])[nomiss(x[[i]])>0])$xmin ),
      error=function(e) { print(paste("Sample",i,": Problem with power law v. Poisson")) }
    )
    utils::setTxtProgressBar(pb, i)
    print(paste("Power law v. Poisson:","Sample",i,"completed on",Sys.time() ))
  }
  close(pb)
  
  colnames(PLvPoi_results) <- c("PLvPoi.rawLR","PLvPoi.normLR","PLvPoi.p")
  return(PLvPoi_results)
}
