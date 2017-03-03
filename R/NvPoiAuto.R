#<NvPoiAuto: automate Normal v. Poisson per sample>

NvPoiAuto <- function(x){
  
  x<-data.frame(x)
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('NvPoi.R')
  
  NvPoi_results<-matrix(,nrow=length(x),ncol=3)
  NvPoi_results<-data.frame(NvPoi_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Normal v. Poisson per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      NvPoi_results[i,] <- NvPoi( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with Normal v. Poisson")) }
    )
    print(paste("Normal v. Poisson:","Sample",i,"completed on",Sys.time() ))
    utils::setTxtProgressBar(pb, i)
    #flush.console()                      
  }
  close(pb)
  
  colnames(NvPoi_results) <- c("NvPoi.rawLR","NvPoi.normLR","NvPoi.p")
  return(NvPoi_results)
  
}#End NvPoiAuto function
