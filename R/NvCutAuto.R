#<NvCutAuto: automate Normal v. PL w cutoff per sample>

NvCutAuto <- function(x){
  
  x<-data.frame(x)
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('NvCut.R')
  
  NvCut_results<-matrix(,nrow=length(x),ncol=3)
  NvCut_results<-data.frame(NvCut_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Normal v. PL w cutoff per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      NvCut_results[i,] <- NvCut( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with Normal v. PL w cutoff")) }
    )
    utils::setTxtProgressBar(pb, i)
    print(paste("Normal v. PL w cutoff:","Sample",i,"completed on",Sys.time() ))
  }
  close(pb)
  
  colnames(NvCut_results) <- c("NvCut.rawLR","NvCut.normLR","NvCut.p")
  return(NvCut_results)
}
