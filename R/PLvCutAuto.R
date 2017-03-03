#<PLvCutAuto: automate PL v. Cut per sample>

PLvCutAuto <- function(x){
  
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('PLvCut.R')
  
  PLvCut_results<-matrix(,nrow=length(x),ncol=2)
  PLvCut_results<-data.frame(PLvCut_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Power law v. PL w cutoff per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      PLvCut_results[i,] <- PLvCut( nomiss(x[[i]])[nomiss(x[[i]])>0], plfit(nomiss(x[[i]])[nomiss(x[[i]])>0])$xmin ),
      error=function(e) { print(paste("Sample",i,": Problem with power law v. power law with cutoff")) }
    )
    utils::setTxtProgressBar(pb, i)
    print(paste("Power law v. PL w cutoff:","Sample",i,"completed on",Sys.time() ))
  }
  close(pb)
  
  colnames(PLvCut_results) <- c("PLvCut.rawLR","PLvCut.p")
  return(PLvCut_results)
}
