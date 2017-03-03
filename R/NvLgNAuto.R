#<NvLgNAuto: automate Normal v. LgN per sample>

NvLgNAuto <- function(x){
  
  x<-data.frame(x)
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('NvLgN.R')
  
  NvLgN_results<-matrix(,nrow=length(x),ncol=3)
  NvLgN_results<-data.frame(NvLgN_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Normal v. lognormal per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      NvLgN_results[i,] <- NvLgN( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with Normal v. lognormal")) }
    )
    print(paste("Normal v. lognormal:","Sample",i,"completed on",Sys.time() ))
    utils::setTxtProgressBar(pb, i)
    #flush.console()                      
  }
  close(pb)
  
  colnames(NvLgN_results) <- c("NvLgN.rawLR","NvLgN.normLR","NvLgN.p")
  return(NvLgN_results)
  
}#End NvLgNAuto function
