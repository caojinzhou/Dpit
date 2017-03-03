#<NvExpAuto: automate Normal v. exponential per sample>

NvExpAuto <- function(x){
  
  x<-data.frame(x)
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('NvExp.R')
  
  NvExp_results<-matrix(,nrow=length(x),ncol=3)
  NvExp_results<-data.frame(NvExp_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Normal v. exponential per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      NvExp_results[i,] <- NvExp( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with Normal v. exponential")) }
    )
    print(paste("Normal v. exponential:","Sample",i,"completed on",Sys.time() ))
    utils::setTxtProgressBar(pb, i)
    #flush.console()                      
  }
  close(pb)
  
  colnames(NvExp_results) <- c("NvExp.rawLR","NvExp.normLR","NvExp.p")
  return(NvExp_results)
  
}#End NvExpAuto function
