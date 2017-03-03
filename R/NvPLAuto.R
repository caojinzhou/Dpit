#<NvPLAuto: automate Normal v. power law per sample>

NvPLAuto <- function(x){
  
  x<-data.frame(x)
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('NvPL.R')
  
  NvPL_results<-matrix(,nrow=length(x),ncol=3)
  NvPL_results<-data.frame(NvPL_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Normal v. power law per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      NvPL_results[i,] <- NvPL( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with Normal v. power law")) }
    )
    print(paste("Normal v. power law:","Sample",i,"completed on",Sys.time() ))
    utils::setTxtProgressBar(pb, i)
    #flush.console()                      
  }
  close(pb)
  
  colnames(NvPL_results) <- c("NvPL.rawLR","NvPL.normLR","NvPL.p")
  return(NvPL_results)
  
}#End NvPLAuto function
