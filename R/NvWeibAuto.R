#<NvWeibAuto: automate Normal v. Weib per sample>

NvWeibAuto <- function(x){
  
  x<-data.frame(x)
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('NvWeib.R')
  
  NvWeib_results<-matrix(,nrow=length(x),ncol=3)
  NvWeib_results<-data.frame(NvWeib_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Normal v. Weibull per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      NvWeib_results[i,] <- NvWeib( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with Normal v. Weibull")) }
    )
    print(paste("Normal v. Weibull:","Sample",i,"completed on",Sys.time() ))
    utils::setTxtProgressBar(pb, i)
    #flush.console()                      
  }
  close(pb)
  
  colnames(NvWeib_results) <- c("NvWeib.rawLR","NvWeib.normLR","NvWeib.p")
  return(NvWeib_results)
  
}#End NvWeibAuto function
