#<CutvWeibAuto: automate Cut v. Weib per sample>

CutvWeibAuto <- function(x){
  
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('CutvWeib.R')
  
  CutvWeib_results<-matrix(,nrow=length(x),ncol=3)
  CutvWeib_results<-data.frame(CutvWeib_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<PL w cutoff v. Weibull per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      CutvWeib_results[i,] <- CutvWeib( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with PL w cutoff v. Weibull")) }
    )
    print(paste("PL w cutoff v. Weibull:","Sample",i,"completed on",Sys.time() ))
    utils::setTxtProgressBar(pb, i)
    #flush.console()                      
  }
  close(pb)
  
  colnames(CutvWeib_results) <- c("CutvWeib.rawLR","CutvWeib.normLR","CutvWeib.p")
  return(CutvWeib_results)

}#End CutvWeibAuto function
