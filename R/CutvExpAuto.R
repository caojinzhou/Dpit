#<CutvExpAuto: automate PL w. cutoff v. Exponential per sample>

CutvExpAuto <- function(x){
  
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('CutvExp.R')
  
  CutvExp_results<-matrix(,nrow=length(x),ncol=3)
  CutvExp_results<-data.frame(CutvExp_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<PL w cutoff v. exponential per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      CutvExp_results[i,] <- CutvExp( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with PL w cutoff v. exponential")) }
    )
    print(paste("PL w cutoff v. exponential:","Sample",i,"completed on",Sys.time() ))
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)
  
  colnames(CutvExp_results) <- c("CutvExp.rawLR","CutvExp.normLR","CutvExp.p")
  return(CutvExp_results)
}
