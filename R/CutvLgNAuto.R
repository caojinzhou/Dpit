#<CutvLgNAuto: automate PL v. LgN per sample>

CutvLgNAuto <- function(x){
  
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('CutvLgN.R')

  CutvLgN_results<-matrix(,nrow=length(x),ncol=3)
  CutvLgN_results<-data.frame(CutvLgN_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<PL w cutoff v. lognormal per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      CutvLgN_results[i,] <- CutvLgN( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with PL w cutoff v. lognormal")) }
    )
    print(paste("PL w cutoff v. lognormal:","Sample",i,"completed on",Sys.time() ))
    utils::setTxtProgressBar(pb, i)
  }
   close(pb)

  
  colnames(CutvLgN_results) <- c("CutvLgN.rawLR","CutvLgN.normLR","CutvLgN.p")
 return(CutvLgN_results)
}
