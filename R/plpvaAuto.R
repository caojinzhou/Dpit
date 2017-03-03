#<plpvaAuto: K-S and its p-value per sample>

plpvaAuto <- function(x){
  
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('plpva.r')
  
  plpva_results<-matrix(,nrow=length(x),ncol=2)
  plpva_results<-data.frame(plpva_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<K-S and its p-value per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      plpva_results[i,] <- plpva(nomiss(x[[i]])[nomiss(x[[i]])>0], plfit(nomiss(x[[i]])[nomiss(x[[i]])>0])$xmin),
      error=function(e) { print(paste("Sample",i,": Problem with calculating K-S and its p-value")) }
    )
    utils::setTxtProgressBar(pb, i)
    print(paste("K-S and its p-value:","Sample",i,"completed on",Sys.time() ))
  }
  close(pb)
  
  colnames(plpva_results) <- c("K-S","p-value (for K-S)")
  return(plpva_results)
}
