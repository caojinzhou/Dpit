#<PLvExpAuto: automate PL v. Exp per sample>

PLvExpAuto <- function(x){
  
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('PLvExp.R')
  
  PLvExp_results<-matrix(,nrow=length(x),ncol=3)
  PLvExp_results<-data.frame(PLvExp_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Power law v. exponential per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      PLvExp_results[i,] <- PLvExp( nomiss(x[[i]])[nomiss(x[[i]])>0], plfit(nomiss(x[[i]])[nomiss(x[[i]])>0])$xmin ),
      error=function(e) { print(paste("Sample",i,": Problem with power law v. exponential")) }
    )
    utils::setTxtProgressBar(pb, i)
    print(paste("Power law v. exponential:","Sample",i,"completed on",Sys.time() ))
  }
  close(pb)
  
  colnames(PLvExp_results) <- c("PLvExp.rawLR","PLvExp.normLR","PLvExp.p")
  return(PLvExp_results)
}
