#<PLvLgNAuto: automate PL v. LgN per sample>

PLvLgNAuto <- function(x){
  
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('PLvLgN.R')

  PLvLgN_results<-matrix(,nrow=length(x),ncol=3)
  PLvLgN_results<-data.frame(PLvLgN_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Power law v. lognormal per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      PLvLgN_results[i,] <- PLvLgN( nomiss(x[[i]])[nomiss(x[[i]])>0], plfit(nomiss(x[[i]])[nomiss(x[[i]])>0])$xmin ),
      error=function(e) { print(paste("Sample",i,": Problem with power law v. lognormal")) }
    )
    utils::setTxtProgressBar(pb, i)
    print(paste("Power law v. lognormal:","Sample",i,"completed on",Sys.time() ))
  }
  close(pb)
  
  colnames(PLvLgN_results) <- c("PLvLgN.rawLR","PLvLgN.normLR","PLvLgN.p")
 return(PLvLgN_results)
}
