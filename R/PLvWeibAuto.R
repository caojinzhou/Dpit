#<PLvWeibAuto: automate PL v. Weib per sample>

PLvWeibAuto <- function(x){
  
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('PLvWeib.R')
  
  PLvWeib_results<-matrix(,nrow=length(x),ncol=3)
  PLvWeib_results<-data.frame(PLvWeib_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Power law v. Weibull per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      PLvWeib_results[i,] <- PLvWeib( nomiss(x[[i]])[nomiss(x[[i]])>0], plfit(nomiss(x[[i]])[nomiss(x[[i]])>0])$xmin ),
      error=function(e) { print(paste("Sample",i,": Problem with power law v. Weibull")) }
    )
    utils::setTxtProgressBar(pb, i)
    print(paste("Power law v. Weibull:","Sample",i,"completed on",Sys.time() ))
  }
  close(pb)
  
  colnames(PLvWeib_results) <- c("PLvWeib.rawLR","PLvWeib.normLR","PLvWeib.p")
  return(PLvWeib_results)
}
