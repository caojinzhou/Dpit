#<plfitAuto: Xmin, alpha, & Ntail per sample>

plfitAuto<- function(x){
  
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  
  plfit_results<-matrix(,nrow=length(x),ncol=4)
  plfit_results<-data.frame(plfit_results)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Xmin, alpha, and Ntail per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      plfit_results[i,1:3] <- plfit(nomiss(x[[i]])[nomiss(x[[i]])>0]),
      error=function(e) { print(paste("Sample",i,": Problem with calculating Xmin, alpha, and Ntail")) }
    )
    utils::setTxtProgressBar(pb, i)
    print(paste("Xmin, alpha, and Ntail:","Sample",i,"completed on",Sys.time() ))
  }
  
  NtailSize<- function(x){
    if(x<50) {
      out <- matrix('Ntail<50: Small sample size may bias results',nrow=1,ncol=1) }
    else{ out<- matrix('Ntail>=50',nrow=1,ncol=1) }
  }
  
  for (i in 1:length(x)) {
    tryCatch(
      plfit_results[i,4] <- NtailSize(plfit(nomiss(x[[i]])[nomiss(x[[i]])>0])$Ntail),
      error=function(e) { print(paste("Sample",i,": Problem with Ntail")) }
    )
  }
  close(pb)
  
  colnames(plfit_results) <- c("Xmin","alpha","Ntail","Note.on.Ntail")
  return(plfit_results)
}

#if( plfit_results[i,3]<50 ) {
#  plfit_results[i,4] <- matrix('Ntail<50: Small sample size may bias results',nrow=1,ncol=1) }
#else{ plfit_results[i,4] <- matrix('Ntail>=50',nrow=1,ncol=1) }

#print(paste... from: http://www.win-vector.com/blog/2012/10/error-handling-in-r/
