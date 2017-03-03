#<wPLvLgNAuto: automate PL v. LgN per sample>

wPLvLgNAuto <- function(x){
  
  x<-data.frame(x)
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('PLvLgN.R')
  
  wPLvLgNAutoResults<-matrix(,nrow=length(x),ncol=3)
  wPLvLgNAutoResults<-data.frame(wPLvLgNAutoResults)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Power law v. lognormal per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      wPLvLgNAutoResults[i,] <- PLvLgN( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with power law v. lognormal")) }
    )
    print(paste("Power law v. lognormal:","Sample",i,"completed on",Sys.time() ))
    utils::setTxtProgressBar(pb, i)
    #flush.console() 
  }
  close(pb)
  
  #colnames(wPLvLgNAutoResults) <- c("wPLvLgN.rawLR","wPLvLgN.normLR","wPLvLgN.p")
  colnames(wPLvLgNAutoResults) <- c("PLvLgN.rawLR","PLvLgN.normLR","PLvLgN.p")
  return(wPLvLgNAutoResults)
  
} #End wPLvLgNAuto function
