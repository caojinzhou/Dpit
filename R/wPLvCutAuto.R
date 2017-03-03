#<wPLvCutAuto: automate PL v. Cut per sample>

wPLvCutAuto <- function(x){
  
  x<-data.frame(x)
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('PLvCut.R')
  
  wPLvCutAutoResults<-matrix(,nrow=length(x),ncol=2)
  wPLvCutAutoResults<-data.frame(wPLvCutAutoResults)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Power law v. PL w cutoff per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      wPLvCutAutoResults[i,] <- PLvCut( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with power law v. PL w cutoff")) }
    )
    print(paste("Power law v. PL w cutoff:","Sample",i,"completed on",Sys.time() ))
    utils::setTxtProgressBar(pb, i)
    #flush.console() 
  }
  close(pb)
  
  #colnames(wPLvCutAutoResults) <- c("wPLvCut.rawLR","wPLvCut.p")
  colnames(wPLvCutAutoResults) <- c("PLvCut.rawLR","PLvCut.p")
  return(wPLvCutAutoResults)
  
} #End wPLvCutAuto function
