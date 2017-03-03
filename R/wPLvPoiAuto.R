#<wPLvPoiAuto: automate PL v. Poisson per sample>

wPLvPoiAuto <- function(x){
  
  x<-data.frame(x)
  nomiss <- function(x) { x[ !is.na(x) ] } 
  #source('plfit.r')
  #source('PLvPoi.R')
  
  wPLvPoiAutoResults<-matrix(,nrow=length(x),ncol=3)
  wPLvPoiAutoResults<-data.frame(wPLvPoiAutoResults)
  
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  print("<<Power law v. Poisson per sample initiated>>")
  for (i in 1:length(x)) {
    tryCatch(
      wPLvPoiAutoResults[i,] <- PLvPoi( nomiss(x[[i]])[nomiss(x[[i]])>0], min(nomiss(x[[i]])[nomiss(x[[i]])>0]) ),
      error=function(e) { print(paste("Sample",i,": Problem with power law v. Poisson")) }
    )
    print(paste("Power law v. Poisson:","Sample",i,"completed on",Sys.time() ))
    utils::setTxtProgressBar(pb, i)
    #flush.console() 
  }
  close(pb)
  
  #colnames(wPLvPoiAutoResults) <- c("wPLvPoi.rawLR","wPLvPoi.normLR","wPLvPoi.p")
  colnames(wPLvPoiAutoResults) <- c("PLvPoi.rawLR","PLvPoi.normLR","PLvPoi.p")
  return(wPLvPoiAutoResults)
  
} #End wPLvPoiAuto function
