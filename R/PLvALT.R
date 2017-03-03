PLvALT <- function(x){ 
  
  #source('PLvCutAuto.R')
  #source('PLvWeibAuto.R')
  #source('PLvLgNAuto.R')
  #source('PLvExpAuto.R')
  #source('PLvPoiAuto.R')
  
  x<-data.frame(x)
  
  PLvCut_results <- PLvCutAuto(x)
  PLvWeib_results<- PLvWeibAuto(x)
  PLvLgN_results <- PLvLgNAuto(x)
  PLvExp_results <- PLvExpAuto(x)
  PLvPoi_results <- PLvPoiAuto(x)
  
  #Combine all results in one matrix
  PLvALT_results<-cbind(PLvCut_results,PLvWeib_results,PLvLgN_results,PLvExp_results,PLvPoi_results)
  
  return(PLvALT_results)
} #end PLvALT function