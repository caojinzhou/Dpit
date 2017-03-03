Dpit <- function(x){ 
  
  #source('descriptives.R')
  #source('plfitAuto.R')
  #source('plpvaAuto.R')
  #source('PLvALT.R')
  #source('wPLvALT.R')
  #source('ALTvALT.R')
  #source('NvALT.R')
  
  x<-data.frame(x)
  
  #print("Calculating descriptive statistics")
  ##descriptivesResults<-descriptives(x)
  #print("Done with calculating descriptive statistics")
  #plfit_results <- plfitAuto(x)
  #plpva_results <- plpvaAuto(x)
  #PLvALT_results <- PLvALT(x)
  wPLvALT_results <- wPLvALT(x) 
  ALTvALT_results<-ALTvALT(x)
  NvALT_results<-NvALT(x)
  
#Combine all results in one matrix
  #Dpit_results<-cbind(descriptivesResults,plfit_results,plpva_results,PLvALT_results,wPLvALT_results,ALTvALT_results,NvALT_results)
  Dpit_results<-cbind(NvALT_results,wPLvALT_results,ALTvALT_results)

 return(Dpit_results)
} #end Dpit function